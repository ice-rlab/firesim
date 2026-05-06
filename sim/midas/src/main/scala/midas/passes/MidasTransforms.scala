// See LICENSE for license details.

package midas
package passes

import midas.passes.partition._

import firrtl._

private[midas] class MidasTransforms extends Transform {
  def inputForm  = LowForm
  def outputForm = HighForm

  def execute(state: CircuitState) = {
    println("Starting MidasTransforms")

    // First convert all external annotations to internal ones
    val newAnnos      = midas.ConvertExternalToInternalAnnotations(state.annotations)
    val internalState = state.copy(annotations = newAnnos)

    var idx = 2
    def next(name: String) = {
      val s = f"${idx}%02d-$name"
      idx += 1
      s
    }

    // Optionally run if the GenerateMultiCycleRamModels parameter is set
    val p                        = internalState.annotations.collectFirst({ case midas.stage.phases.ConfigParametersAnnotation(p) => p }).get
    val optionalTargetTransforms =
      if (p(GenerateMultiCycleRamModels))
        Seq(new fame.LabelSRAMModels, new ResolveAndCheck, new EmitFirrtl(next("post-wrap-sram-models.fir")))
      else Seq()

    val partition = p(FireAxePartitionGlobalInfo).isDefined
    val extract   = p(FireAxePartitionIndex).isDefined

    val performExtractPass = if (partition && extract) {
      println("PerformExtractPass")
      Seq(
        new CheckCombPathLength,
        new WrapAndGroupModulesToPartition,
        new EmitFirrtl(next("post-wrap-and-group.fir")),
        new fame.EmitFAMEAnnotations(next("post-wrap-and-group.json")),
        new ResolveAndCheck,
        new CheckCombLogic,
        new fame.EmitFAMEAnnotations(next("post-check-comb.json")),
        new GenerateFireSimWrapper,
        new EmitFirrtl(next("post-gen-firesim-wrapper.fir")),
        new fame.EmitFAMEAnnotations(next("post-gen-firesim-wrapper.json")),
        new PrunedExtraModulesAndAddBridgeAnnos,
        new EmitFirrtl(next("post-prune-extra.fir")),
        new fame.EmitAllAnnotations(next("post-prune-extra.json")),
        new ResolveAndCheck,
        new ModifyTargetBoundaryForExtractPass,
        new EmitFirrtl(next("post-modify-boundary.fir")),
        new fame.EmitFAMEAnnotations(next("post-modify-boundary.json")),
        new ResolveAndCheck,
        new EmitFirrtl(next("post-modify-boundary-and-resolve.fir")),
      )
    } else {
      Seq()
    }

    val performRemovePass = if (partition && !extract) {
      println("PerformRemovePass")
      Seq(
        new CheckCombPathLength,
        new WrapAndGroupModulesToPartition,
        new EmitFirrtl(next("post-wrap-and-group.fir")),
        new fame.EmitFAMEAnnotations(next("post-wrap-and-group.json")),
        new ResolveAndCheck,
        new CheckCombLogic,
        new fame.EmitFAMEAnnotations(next("post-check-comb.json")),
        new GenerateCutBridgeInGroupedWrapper,
        new EmitFirrtl(next("post-gen-cutbridge.fir")),
        new fame.EmitFAMEAnnotations(next("post-gen-cutbridge.json")),
        new ResolveAndCheck,
        new ModifyTargetBoundaryForRemovePass,
        new EmitFirrtl(next("post-modify-boundary.fir")),
        new fame.EmitAllAnnotations(next("post-modify-boundary.json")),
        new PruneUnrelatedAnnoPass,
        new fame.EmitAllAnnotations(next("post-prune.json")),
        new ResolveAndCheck,
        new EmitFirrtl(next("post-modify-boundary-and-resolve.fir")),
      )
    } else {
      Seq()
    }

    val nocPartitionPass = if (p(FireAxeNoCPartitionPass)) {
      println("PerformNoCPass")
      Seq(
        new LowerStatePass,
        new ResolveAndCheck,
        new RemoveDirectWireConnectionPass,
        new ResolveAndCheck,
        new NoCConnectHartIdPass,
        new EmitFirrtl(next("post-hartid-connection.fir")),
        new ResolveAndCheck,
        new NoCPartitionRoutersPass,
        new EmitFirrtl(next("post-group-router.fir")),
        new fame.EmitFAMEAnnotations(next("post-group-router.json")),
        new ResolveAndCheck,
        new NoCReparentRouterGroupPass,
        new EmitFirrtl(next("post-reparent-router.fir")),
        new ResolveAndCheck,
        new NoCCollectModulesInPathAndRegroupPass,
        new EmitFirrtl(next("post-collect-and-reparent.fir")),
        new ResolveAndCheck,
        new DedupClockAndResetPass,
        new EmitFirrtl(next("post-dedup-clock-and-reset.fir")),
        new NoCConnectInterruptsPass,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-connect-interrupts-and-resolve.fir")),
      )
    } else {
      Seq()
    }

    val fireAxePasses = nocPartitionPass ++ performRemovePass ++ performExtractPass

    // HACK : Only perfrom dedup when the current pass is not a remove module pass.
    // This is only a temporary solution.
    // Dedup here in 'AQB form' after lowering instance bulk connects
    val optionalDedup = if (partition && !extract) Seq() else Seq(new midas.passes.EnableAndRunDedupOnce)

    val xforms = Seq(
      new ResolveAndCheck,
      HoistStopAndPrintfEnables,
      firrtl.passes.RemoveValidIf,
      new firrtl.transforms.ConstantPropagation,
      firrtl.passes.SplitExpressions,
      new EmitFirrtl(next("post-split-expressions.fir")),
      new fame.EmitFAMEAnnotations(next("post-split-expressions.json")),
      // SplitExpressions invalidates ResolveKinds which can lead to missed CSE opportunities since
      // identical expressions may have different Kinds
      firrtl.passes.ResolveKinds,
      firrtl.passes.CommonSubexpressionElimination,
      new firrtl.transforms.DeadCodeElimination,
      new firrtl.transforms.InferResets,
      firrtl.passes.CheckTypes,
      new HighFirrtlToMiddleFirrtl,
      new MiddleFirrtlToLowFirrtl,
      new EmitFirrtl(next("pre-partition.fir")),
      new fame.EmitFAMEAnnotations(next("pre-partition.json")),
      new fame.EmitAllAnnotations(next("pre-partition-all.json")),
    ) ++
      fireAxePasses ++
      Seq(
        new EmitFirrtl(next("post-partition.fir")),
        new fame.EmitFAMEAnnotations(next("post-partition.json")),
        new fame.EmitAllAnnotations(next("post-partition-all.json")),
        PlusArgsWiringTransform,
        new EmitFirrtl(next("post-plusargs-wiring.fir")),
        new fame.EmitFAMEAnnotations(next("post-plusargs-wiring.json")),
        CoerceAsyncToSyncReset,
        EnsureNoTargetIO,         // Simple checking pass
        new BridgeExtraction,     // Promote all the bridges to the top level / Add FAMEChannelConnectionAnnotation (which indicates the top level connections for FAMETop)
        new ResolveAndCheck,
        new EmitFirrtl(next("post-bridge-extraction.fir")),
        new fame.EmitFAMEAnnotations(next("post-bridge-extraction.json")),
        new HighFirrtlToMiddleFirrtl,
        new MiddleFirrtlToLowFirrtl,
        new AutoCounterTransform,
        new EmitFirrtl(next("post-autocounter.fir")),
        new fame.EmitFAMEAnnotations(next("post-autocounter.json")),
        new TraceDoctorTransform,
        new EmitFirrtl(next("post-tracedoctor.fir")),
        new fame.EmitFAMEAnnotations(next("post-tracedoctor.json")),
        new ResolveAndCheck,
        new AssertionSynthesis,
        new PrintSynthesis,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-debug-synthesis.fir")),
        new fame.EmitFAMEAnnotations(next("post-debug-synthesis.json")),
        // All trigger sources and sinks must exist in the target RTL before this pass runs
        // As its naming suggests, it wires up all the trigger sources
        TriggerWiring,
        new EmitFirrtl(next("post-trigger-wiring.fir")),
        new fame.EmitFAMEAnnotations(next("post-trigger-wiring.json")),
        GlobalResetConditionWiring,
        // We should consider moving these lower
        ChannelClockInfoAnalysis, // Adds annotations containing info about clocks for each channel
        UpdateBridgeClockInfo,    // Determines each bridges clock domain & adds annotations about it
        fame.WrapTop,             // Wrap FireSim with FAMETop
        fame.LabelMultiThreadedInstances,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-wrap-top.fir")),
        new fame.EmitAllAnnotations(next("post-wrap-top-all.json")),
      ) ++
      optionalTargetTransforms ++
      Seq(
        new EmitFirrtl(next("pre-extract-model.fir")),
        new fame.EmitAllAnnotations(next("pre-extract-model-all.json")),
        new fame.ExtractModel,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-extract-model.fir")),
        new fame.EmitAllAnnotations(next("post-extract-model-all.json")),
        new HighFirrtlToMiddleFirrtl,
        new MiddleFirrtlToLowFirrtl,
      ) ++
      optionalDedup ++
      Seq(
        fame.PromotePassthroughConnections,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-promote-passthrough.fir")),
        new fame.EmitFAMEAnnotations(next("post-promote-passthrough.json")),
        new fame.FAMEDefaults,
        new EmitFirrtl(next("post-fame-defaults.fir")),
        new fame.EmitFAMEAnnotations(next("post-fame-defaults.json")),
        fame.FindDefaultClocks,
        new fame.EmitFAMEAnnotations(next("post-find-default-clocks.json")),
        new fame.ChannelExcision,
        new fame.EmitFAMEAnnotations(next("post-channel-excision.json")),
        new EmitFirrtl(next("post-channel-excision.fir")),
        // We could delay adding FAMETransformAnnotations to all top modules to here (not used before this)
        new fame.InferModelPorts,
        new EmitFirrtl(next("post-infer-model-ports.fir")),
        new fame.EmitFAMEAnnotations(next("post-infer-model-ports.json")),
        new fame.FAMETransform,
        DefineAbstractClockGate,
        fame.AddRemainingFanoutAnnotations,
        new EmitFirrtl(next("post-fame-transform.fir")),
        new fame.EmitFAMEAnnotations(next("post-fame-transform.json")),
        new ResolveAndCheck,
        new EmitFirrtl(next("pre-fame5-transform.fir")),
        new fame.EmitFAMEAnnotations(next("pre-fame5-transform.json")),
        fame.MultiThreadFAME5Models,
        new EmitFirrtl(next("post-fame5-transform.fir")),
        new fame.EmitFAMEAnnotations(next("post-fame5-transform.json")),
        new ResolveAndCheck,
        new passes.InlineInstances,
        passes.ResolveKinds,
        new fame.EmitAndWrapRAMModels,
        new ResolveAndCheck,
        new EmitFirrtl(next("post-gen-sram-models.fir")),
        new fame.EmitFAMEAnnotations(next("post-gen-sram-models.json")),
        new SimulationMapping(internalState.circuit.main),
        xilinx.HostSpecialization,
        new ResolveAndCheck,
      )
    (xforms
          .foldLeft(internalState)) { (in, xform) =>
            val passName = xform.getClass.getName

            // Simple "size" metric: number of modules + total statements
            def circuitSize(c: ir.Circuit): (Int, Int) = {
              val numModules = c.modules.size
              val numStmts = c.modules.map {
                case m: ir.Module =>
                  def countStmt(s: ir.Statement): Int = s match {
                    case ir.Block(stmts) => stmts.map(countStmt).sum
                    case _               => 1
                  }
                  countStmt(m.body)
                case _ => 0
              }.sum
              (numModules, numStmts)
            }

            val (modsBefore, stmtsBefore) = circuitSize(in.circuit)
            println(
              s"[PASS START] $passName | modules=$modsBefore stmts=$stmtsBefore"
            )

            val start = System.nanoTime()
            val out = xform.runTransform(in)
            val end = System.nanoTime()

            val (modsAfter, stmtsAfter) = circuitSize(out.circuit)
            val timeMs = (end - start) / 1e6

            println(
              s"[PASS END]   $passName | modules=$modsAfter stmts=$stmtsAfter | Δstmts=${stmtsAfter - stmtsBefore} | time=${timeMs}ms"
            )

            out
          }
          .copy(form = outputForm)
  }
}