// See LICENSE for license details.

package midas.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils.{one, zero, BoolType}
import firrtl.annotations._
import midas.{EnablePrefetchProf, InternalGlobalResetConditionSink}
import midas.widgets._
import midas.targetutils._
import midas.{InternalPrefetchProfFirrtlAnnotation, InternalTriggerSinkAnnotation}
import midas.passes.fame.{FAMEChannelConnectionAnnotation, WireChannel}
import firesim.lib.bridgeutils.BridgeIOAnnotation
import firechip.bridgeinterfaces.{PrefetchProfEventMetadata, PrefetchProfKey}
import firechip.goldengateimplementations.PrefetchProfBridgeModule

import collection.mutable

/** Takes the signals annotated with [[PrefetchProfTarget]], wires them to the top of the design and connects them to a
  * PrefetchProf bridge. Does nothing unless [[EnablePrefetchProf]] is set and at least one signal is annotated.
  */
class PrefetchProfTransform extends Transform {
  def inputForm: CircuitForm  = LowForm
  def outputForm: CircuitForm = MidForm
  override def name           = "[Golden Gate] PrefetchProf Transform"

  // Gates each event with the associated reset, moving the annotation's target to point at the new wire.
  private def gateEventsWithReset(
    annosByModule: Map[String, Seq[InternalPrefetchProfFirrtlAnnotation]],
    updatedAnnos:  mutable.ArrayBuffer[InternalPrefetchProfFirrtlAnnotation],
  )(mod:           DefModule
  ): DefModule = mod match {
    case m: Module if annosByModule.isDefinedAt(m.name) =>
      val annos      = annosByModule(m.name)
      val mT         = annos.head.enclosingModuleTarget()
      val moduleNS   = Namespace(mod)
      // A target that was optimized away would make the gating wire below refer to itself (a combinational loop).
      annos.foreach { anno =>
        require(
          moduleNS.contains(anno.target.ref),
          s"[PrefetchProf] annotated signal '${anno.label}' (${anno.target.serialize}) does not exist in the circuit; " +
            "it was probably optimized away before Golden Gate ran."
        )
      }
      val addedStmts = annos.flatMap({ anno =>
        val eventName = moduleNS.newName(anno.label)
        updatedAnnos += anno.copy(target = mT.ref(eventName))
        Seq(
          DefWire(NoInfo, eventName, UIntType(UnknownWidth)),
          Connect(NoInfo, WRef(eventName), Mux(WRef(anno.reset.ref), zero, WRef(anno.target.ref))),
        )
      })
      m.copy(body = Block(m.body, addedStmts: _*))
    case o                                              => o
  }

  private def implementViaBridge(
    state:         CircuitState,
    annosByModule: Map[String, Seq[InternalPrefetchProfFirrtlAnnotation]],
  ): CircuitState = {
    val sourceToAnnoMap      = annosByModule.values.flatten.map(anno => anno.target -> anno).toMap
    val bridgeTopWiringAnnos =
      annosByModule.values.flatten.map(anno => BridgeTopWiringAnnotation(anno.target, anno.clock))

    // Step 1: Call BridgeTopWiring, grouping all events by their source clock
    val topWiringPrefix = "prefetchprof"
    val wiredState      = (new BridgeTopWiring(topWiringPrefix + "_"))
      .execute(state.copy(annotations = state.annotations ++ bridgeTopWiringAnnos))
    val outputAnnos     = wiredState.annotations.collect({ case a: BridgeTopWiringOutputAnnotation => a })
    val groupedOutputs  = outputAnnos.groupBy(_.srcClockPort)

    // Step 2: For each group of wired events, generate associated bridge annotations
    val c            = wiredState.circuit
    val topModule    = c.modules.collectFirst({ case m: Module if m.name == c.main => m }).get
    val topMT        = ModuleTarget(c.main, c.main)
    val topNS        = Namespace(topModule)
    val addedPorts   = mutable.ArrayBuffer[Port]()
    val addedStmts   = mutable.ArrayBuffer[Statement]()
    // Needed to pass out the widths for each event; sufficient to grab just uint ports
    val portWidthMap = topModule.ports.collect { case Port(_, name, _, UIntType(IntWidth(w))) =>
      name -> w.toInt
    }.toMap

    val bridgeAnnos = for ((srcClockRT, oAnnos) <- groupedOutputs.toSeq.sortBy(_._1.ref)) yield {
      val sinkClockRT = oAnnos.head.sinkClockPort
      val fccas       = oAnnos.map({ anno =>
        FAMEChannelConnectionAnnotation.source(anno.topSink.ref, WireChannel, Some(sinkClockRT), Seq(anno.topSink))
      })

      // Names identify the signals inside the bridge, so they must be unique. If the same name is annotated in several
      // instances (e.g., one per core), qualify those with the instance path. Elements of a sequence share a name.
      def instPathOf(anno: BridgeTopWiringOutputAnnotation) =
        anno.absoluteSource.circuit +: anno.absoluteSource.asPath.map(_._1.value)
      def baseName(anno: BridgeTopWiringOutputAnnotation) = {
        val a = sourceToAnnoMap(anno.pathlessSource)
        a.vecGroup.getOrElse(a.label)
      }
      val duplicated = oAnnos.groupBy(baseName).filter(_._2.map(instPathOf).distinct.size > 1).keySet

      val eventMetadata = oAnnos.map({ anno =>
        val prefetchAnno = sourceToAnnoMap(anno.pathlessSource)
        val qualify      = duplicated(baseName(anno))
        def q(n: String) = if (qualify) (instPathOf(anno) :+ n).mkString("_") else n
        // Sequence elements are named "<group>_<index>"; scalar events keep their own label.
        val label        = prefetchAnno.vecGroup match {
          case Some(group) => s"${q(group)}_${prefetchAnno.vecIndex}"
          case None        => q(prefetchAnno.label)
        }
        PrefetchProfEventMetadata(
          anno.topSink.ref,
          label,
          prefetchAnno.description,
          portWidthMap(anno.topSink.ref),
          prefetchAnno.vecGroup.map(q),
          prefetchAnno.vecIndex,
          prefetchAnno.tag,
        )
      })

      // Step 2b. Manually add boolean channels, to carry the trigger and
      // globalResetCondition to the bridge
      val triggerPortName = topNS.newName(s"${topWiringPrefix}_triggerEnable")
      // Introduce an extra node until TriggerWriring supports port sinks
      val triggerPortNode = topNS.newName(s"${topWiringPrefix}_triggerEnable_node")
      addedPorts += Port(NoInfo, triggerPortName, Output, BoolType)

      val globalResetName = topNS.newName(s"${topWiringPrefix}_globalResetCondition")
      addedPorts += Port(NoInfo, globalResetName, Output, BoolType)

      // In the event there are no trigger sources, default to enabled
      addedStmts ++= Seq(
        DefNode(NoInfo, triggerPortNode, one),
        Connect(NoInfo, WRef(triggerPortName), WRef(triggerPortNode)),
        Connect(NoInfo, WRef(globalResetName), zero),
      )

      def predicateFCCA(name: String) =
        FAMEChannelConnectionAnnotation.source(name, WireChannel, Some(sinkClockRT), Seq(topMT.ref(name)))

      val triggerFCCA     = predicateFCCA(triggerPortName)
      val triggerSinkAnno = InternalTriggerSinkAnnotation(topMT.ref(triggerPortNode), sinkClockRT)
      val globalResetFCCA = predicateFCCA(globalResetName)
      val globalResetSink = InternalGlobalResetConditionSink(topMT.ref(globalResetName))

      val bridgeAnno = BridgeIOAnnotation(
        target               = topMT.ref(topWiringPrefix),
        widgetClass          = classOf[PrefetchProfBridgeModule].getName,
        widgetConstructorKey = PrefetchProfKey(eventMetadata, triggerPortName, globalResetName),
        channelNames         = (triggerFCCA +: globalResetFCCA +: fccas).map(_.globalName),
      )
      Seq(bridgeAnno, triggerSinkAnno, triggerFCCA, globalResetFCCA, globalResetSink) ++ fccas
    }

    val updatedCircuit = c.copy(modules = c.modules.map({
      case m: Module if m.name == c.main =>
        m.copy(ports = m.ports ++ addedPorts, body = Block(m.body, addedStmts.toSeq: _*))
      case o                             => o
    }))

    val cleanedAnnotations = wiredState.annotations.filterNot(outputAnnos.toSet)
    CircuitState(updatedCircuit, wiredState.form, cleanedAnnotations ++ bridgeAnnos.flatten)
  }

  def doTransform(state: CircuitState): CircuitState = {
    val eventAnnos     = new mutable.ArrayBuffer[InternalPrefetchProfFirrtlAnnotation]()
    val remainingAnnos = new mutable.ArrayBuffer[Annotation]()
    state.annotations.foreach {
      case a: InternalPrefetchProfFirrtlAnnotation => eventAnnos += a
      case a: PrefetchProfFirrtlAnnotation         => eventAnnos += InternalPrefetchProfFirrtlAnnotation(a)
      case a: PrefetchProfVecFirrtlAnnotation      => eventAnnos += InternalPrefetchProfFirrtlAnnotation(a)
      case o                                       => remainingAnnos += o
    }

    println(s"[PrefetchProf] selected ${eventAnnos.length} signals for instrumentation")
    if (eventAnnos.isEmpty) {
      state
    } else {
      val annosByModule = eventAnnos.groupBy(_.enclosingModule()).map { case (k, v) => k -> v.toSeq }
      annosByModule.foreach({ case (modName, localEvents) =>
        println(s"  Module ${modName}")
        localEvents.foreach({ anno => println(s"   ${anno.label}: ${anno.description}") })
      })

      // Gate all annotated events with their associated reset
      val updatedAnnos   = new mutable.ArrayBuffer[InternalPrefetchProfFirrtlAnnotation]()
      val updatedModules = state.circuit.modules.map(gateEventsWithReset(annosByModule, updatedAnnos))
      val gatedAnnoMap   = updatedAnnos.groupBy(_.enclosingModule()).map { case (k, v) => k -> v.toSeq }
      val gatedState     =
        state.copy(circuit = state.circuit.copy(modules = updatedModules), annotations = remainingAnnos.toSeq)

      implementViaBridge((new ResolveAndCheck).runTransform(gatedState), gatedAnnoMap)
    }
  }

  def execute(state: CircuitState): CircuitState = {
    val p = state.annotations.collectFirst({ case midas.stage.phases.ConfigParametersAnnotation(p) => p }).get

    val updatedState = if (p(EnablePrefetchProf)) doTransform(state) else state
    // Clean up PrefetchProf annotations so that their ReferenceTargets, which
    // are implicitly marked as DontTouch, can be optimized across
    updatedState.copy(annotations = updatedState.annotations.filter {
      case _: InternalPrefetchProfFirrtlAnnotation => false
      case _: PrefetchProfFirrtlAnnotation         => false
      case _: PrefetchProfVecFirrtlAnnotation      => false
      case _                                       => true
    })
  }
}
