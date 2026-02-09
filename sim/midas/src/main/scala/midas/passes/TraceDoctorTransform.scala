// See LICENSE for license details.

package midas.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils.{one, zero, BoolType}
import firrtl.annotations._
import freechips.rocketchip.util.property
import midas.{InternalGlobalResetConditionSink}
import midas.widgets._
import midas.targetutils._
import midas.{InternalTraceDoctorFirrtlAnnotation, InternalTriggerSinkAnnotation}
import midas.passes.fame.{And, FAMEChannelConnectionAnnotation, Neq, WireChannel}
import firesim.lib.bridgeutils.BridgeIOAnnotation
import firechip.bridgeinterfaces.{TraceDoctorEventMetadata, TraceDoctorKey}
import firechip.goldengateimplementations.TraceDoctorBridgeModule

import java.io._
import collection.mutable

class TraceDoctorFireSimPropertyLibrary extends property.BasePropertyLibrary {
  import chisel3.internal.sourceinfo.SourceInfo
  import chisel3.experimental.{annotate, ChiselAnnotation}
  def generateProperty(prop_param: property.BasePropertyParameters)(implicit sourceInfo: SourceInfo): Unit = {
    //requireIsHardware(prop_param.cond, "condition covered for counter is not hardware!")
    if (!(prop_param.cond.isLit) && chisel3.experimental.DataMirror.internal.isSynthesizable(prop_param.cond)) {
      annotate(new ChiselAnnotation {
        val implicitClock = chisel3.Module.clock
        val implicitReset = chisel3.Module.reset
        def toFirrtl      = InternalTraceDoctorFirrtlAnnotation(
          prop_param.cond.toNamed,
          implicitClock.toNamed.toTarget,
          implicitReset.toNamed.toTarget,
          prop_param.label,
          prop_param.message,
          coverGenerated = true,
        )
      })
    }
  }
}
//freechips.rocketchip.util.property.cover.setPropLib(new midas.passes.TraceDoctorFireSimPropertyLibrary())
//=========================================================================

/** Take the annotated cover points and convert them to counters
  */
class TraceDoctorTransform extends Transform {
  def inputForm: CircuitForm  = LowForm
  def outputForm: CircuitForm = MidForm
  override def name           = "[Golden Gate] TraceDoctor Cover Transform"

  // Gates each auto-counter event with the associated reset, moving the
  // annotation's target to point at the new boolean (updatedAnnos).
  // This is used in both implementation strategies.
  private def gateEventsWithReset(
    coverTupleAnnoMap: Map[String, Seq[InternalTraceDoctorFirrtlAnnotation]],
    updatedAnnos:      mutable.ArrayBuffer[InternalTraceDoctorFirrtlAnnotation],
  )(mod:               DefModule
  ): DefModule = mod match {
    case m: Module if coverTupleAnnoMap.isDefinedAt(m.name) =>
      val coverAnnos = coverTupleAnnoMap(m.name)
      val mT         = coverAnnos.head.enclosingModuleTarget()
      val moduleNS   = Namespace(mod)
      val addedStmts = coverAnnos.flatMap({ anno =>
        val eventName = moduleNS.newName(anno.label)
        updatedAnnos += anno.copy(target = mT.ref(eventName))
        Seq(
          DefWire(NoInfo, eventName, UIntType(UnknownWidth)),
          Connect(NoInfo, WRef(eventName), Mux(WRef(anno.reset.ref), zero, WRef(anno.target.ref))),
        )
      })
      m.copy(body = Block(m.body, addedStmts: _*))
    case o                                                  => o
  }

  private def onModulePrintfImpl(
    coverTupleAnnoMap: Map[String, Seq[InternalTraceDoctorFirrtlAnnotation]],
    addedAnnos:        mutable.ArrayBuffer[Annotation],
  )(mod:               DefModule
  ): DefModule = mod match {
    case m: Module if coverTupleAnnoMap.isDefinedAt(m.name) =>
      val coverAnnos = coverTupleAnnoMap(m.name)
      val mT         = coverAnnos.head.enclosingModuleTarget()
      val moduleNS   = Namespace(mod)
      val addedStmts = new mutable.ArrayBuffer[Statement]

      val countType = UIntType(IntWidth(64))
      val zeroLit   = UIntLiteral(0, IntWidth(64))

      def generatePrintf(
        label:              String,
        clock:              ReferenceTarget,
        valueToPrint:       WRef,
        printEnable:        Expression,
        suggestedPrintName: String,
      ): Unit = {
        // Generate a trigger sink and annotate it
        val triggerName = moduleNS.newName("trigger")
        val trigger     = DefWire(NoInfo, triggerName, BoolType)
        addedStmts ++= Seq(trigger, Connect(NoInfo, WRef(trigger), one))
        addedAnnos += InternalTriggerSinkAnnotation(mT.ref(triggerName), clock)

        // Now emit a printf using all the generated hardware
        val printFormat = StringLit(s"""[TraceDoctor] $label: %d\n""")
        val printName   = moduleNS.newName(suggestedPrintName)
        val printStmt   =
          Print(NoInfo, printFormat, Seq(valueToPrint), WRef(clock.ref), And(WRef(trigger), printEnable), printName)
        addedAnnos += SynthPrintfAnnotation(mT.ref(printName))
        addedStmts += printStmt
      }

      coverAnnos.foreach {
        case InternalTraceDoctorFirrtlAnnotation(target, clock, reset, label, _, _) =>
          val countName   = moduleNS.newName(label + "_counter")
          val count       = DefRegister(NoInfo, countName, countType, WRef(clock.ref), WRef(reset.ref), zeroLit)
          val nextName    = moduleNS.newName(label + "_next")
          val next        =
            DefNode(NoInfo, nextName, DoPrim(PrimOps.Add, Seq(WRef(count), WRef(target.ref)), Seq.empty, countType))
          val countUpdate = Connect(NoInfo, WRef(count), WRef(next))
          addedStmts ++= Seq(count, next, countUpdate)

          def printEnable = Neq(WRef(target.ref), zero)
          generatePrintf(label, clock, WRef(count), printEnable, target.ref + "_print")
      }
      m.copy(body = Block(m.body, addedStmts.toSeq: _*))
    case o                                                  => o
  }

  private def implementViaPrintf(
    state:          CircuitState,
    eventModuleMap: Map[String, Seq[InternalTraceDoctorFirrtlAnnotation]],
  ): CircuitState = {

    val addedAnnos     = new mutable.ArrayBuffer[Annotation]()
    val updatedModules = state.circuit.modules.map(onModulePrintfImpl(eventModuleMap, addedAnnos))
    state.copy(circuit = state.circuit.copy(modules = updatedModules), annotations = state.annotations ++ addedAnnos)
  }

  private def implementViaBridge(
    state:          CircuitState,
    eventModuleMap: Map[String, Seq[InternalTraceDoctorFirrtlAnnotation]],
  ): CircuitState = {
    println(s"    [TraceDoctorTransform] invoking implementViaBridge with ${eventModuleMap.keySet.size} map entries")

    val sourceToTraceDoctorAnnoMap = eventModuleMap.values.flatten.map(anno => anno.target -> anno).toMap
    val bridgeTopWiringAnnos       =
      eventModuleMap.values.flatten.map(anno => BridgeTopWiringAnnotation(anno.target, anno.clock))

    // Step 1: Call BridgeTopWiring, grouping all events by their source clock
    val topWiringPrefix = "tracedoctor"
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
    // Needed to pass out the widths for each autocoutner event; sufficient to grab just uint ports
    val portWidthMap = topModule.ports.collect { case Port(_, name, _, UIntType(IntWidth(w))) =>
      name -> w.toInt
    }.toMap

    // MG TODO: only make one bridge annotation, not a separate bridge for each eventMetadata entry
    val bridgeAnnos = for ((srcClockRT, oAnnos) <- groupedOutputs.toSeq.sortBy(_._1.ref)) yield {
      val sinkClockRT = oAnnos.head.sinkClockPort
      val fccas       = oAnnos.map({ anno =>
        FAMEChannelConnectionAnnotation.source(anno.topSink.ref, WireChannel, Some(sinkClockRT), Seq(anno.topSink))
      })

      val eventMetadata = oAnnos.map({ anno =>
        val traceDoctorAnno = sourceToTraceDoctorAnnoMap(anno.pathlessSource)
        val pathlessLabel   = traceDoctorAnno.label
        val instPath        = anno.absoluteSource.circuit +: anno.absoluteSource.asPath.map(_._1.value)
        val eventWidth      = portWidthMap(anno.topSink.ref)
        TraceDoctorEventMetadata(
          anno.topSink.ref,
          //(instPath :+ pathlessLabel).mkString("_"),
          traceDoctorAnno.label,
          traceDoctorAnno.description,
          eventWidth,
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
        // We need to pass the name of the trigger port so each bridge can
        // disambiguate between them and connect to the correct one in simulation mapping
        widgetClass          = classOf[TraceDoctorBridgeModule].getName,
        widgetConstructorKey = TraceDoctorKey(512, eventMetadata, triggerPortName, globalResetName),
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

  def doTransform(state: CircuitState, usePrintfImplementation: Boolean): CircuitState = {
    val dir            = state.annotations.collectFirst({ case TargetDirAnnotation(dir) => dir }).get
    //select/filter which modules do we want to actually look at, and generate counters for
    //this can be done in one of two way:
    //1. Using an input file called `covermodules.txt` in a directory declared in the transform concstructor
    //2. Using chisel annotations to be added in the Platform Config (in SimConfigs.scala). The annotations are
    //   of the form TraceDoctorModuleAnnotation("ModuleName")
    val counterAnnos   = new mutable.ArrayBuffer[InternalTraceDoctorFirrtlAnnotation]()
    val remainingAnnos = new mutable.ArrayBuffer[Annotation]()
    println(
      s"[TraceDoctor] Scanning ${state.annotations.length} annotations"
    )
    state.annotations.foreach {
      case a: InternalTraceDoctorFirrtlAnnotation    => {
        counterAnnos += a
        println(
          s"[TraceDoctor] Found InternalTraceDoctorFirrtlAnnotation: ${classOf[InternalTraceDoctorFirrtlAnnotation].getName}"
        )
      }
      case a: TraceDoctorFirrtlAnnotation    => {
        counterAnnos += InternalTraceDoctorFirrtlAnnotation(a)
        println(
          s"[TraceDoctor] Found TraceDoctorFirrtlAnnotation: ${classOf[TraceDoctorFirrtlAnnotation].getName}"
        )
      }
      case o                                         => {
        remainingAnnos += o
        //println(
        //  s"[TraceDoctor] Annotation type is ${o.getClass.getName}"
        //)
      }
    }

    println(
      s"[TraceDoctor] There are ${counterAnnos.length} counterAnnos available for selection in the following modules:"
    )
    counterAnnos.map(_.target.module).distinct.foreach({ i => println(s"  ${i}") })

    //collect annotations for manually annotated TraceDoctor perf counters
    val filteredCounterAnnos = counterAnnos
    println(s"[TraceDoctor] selected ${filteredCounterAnnos.length} signals for instrumentation")
    filteredCounterAnnos.foreach({ i => println(s"  ${i}") })

    // group the selected signal by modules, and attach label from the cover point to each signal
    val selectedsignals = filteredCounterAnnos.groupBy(_.enclosingModule()).map { case (k, v) => k -> v.toSeq }

    if (!selectedsignals.isEmpty) {
      println("[TraceDoctor] signals are:")
      selectedsignals.foreach({ case (modName, localEvents) =>
        println(s"  Module ${modName}")
        localEvents.foreach({ anno => println(s"   ${anno.label}: ${anno.description}") })
      })

      // Common preprocessing: gate all annotated events with their associated reset
      val updatedAnnos   = new mutable.ArrayBuffer[InternalTraceDoctorFirrtlAnnotation]()
      val updatedModules = state.circuit.modules.map((gateEventsWithReset(selectedsignals, updatedAnnos)))
      val eventModuleMap = updatedAnnos.groupBy(_.enclosingModule()).map { case (k, v) => k -> v.toSeq }
      val gatedState     =
        state.copy(circuit = state.circuit.copy(modules = updatedModules), annotations = remainingAnnos.toSeq)

      val preppedState   = (new ResolveAndCheck).runTransform(gatedState)

      if (usePrintfImplementation) {
        implementViaPrintf(preppedState, eventModuleMap)
      } else {
        implementViaBridge(preppedState, eventModuleMap)
      }
    } else { state }
  }

  def execute(state: CircuitState): CircuitState = {
    val p                       = state.annotations.collectFirst({ case midas.stage.phases.ConfigParametersAnnotation(p) => p }).get
    val enableTransform         = true
    val usePrintfImplementation = false

    val updatedState = if (enableTransform) doTransform(state, usePrintfImplementation) else state
    // Clean up tracedoctor annotations so that their ReferenceTargets, which
    // are implicitly marked as DontTouch, can be optimized across
    updatedState.copy(annotations = updatedState.annotations.filter {
      case InternalTraceDoctorFirrtlAnnotation(_, _, _, _, _, _) => false
      case _                                                        => true
    })
  }
}
