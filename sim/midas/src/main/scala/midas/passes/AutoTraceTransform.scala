// See LICENSE for license details.

package midas.passes

import scala.collection.mutable

import firrtl._
import firrtl.Utils.{BoolType, one, zero}
import firrtl.annotations._
import firrtl.ir._

import firesim.lib.bridgeutils.BridgeIOAnnotation

import midas.{
  EnableAutoTrace,
  AutoTraceBufferDepth,
  AutoTraceStreamQueueDepth,
  InternalAutoTraceFirrtlAnnotation,
  InternalGlobalResetConditionSink,
  InternalTriggerSinkAnnotation,
}
import midas.passes.fame.{FAMEChannelConnectionAnnotation, WireChannel}
import midas.targetutils.AutoTraceFirrtlAnnotation
import midas.widgets.{AutoTraceBridgeModule, AutoTraceMetadata, AutoTraceParameters}

class AutoTraceTransform extends Transform {
  def inputForm: CircuitForm = LowForm
  def outputForm: CircuitForm = MidForm
  override def name = "[Golden Gate] AutoTrace Transform"

  private def gateEventsWithReset(
    eventModuleMap: Map[String, Seq[InternalAutoTraceFirrtlAnnotation]],
    updatedAnnos: mutable.ArrayBuffer[InternalAutoTraceFirrtlAnnotation],
  )(mod: DefModule): DefModule = mod match {
    case m: Module if eventModuleMap.contains(m.name) =>
      val moduleNS = Namespace(m)
      val addedStmts = eventModuleMap(m.name).flatMap { anno =>
        val enableName = moduleNS.newName(s"${anno.enable.ref}_notReset")
        updatedAnnos += anno.copy(enable = anno.enclosingModuleTarget().ref(enableName))
        Seq(
          DefWire(NoInfo, enableName, BoolType),
          Connect(NoInfo, WRef(enableName), Mux(WRef(anno.reset.ref), zero, WRef(anno.enable.ref))),
        )
      }
      m.copy(body = Block(m.body, addedStmts: _*))
    case other => other
  }

  def doTransform(state: CircuitState, captureBufferDepth: Int, streamQueueDepth: Int): CircuitState = {
    val traceAnnos = state.annotations.collect {
      case a: InternalAutoTraceFirrtlAnnotation => a
      case a: AutoTraceFirrtlAnnotation => InternalAutoTraceFirrtlAnnotation(a)
    }
    if (traceAnnos.isEmpty) return state
    traceAnnos.foreach { a =>
      val references = Seq(a.target, a.enable, a.clock, a.reset)
      require(references.forall(r => r.circuit == a.target.circuit && r.module == a.target.module &&
        r.path.isEmpty && r.component.isEmpty), "AutoTrace requires scalar references in the same enclosing module")
    }
    require(traceAnnos.map(_.target).distinct.size == traceAnnos.size,
      "Each AutoTrace annotation must have its own observation target")

    val remainingAnnos = state.annotations.filterNot {
      case _: InternalAutoTraceFirrtlAnnotation | _: AutoTraceFirrtlAnnotation => true
      case _ => false
    }
    val selectedSignals = traceAnnos.groupBy(_.enclosingModule())
    val updatedAnnos = mutable.ArrayBuffer[InternalAutoTraceFirrtlAnnotation]()
    val modules = state.circuit.modules.map(gateEventsWithReset(selectedSignals, updatedAnnos))
    require(updatedAnnos.size == traceAnnos.size, "AutoTrace annotation refers to a missing or external module")
    val gatedState = state.copy(circuit = state.circuit.copy(modules = modules), annotations = remainingAnnos)
    val preppedState = (new ResolveAndCheck).runTransform(gatedState)

    // Wire both signals with AutoCounter's BridgeTopWiring helper. Pair by absolute
    // instance path afterwards, never by label or traversal order.
    val wiringAnnos = updatedAnnos.flatMap(a => Seq(
      BridgeTopWiringAnnotation(a.target, a.clock), BridgeTopWiringAnnotation(a.enable, a.clock)))
    require(!preppedState.annotations.exists(_.isInstanceOf[BridgeTopWiringAnnotation]),
      "AutoTrace cannot consume another pass's pending bridge wiring annotations")
    val wiredState = (new BridgeTopWiring("autotrace_"))
      .execute(preppedState.copy(annotations = preppedState.annotations ++ wiringAnnos))
    val outputs = wiredState.annotations.collect { case a: BridgeTopWiringOutputAnnotation => a }
    val sourceMap = updatedAnnos.map(a => a.target -> a).toMap
    val outputMap = outputs.map(a => a.absoluteSource -> a).toMap
    val targetOutputs = outputs.filter(a => sourceMap.contains(a.pathlessSource))
      .sortBy(a => (a.srcClockPort.serialize, a.absoluteSource.serialize))
    val topModule = wiredState.circuit.modules.collectFirst {
      case m: Module if m.name == wiredState.circuit.main => m
    }.get
    val widths = topModule.ports.collect {
      case Port(_, name, _, UIntType(IntWidth(width))) => name -> width.toInt
    }.toMap
    val topNS = Namespace(topModule)
    wiredState.annotations.collect { case a: FAMEChannelConnectionAnnotation => a.globalName }
      .foreach(topNS.newName(_))
    val topMT      = ModuleTarget(wiredState.circuit.main, topModule.name)
    val addedPorts = mutable.ArrayBuffer[Port]()
    val addedStmts = mutable.ArrayBuffer[Statement]()

    // IDs are stable across instances and domains. Labels alone need not be unique.
    val groupedOutputs = targetOutputs.zipWithIndex.groupBy(_._1.srcClockPort)
    val bridgeAnnos = groupedOutputs.toSeq.sortBy(_._1.serialize).flatMap { case (_, observations) =>
      val sinkClockRT = observations.head._1.sinkClockPort
      require(observations.forall(_._1.sinkClockPort == sinkClockRT),
        "AutoTrace observations in one source domain must share a top-level clock")
      val fccas = mutable.ArrayBuffer[FAMEChannelConnectionAnnotation]()
      val channelMapping = mutable.LinkedHashMap[String, String]()

      // HostPort joins scalar channels into a single target-cycle token, like AutoCounter.
      def addChannel(target: ReferenceTarget): Unit = {
        val channelName = topNS.newName(s"${target.ref}_channel")
        fccas += FAMEChannelConnectionAnnotation.source(channelName, WireChannel, Some(sinkClockRT), Seq(target))
        channelMapping += target.ref -> channelName
      }

      val traceMetadata = observations.map { case (targetOutput, traceId) =>
        val anno = sourceMap(targetOutput.pathlessSource)
        val absoluteEnable = targetOutput.absoluteSource.copy(ref = anno.enable.ref)
        val enableOutput = outputMap.getOrElse(absoluteEnable,
          throw new IllegalArgumentException(s"Missing AutoTrace enable for ${targetOutput.absoluteSource}"))
        require(targetOutput.srcClockPort == enableOutput.srcClockPort &&
          targetOutput.sinkClockPort == enableOutput.sinkClockPort, "AutoTrace target and enable have different clocks")
        require(widths(targetOutput.topSink.ref) == anno.schema.width,
          "AutoTrace target width does not match its schema")
        require(widths(enableOutput.topSink.ref) == 1, "AutoTrace enable must be one bit")
        addChannel(targetOutput.topSink)
        addChannel(enableOutput.topSink)
        val instancePath = (targetOutput.absoluteSource.circuit +:
          targetOutput.absoluteSource.asPath.map(_._1.value)).mkString(".")
        AutoTraceMetadata(targetOutput.topSink.ref, enableOutput.topSink.ref, traceId,
          instancePath, anno.label, anno.description, anno.schema)
      }

      // Use the same trigger and global-reset wiring as AutoCounter. Without a trigger
      // source, capture is enabled. Local reset has already been applied per observation.
      val triggerPortName = topNS.newName("autotrace_triggerEnable")
      val triggerPortNode = topNS.newName("autotrace_triggerEnable_node")
      val globalResetName = topNS.newName("autotrace_globalResetCondition")
      addedPorts ++= Seq(Port(NoInfo, triggerPortName, Output, BoolType),
        Port(NoInfo, globalResetName, Output, BoolType))
      addedStmts ++= Seq(
        DefNode(NoInfo, triggerPortNode, one),
        Connect(NoInfo, WRef(triggerPortName), WRef(triggerPortNode)),
        Connect(NoInfo, WRef(globalResetName), zero),
      )
      addChannel(topMT.ref(triggerPortName))
      addChannel(topMT.ref(globalResetName))

      val bridgeAnno = BridgeIOAnnotation(
        target = topMT.ref(topNS.newName("autotrace")),
        channelMapping = channelMapping.toMap,
        widgetClass = classOf[AutoTraceBridgeModule].getName,
        widgetConstructorKey = Some(AutoTraceParameters(traceMetadata, triggerPortName, globalResetName,
          captureBufferDepth, streamQueueDepth)),
      )
      Seq(bridgeAnno, InternalTriggerSinkAnnotation(topMT.ref(triggerPortNode), sinkClockRT),
        InternalGlobalResetConditionSink(topMT.ref(globalResetName))) ++ fccas
    }
    val updatedCircuit = wiredState.circuit.copy(modules = wiredState.circuit.modules.map {
      case m: Module if m.name == topModule.name =>
        m.copy(ports = m.ports ++ addedPorts, body = Block(m.body, addedStmts.toSeq: _*))
      case other => other
    })
    println(s"[AutoTrace] created ${groupedOutputs.size} clock-domain bridges for ${targetOutputs.size} observations")
    wiredState.copy(circuit = updatedCircuit,
      annotations = wiredState.annotations.filterNot(outputs.toSet) ++ bridgeAnnos)
  }

  def execute(state: CircuitState): CircuitState = {
    val p = state.annotations.collectFirst {
      case midas.stage.phases.ConfigParametersAnnotation(p) => p
    }.get
    val updatedState = if (p(EnableAutoTrace))
      doTransform(state, p(AutoTraceBufferDepth), p(AutoTraceStreamQueueDepth)) else state
    updatedState.copy(annotations = updatedState.annotations.filterNot {
      case _: InternalAutoTraceFirrtlAnnotation | _: AutoTraceFirrtlAnnotation => true
      case _ => false
    })
  }
}
