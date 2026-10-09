// See LICENSE for license details.

package midas
package widgets

import chisel3._
import chisel3.util._

import firrtl.annotations.HasSerializationHints
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.DecoupledHelper

import firesim.lib.bridgeutils._
import midas.targetutils.{AutoTraceField, AutoTraceSchema}

/** Identifies an instantiated observation and the scalar ports wired out by AutoTraceTransform. */
case class AutoTraceMetadata(
  targetPortName: String,
  enablePortName: String,
  traceId:       Int,
  instancePath:  String,
  label:         String,
  description:   String,
  schema:        AutoTraceSchema,
)

object AutoTraceConsts {
  val captureBufferDepth = 16
  val streamQueueDepth   = 6144
}

case class AutoTraceParameters(
  traceMetadata:      Seq[AutoTraceMetadata],
  triggerName:        String,
  resetPortName:      String,
  captureBufferDepth: Int = AutoTraceConsts.captureBufferDepth,
  streamQueueDepth:   Int = AutoTraceConsts.streamQueueDepth,
) extends HasSerializationHints {
  require(traceMetadata.nonEmpty, "AutoTrace bridges require at least one observation")
  require(captureBufferDepth > 0 && streamQueueDepth > 0, "AutoTrace queue depths must be positive")
  require(traceMetadata.forall(_.traceId >= 0), "AutoTrace IDs must be nonnegative")
  require(traceMetadata.map(_.traceId).distinct.size == traceMetadata.size, "AutoTrace IDs must be unique")
  private val portNames = Seq(triggerName, resetPortName) ++
    traceMetadata.flatMap(m => Seq(m.targetPortName, m.enablePortName))
  require(portNames.distinct.size == portNames.size, "AutoTrace bridge port names must be unique")

  def typeHints: Seq[Class[_]] = Seq(classOf[AutoTraceMetadata], classOf[AutoTraceSchema], classOf[AutoTraceField])
}

/** HostPort consumes all of these scalar channels together, as in AutoCounterBundle. */
class AutoTraceBundle(key: AutoTraceParameters) extends Record {
  val triggerEnable    = Input(Bool())
  val underGlobalReset = Input(Bool())
  val targets          = key.traceMetadata.map(m => m.targetPortName -> Input(UInt(m.schema.width.W)))
  val enables          = key.traceMetadata.map(m => m.enablePortName -> Input(Bool()))
  val elements         = collection.immutable.ListMap(
    ((key.triggerName, triggerEnable) +: (key.resetPortName, underGlobalReset) +: (targets ++ enables)): _*
  )
}

/** Stream records are aligned to 512-bit beats and sent low bits first.
  * Bits [31:0] hold the trace ID, [63:32] the target width, [127:64] the local cycle,
  * and [128 + width - 1:128] the target. The final beat is padded with zeros.
  * Every enabled observation gets its own record, including simultaneous observations.
  */
object AutoTraceRecord {
  val formatVersion = 1
  val headerWidth   = 128

  def beats(targetWidth: Int): Int =
    ((targetWidth.toLong + headerWidth + BridgeStreamConstants.streamWidthBits - 1) /
      BridgeStreamConstants.streamWidthBits).toInt
}

/** Captures complete target cycles before serializing observations onto FireSim's host stream.
  * Queue pressure stalls target tokens; timestamps advance only when a target token is accepted.
  * No retirement or squash condition is added to the caller's enable.
  */
class AutoTraceBridgeModule(key: AutoTraceParameters)(implicit p: Parameters)
    extends BridgeModule[HostPortIO[AutoTraceBundle]]()(p)
    with StreamToHostCPU {
  def toHostCPUQueueDepth: Int = key.streamQueueDepth

  lazy val module = new BridgeModuleImp(this) {
    val io    = IO(new WidgetIO())
    val hPort = IO(HostPort(new AutoTraceBundle(key)))

    private val metadata     = key.traceMetadata
    private val numTraces    = metadata.size
    private val targetWidth  = metadata.foldLeft(0)((width, m) => Math.addExact(width, m.schema.width))
    private val streamWidth  = BridgeStreamConstants.streamWidthBits
    private val maxBeats     = metadata.map(m => AutoTraceRecord.beats(m.schema.width)).max
    private val recordWidth  = Math.multiplyExact(maxBeats, streamWidth)
    private val beatIdxWidth = math.max(1, log2Ceil(maxBeats))

    val initDone    = genWORegInit(Wire(Bool()), "init_done", false.B)
    val traceEnable = genWORegInit(Wire(Bool()), "trace_enable", true.B)
    val cycles      = RegInit(0.U(64.W))
    val enabled     = VecInit(hPort.hBits.enables.map(_._2)).asUInt
    val capture     = enabled.orR && traceEnable && hPort.hBits.triggerEnable && !hPort.hBits.underGlobalReset

    // One entry holds all observations from a target cycle. Enqueue atomically so
    // simultaneous events can never be lost by selecting only one source at capture.
    val captures = Module(new Queue(new Bundle {
      val targets = UInt(targetWidth.W)
      val enables = UInt(numTraces.W)
      val cycle   = UInt(64.W)
    }, key.captureBufferDepth))

    val canAdvance = !capture || captures.io.enq.ready
    val fireHelper = DecoupledHelper(hPort.toHost.hValid, hPort.fromHost.hReady, initDone, canAdvance)
    val targetFire = fireHelper.fire()
    hPort.toHost.hReady          := fireHelper.fire(hPort.toHost.hValid)
    hPort.fromHost.hValid        := true.B // Observation-only bridge: no return data channels.
    captures.io.enq.valid        := fireHelper.fire(canAdvance) && capture
    captures.io.enq.bits.targets := Cat(hPort.hBits.targets.reverse.map(_._2))
    captures.io.enq.bits.enables := enabled
    captures.io.enq.bits.cycle   := cycles
    when(targetFire) { cycles := cycles + 1.U }

    // Hold one batch independently of target advancement and stream backpressure.
    val active       = RegInit(false.B)
    val savedTargets = Reg(UInt(targetWidth.W))
    val savedCycle   = Reg(UInt(64.W))
    val pending      = RegInit(0.U(numTraces.W))
    val beatIndex    = RegInit(0.U(beatIdxWidth.W))
    captures.io.deq.ready := !active
    when(captures.io.deq.fire) {
      active       := true.B
      savedTargets := captures.io.deq.bits.targets
      savedCycle   := captures.io.deq.bits.cycle
      pending      := captures.io.deq.bits.enables
      beatIndex    := 0.U
    }

    // Emit records in metadata order; every record in this batch has the same timestamp.
    val selected = if (numTraces == 1) 0.U else PriorityEncoder(pending)
    val offsets  = metadata.scanLeft(0)((offset, m) => Math.addExact(offset, m.schema.width))
    val records  = metadata.zipWithIndex.map { case (m, i) =>
      val target = savedTargets(offsets(i + 1) - 1, offsets(i))
      Cat(target, savedCycle, m.schema.width.U(32.W), m.traceId.U(32.W)).pad(recordWidth)
    }
    val record   = VecInit(records)(selected)
    val beats    = VecInit((0 until maxBeats).map(i => record((i + 1) * streamWidth - 1, i * streamWidth)))
    val lastBeat = VecInit(metadata.map(m => (AutoTraceRecord.beats(m.schema.width) - 1).U(beatIdxWidth.W)))(selected)
    streamEnq.valid := active
    streamEnq.bits  := beats(beatIndex)
    when(streamEnq.fire) {
      when(beatIndex === lastBeat) {
        val remaining = pending & ~UIntToOH(selected, numTraces)
        pending   := remaining
        beatIndex := 0.U
        when(!remaining.orR) { active := false.B }
      }.otherwise {
        beatIndex := beatIndex + 1.U
      }
    }

    // Stop capture with trace_enable, then poll idle before flushing/draining the stream.
    // idle covers this bridge's storage; beats already in the stream engine still need draining.
    attach(!active && !captures.io.deq.valid, "idle", ReadOnly)

    override def genHeader(base: BigInt, memoryRegions: Map[String, BigInt], sb: StringBuilder): Unit = {
      genConstructor(
        base,
        sb,
        "autotrace_t",
        "autotrace",
        Seq(
          UInt32(toHostStreamIdx),
          UInt32(toHostCPUQueueDepth),
          StdVector("AutoTraceMetadata", metadata.map { m =>
            CppStruct("AutoTraceMetadata", Seq(
              "trace_id"      -> UInt32(m.traceId),
              "instance_path" -> CStrLit(m.instancePath),
              "label"         -> CStrLit(m.label),
              "description"   -> CStrLit(m.description),
              "target_width"  -> UInt32(m.schema.width),
              "fields"        -> StdVector("AutoTraceField", m.schema.fields.map { field =>
                CppStruct("AutoTraceField", Seq(
                  "path"      -> CStrLit(field.path),
                  "width"     -> UInt32(field.width),
                  "offset"    -> UInt32(field.offset),
                  "is_signed" -> CppBoolean(field.signed),
                ))
              }),
            ))
          }),
          Verbatim(clockDomainInfo.toC()),
        ),
        hasStreams = true,
      )
    }
    genCRFile()
  }
}
