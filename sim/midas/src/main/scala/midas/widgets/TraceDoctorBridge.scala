// See LICENSE for license details.

package midas
package widgets

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._

import midas.widgets._
import firesim.lib.bridgeutils._

object TokenQueueConsts {
  val TOKENS_PER_BIG_TOKEN = 7
  val BIG_TOKEN_WIDTH = (TOKENS_PER_BIG_TOKEN + 1) * 64
  val TOKEN_QUEUE_DEPTH = 3072
}
import TokenQueueConsts._


import firechip.bridgeinterfaces._
case class TraceDoctorEventMetadata(
  portName:    String,
  label:       String,
  description: String,
  width:       Int,
)

object TraceDoctorEventMetadata {
  val localCycleCount =
    TraceDoctorEventMetadata("N/A", "local_cycle", "Clock cycles elapsed in the local domain.", 1)
}

case class TraceDoctorKey(traceWidth: Int, eventMetadata: Seq[TraceDoctorEventMetadata], triggerName: String, resetPortName: String)

class TraceDoctorTargetIO(
  val traceWidth: Int,
  eventMetadata: Seq[TraceDoctorEventMetadata],
  triggerName:   String,
  resetPortName: String,
) extends Record {
  val triggerEnable    = Input(Bool())
  val underGlobalReset = Input(Bool())
  val events           = eventMetadata.map(e => e.portName -> Input(UInt(e.width.W)))
  val elements         = collection.immutable.ListMap(
    ((triggerName, triggerEnable) +:
      (resetPortName, underGlobalReset) +:
      events): _*
  )
}

//case class TraceDoctorEventMetadata(
//  portName:    String,
//  label:       String,
//  description: String,
//  width:       Int,
//) extends AutoCounterConsts
//
//object TraceDoctorEventMetadata {
//  val localCycleCount =
//    TraceDoctorEventMetadata("N/A", "local_cycle", "Clock cycles elapsed in the local domain.", 1)
//}
//
//class TraceDoctorBundle(
//  eventMetadata: Seq[TraceDoctorEventMetadata],
//  triggerName:   String,
//  resetPortName: String,
//) extends Record {
//  val triggerEnable    = Input(Bool())
//  val underGlobalReset = Input(Bool())
//  val events           = eventMetadata.map(e => e.portName -> Input(UInt(e.width.W)))
//  val elements         = collection.immutable.ListMap(
//    ((triggerName, triggerEnable) +:
//      (resetPortName, underGlobalReset) +:
//      events): _*
//  )
//}
//
//case class TraceDoctorParameters(eventMetadata: Seq[EventMetadata], triggerName: String, resetPortName: String)

class TraceDoctorBridgeModule(key: TraceDoctorKey)(implicit p: Parameters)
    extends BridgeModule[HostPortIO[TraceDoctorTargetIO]]()(p)
    with StreamToHostCPU {

  val toHostCPUQueueDepth  = TokenQueueConsts.TOKEN_QUEUE_DEPTH
  println(s"TraceDoctorBridgeModule key has ${key.eventMetadata.length} metadata entries")

  lazy val module = new BridgeModuleImp(this) {
    val io = IO(new WidgetIO)
    val hPort = IO(HostPort(new TraceDoctorTargetIO(key.traceWidth, key.eventMetadata, key.triggerName, key.resetPortName)))
      println(s"TraceDoctorBridgeModule key has ${key.eventMetadata.length} metadata entries")

    val initDone = genWORegInit(Wire(Bool()), "initDone", false.B)
    val traceEnable = genWORegInit(Wire(Bool()), "traceEnable", false.B)

    // Trigger Selector
    val triggerSelector = RegInit(0.U((p(CtrlNastiKey).dataBits).W))
    attach(triggerSelector, "triggerSelector", WriteOnly)

    // Mask off ready samples when under reset
    val traceValid = hPort.hBits.triggerEnable && !hPort.hBits.underGlobalReset

    // Connect trigger
    val traceOut = initDone && traceEnable && traceValid

    // Width of the trace vector
    val traceWidth = key.traceWidth
    println(s"TraceDoctorBridge traceWidth is ${traceWidth}")
    // Width of one token as defined by the DMA
    val discreteDmaWidth = TokenQueueConsts.BIG_TOKEN_WIDTH
    // How many tokens we need to trace out the bit vector, at least one for DMA sanity
    val tokensPerTrace = math.max((traceWidth + discreteDmaWidth - 1) / discreteDmaWidth, 1)

    // Bridge DMA Parameters
    lazy val dmaSize = BigInt((discreteDmaWidth / 8) * TokenQueueConsts.TOKEN_QUEUE_DEPTH)

    // TODO: the commented out changes below show how multi-token transfers would work
    // However they show a bad performance for yet unknown reasons in terms of FPGA synth
    // timings -- verilator shows expected results
    // for now we limit us to 512 bits with an assert.

    assert(tokensPerTrace == 1)

    println( "TraceDoctorBridgeModule")
    println(s"    traceWidth      ${traceWidth}")
    println(s"    dmaTokenWidth   ${discreteDmaWidth}")
    println(s"    requiredTokens  {")
    for (i <- 0 until tokensPerTrace)  {
      val from = ((i + 1) * discreteDmaWidth) - 1
      val to   = i * discreteDmaWidth
      println(s"        ${i} -> traceBits(${from}, ${to})")
    }
    println( "    }")
    println( "")

    // concatenate bits from each event
    val eventBits = RegInit(0.U(512.W))
    val bits = Cat(for (((_, field), metadata) <- hPort.hBits.events.zip(key.eventMetadata)) yield {
      println(s"Event ${metadata.portName} ${metadata.label} ${metadata.description} ${metadata.width}")
      println(s"Field ${field}")
      field
    })
    eventBits := 0.U((traceWidth - bits.getWidth).W) ## bits

    // generate header description
    val fieldsSb = new StringBuilder()
    fieldsSb.append("{\n")
    var bit = 0
    for (((_, field), metadata) <- hPort.hBits.events.zip(key.eventMetadata)) {
      val nextBit = bit + metadata.width - 1
      fieldsSb.append(s"{${'\"'}${metadata.label}${'\"'}, ${bit}, ${nextBit}},\n")
      bit = nextBit + 1
    }
    fieldsSb.append(s"{${'\"'}null${'\"'},${traceWidth},${traceWidth}}\n")
    fieldsSb.append("}")

    // enqueue in DMA stream
    streamEnq.valid := hPort.toHost.hValid && traceOut
    //streamEnq.bits := trace.bits.asUInt.pad(discreteDmaWidth)
    streamEnq.bits := VecInit(eventBits)(0)

    hPort.toHost.hReady := initDone && streamEnq.ready
    hPort.fromHost.hValid := true.B

    genCRFile()
    override def genHeader(base: BigInt, memoryRegions: Map[String, BigInt], sb: StringBuilder): Unit = {
      genConstructor(
        base,
        sb,
        "tracedoctor_t",
        "tracedoctor",
        Seq(
          UInt32(toHostStreamIdx),
          UInt32(toHostCPUQueueDepth), // TokenQueueConsts.TOKEN_QUEUE_DEPTH
          UInt32(discreteDmaWidth),
          UInt32(traceWidth),
          Verbatim(clockDomainInfo.toC()),
          Verbatim(fieldsSb.result()),
        ),
        hasStreams = true,
      )
    }
  }
}
