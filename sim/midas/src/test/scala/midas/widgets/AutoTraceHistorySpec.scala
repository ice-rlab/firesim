package midas.widgets

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.sys.process._

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.{CircuitState, LowForm, Parser, RenameMap}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firesim.lib.bridgeutils.BridgeIOAnnotation
import midas.InternalAutoTraceFirrtlAnnotation
import midas.passes.AutoTraceTransform
import midas.targetutils.{AutoTraceField, AutoTraceFirrtlAnnotation, AutoTraceSchema}
import org.scalatest.flatspec.AnyFlatSpec

class AutoTraceHistorySpec extends AnyFlatSpec {
  private val schema = AutoTraceSchema(Seq(AutoTraceField("pc", 8, 0, false)))
  private val mt = ModuleTarget("Top", "Top")
  private def annotation = AutoTraceFirrtlAnnotation(mt.ref("pc"), mt.ref("trigger"),
    mt.ref("clock"), mt.ref("reset"), "pcs", "prior PCs", schema, 32, Some(mt.ref("control")))

  "AutoTrace history" should "rename and wire its controls and preserve history metadata" in {
    val renames = RenameMap()
    renames.record(mt.ref("control"), mt.ref("renamedControl"))
    assert(annotation.update(renames).head.asInstanceOf[AutoTraceFirrtlAnnotation]
      .historyControl.contains(mt.ref("renamedControl")))
    assert(InternalAutoTraceFirrtlAnnotation(annotation).update(renames).head
      .asInstanceOf[InternalAutoTraceFirrtlAnnotation].historyControl.contains(mt.ref("renamedControl")))
    val circuit = Parser.parse("""circuit Top :
      |  module Top :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input pc : UInt<8>
      |    input trigger : UInt<1>
      |    input control : UInt<2>
      |""".stripMargin)
    val state = new AutoTraceTransform().doTransform(CircuitState(circuit, LowForm, Seq(annotation)), 2, 16)
    val bridge = state.annotations.collectFirst { case a: BridgeIOAnnotation => a }.get
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(Seq(bridge))).head == bridge)
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(Seq(annotation))).head == annotation)
    val key = bridge.widgetConstructorKey.get.asInstanceOf[AutoTraceParameters]
    val m = key.traceMetadata.head
    assert(m.historyDepth == 32)
    assert(m.historyControlPortName.nonEmpty)
    assert(m.recordSchema.width == 6 + 32 * 8)
    assert(m.recordSchema.fields.head == AutoTraceField("count", 6, 0, false))
    assert(m.recordSchema.fields.last == AutoTraceField("samples[31].pc", 8, 254, false))
    val plain = m.copy(historyDepth = 0, historyControlPortName = None)
    assert(plain.recordSchema == schema)
  }

  it should "emit the history annotation through the public API without target registers" in {
    class Target extends Module {
      val pc = IO(Input(UInt(8.W)))
      val valid = IO(Input(Bool()))
      val miss = IO(Input(Bool()))
      midas.targetutils.AutoTrace.history(pc, valid, miss, 32, "pcs")
    }
    val dir = Files.createTempDirectory("autotrace-api")
    val annos = (new ChiselStage).execute(Array("--target-dir", dir.toString),
      Seq(ChiselGeneratorAnnotation(() => new Target)))
    val a = annos.collectFirst { case a: AutoTraceFirrtlAnnotation => a }.get
    assert(a.historyDepth == 32 && a.historyControl.nonEmpty)
    assert(a.schema.width == 8)
    val verilog = new String(Files.readAllBytes(dir.resolve("Target.v")), StandardCharsets.UTF_8)
    assert(!verilog.contains("always @"))
  }

  it should "elaborate depth one and 32" in {
    Seq(1, 32).foreach { depth =>
      val dir = Files.createTempDirectory("autotrace-elaboration")
      (new ChiselStage).emitVerilog(new AutoTraceHistory(8, depth), Array("--target-dir", dir.toString))
    }
  }

  it should "retain sparse samples, roll over, ignore stalls, and clear on accepted reset" in {
    if (Process(Seq("sh", "-c", "command -v verilator")).! != 0) cancel("Verilator is unavailable")
    val dir = Files.createTempDirectory("autotrace-history")
    (new ChiselStage).emitVerilog(new AutoTraceHistory(8, 3), Array("--target-dir", dir.toString))
    val cpp = """#include "VAutoTraceHistory.h"
      |#include <cassert>
      |#include <cstdint>
      |int main() {
      |  VAutoTraceHistory d;
      |  auto tick = [&]() { d.clock=0; d.eval(); d.clock=1; d.eval(); d.clock=0; d.eval(); };
      |  auto check = [&](unsigned count, unsigned a=0, unsigned b=0, unsigned c=0) {
      |    d.eval(); assert(d.io_snapshot == (count | (a<<2) | (b<<10) | (c<<18)));
      |  };
      |  d.reset=1; d.io_advance=0; d.io_clear=0; d.io_sample=0; d.io_target=0; tick();
      |  d.reset=0; check(0);
      |  d.io_advance=1; d.io_sample=1; d.io_target=11;
      |  check(0); tick(); check(1,11); // trigger-cycle input is absent before tick
      |  d.io_sample=0; d.io_target=99; tick(); check(1,11);
      |  d.io_sample=1; d.io_target=22; tick(); check(2,11,22);
      |  d.io_advance=0; d.io_target=99; tick(); tick(); check(2,11,22);
      |  d.io_advance=1; d.io_target=33; tick(); check(3,11,22,33);
      |  d.io_target=44; tick(); check(3,22,33,44);
      |  d.io_clear=1; d.io_advance=0; tick(); check(3,22,33,44);
      |  d.io_advance=1; tick(); check(0);
      |  d.io_clear=0; d.io_target=55; tick(); check(1,55);
      |}
      |""".stripMargin
    Files.write(dir.resolve("test.cpp"), cpp.getBytes(StandardCharsets.UTF_8))
    assert(Process(Seq("verilator", "--cc", "--exe", "-Wno-fatal", "--top-module",
      "AutoTraceHistory", "AutoTraceHistory.v", "test.cpp"), dir.toFile).! == 0)
    assert(Process(Seq("make", "-C", "obj_dir", "-f", "VAutoTraceHistory.mk"), dir.toFile).! == 0)
    assert(Process(Seq(dir.resolve("obj_dir/VAutoTraceHistory").toString), dir.toFile).! == 0)
  }
}
