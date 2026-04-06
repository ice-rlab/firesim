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
import scala.util.control._

/** Take the annotated cover points and convert them to counters
  */
class PowerTraceTransform extends Transform {
  def inputForm: CircuitForm  = LowForm
  def outputForm: CircuitForm = MidForm
  override def name           = "[Golden Gate] TraceDoctor Cover Transform"

  val max_power_annotation_bits = 448 // 512-64
  val pnr_top = "RocketTile"

// https://chipyard.readthedocs.io/en/stable/Tools/Barstools.html#separating-the-top-module-from-the-testharness-module
// https://chipyard.readthedocs.io/en/stable/Customization/Firrtl-Transforms.html
// https://github.com/chipsalliance/firrtl/tree/master-deprecated/src/main/scala/firrtl/transforms
  def execute(state: CircuitState): CircuitState = {
    val loop = new Breaks
    val loop2 = new Breaks

    val dir            = state.annotations.collectFirst({ case TargetDirAnnotation(dir) => dir }).get
    val powerFile      = new File("/home/mgrieco", "power.csv")
    //val powerModelFile = new File("/home/mgrieco", "power_model.csv")
    val powerAnnos     = new mutable.ArrayBuffer[TraceDoctorFirrtlAnnotation]()

    println(s"[PowerTraceTransform] Opening ${powerFile.getPath()} to find ${max_power_annotation_bits} most power-significant events")
    if (powerFile.exists()) {
      println("[PowerTraceTransform] Reading " + powerFile.getPath())
      val sourceFile        = scala.io.Source.fromFile(powerFile.getPath())
      //val destFile          = scala.io.Source.fromFile(powerModelFile.getPath())

      // power tracking
      var clock_power = 0.0
      var idle_power = 0.0
      val max_power = mutable.Seq.fill(max_power_annotation_bits)(0.0)
      val max_power_names = mutable.Seq.fill(max_power_annotation_bits)("")
      val max_power_refs = mutable.Seq.fill(max_power_annotation_bits)(ReferenceTarget( // target
              "state.circuit.main",
              "",
              Seq(),
              "",
              Seq()
            ))

      // get top module
      var topModule: DefModule = null
      loop.breakable {
        for (m <- state.circuit.modules) {
          if (m.name.equalsIgnoreCase(pnr_top)) {
            topModule = m
            println(s"Found top module with name ${m.name}")
            loop.break
          }
        }
      }

      // get largest contributors to power
      for (line <- sourceFile.getLines()) {
        val tokens = line.split(",")
        val name = tokens(0)
        val power = tokens(1).toDouble

        // split line into name and power
        if (name == "clock") {
          clock_power += power
        } else {
          var idx = 0
          loop.breakable {
            for (i <- 0 to max_power_annotation_bits-1) {
              if (power > max_power(i)) {
                loop.break
              }
              idx += 1
            }
          }

          println(s"[PowerTraceTransform] Adding signal with name ${name} and power ${power} mW at index ${idx} (originally ${tokens(1)})")

          if (idx == max_power_annotation_bits) {
            // power consumption did not pass the maximum ranking, so leave as "idle"
            idle_power += power
          } else {
            // find reference

            // strip array and register tags
            var stripped_name = name.split("""\[0\]/q$""")(0)
            stripped_name = stripped_name.split("""/q$""")(0)

            // split name into path and reference
            val s_path = stripped_name.split("/")
            val ref_name = s_path(s_path.size - 1)
            val path = new mutable.ArrayBuffer[(TargetToken.Instance, TargetToken.OfModule)]

            // find module-specific path
            // start with top
            var mod: DefModule = topModule
            loop.breakable { for (p <- s_path) {
              //if (p == ref_name) {
              //  loop.break
              //}

              var newMod: DefModule = null
              loop2.breakable {
                // skip through module block
                var block: Statement = null
                mod.foreachStmt({
                  case o => block = o
                })

                if (p == ref_name) {
                  // find register instance
                  block.foreachStmt({
                    case r: DefRegister => {
                      // found valid reference
                      //println(s"Register with name ${r.name}")
                      if (r.name.equals(p) || r.name.equals(s"${p}_")) {
                        loop.break
                      }
                    }
                    case o => { null }
                  })
                } else {
                  // iterate over all instances inside the module
                  block.foreachStmt({
                    case i: DefInstance => {
                      if (i.name.equals(p) || i.name.equals(s"${p}_")) {
                        var modName = i.module
                        println(s"Module for ${p} is ${modName}")
                        path += ((new TargetToken.Instance(i.name), new TargetToken.OfModule(modName)))
                        for (m <- state.circuit.modules) {
                          if (m.name.equalsIgnoreCase(modName)) {
                            newMod = m
                            loop2.break
                          }
                        }
                      }
                      null
                    }
                    case o => { null }
                  })
                }
              }
              mod = newMod
              if (mod == null) {
                println(s"Could not find module for path segment ${p} in signal ${name}")
                loop.break
              }
            }}

            if (mod == null) {
              // could not find reference, so leave as "idle"
              println(s"Could not find reference for signal ${name}")
              idle_power += power
            } else {
              println(s"Found reference for signal ${name}")
              // pop smallest power consumption
              idle_power += max_power(max_power_annotation_bits-1)
              for (j <- max_power_annotation_bits-1 to (idx+1) by -1) {
                max_power(j) = max_power(j-1)
                max_power_names(j) = max_power_names(j-1)
                max_power_refs(j) = max_power_refs(j-1)
              }
              max_power(idx) = power
              max_power_names(idx) = stripped_name

              println(s"Adding ReferenceTarget from ${state.circuit.main} to ${mod.name} > ${ref_name} with path:")
              for (p <- path.toSeq) {
                println(s"  (${p._1.value}, ${p._2.value})")
              }

              // ReferenceTarget(circuit: String, module: String, path: Seq[(Instance, OfModule)], ref: String, component: Seq[TargetToken])
              max_power_refs(idx) = ReferenceTarget( // target
                state.circuit.main,
                mod.name,
                path.toSeq,
                ref_name,
                Seq()
              )
            }
          }
        }
      }
      sourceFile.close()

      // reference objects
      val clock = ReferenceTarget(
        "FireSim",
        "Rocket",
        Seq(),
        "clock",
        Seq()
      )
      val reset = ReferenceTarget(
        "FireSim",
        "Rocket",
        Seq(),
        "reset",
        Seq()
      )

      // add TraceDoctorAnnotation objects to each of the largest contributors
      var num_power_map_entries = 0
      for (i <- 0 to max_power_annotation_bits-1) {
        if (max_power(i) > 0.0) {
          val target = max_power_refs(i)
          val label = max_power_names(i)
          val description = s"${max_power(i)}"

          println(s"[PowerTraceTransform] Adding TraceDoctorTransform to name ${label} with power ${description} mW")

          // write power coefficient for this signal
          num_power_map_entries += 1

          powerAnnos +=
            TraceDoctorFirrtlAnnotation(
              target, clock, reset,
              label, description, false,
              1, max_power(i),
            )
        }

        // write raw clock power
        // write idle power, scaled by 0.2 (default activity)
        // write coefficients for each significant signal
        // pass arguments to TraceDoctorBridge as float-based metadata, so map.csv describes bit locations and power
        //
        // println(s" #define N_POWER_MAP_ENTRIES ${num_power_map_entries}")
        // println(s" #define TRACE_SIZE 512")
        // println(s" typedef unsigned char[64] tracedoctor_word_t;")
        // println(s" #define SELECT_DATA_WORD_BIT_I(bit_i) ((bit_i >> 3) & 0b111111)")
        // println(s" #define BIT_SELECT(bit_i) (1 << (bit_i & 0b111))")
        // println(" power_map_t power_map[N_POWER_MAP_ENTRIES+2] = {")
        // println(" {"signal_name", signal_power_f},")
        // println(" {"clock", clock_power_f},")
        // println(" {"idle", idle_power_f}")
        // println(" }")

        //destFile.close()
      }
    } else {
      println("[PowerTraceTransform] Could not find power file: " + powerFile.getPath())
    }

    var extendedAnnotations = state.annotations ++ powerAnnos
    var extendedState = state.copy(annotations = extendedAnnotations.toSeq)
    (new ResolveAndCheck).runTransform(extendedState)
  }
}
