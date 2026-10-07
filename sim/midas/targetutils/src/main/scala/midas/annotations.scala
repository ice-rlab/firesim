// See LICENSE for license details.

package midas.targetutils

import chisel3.{
  dontTouch,
  fromBooleanToLiteral,
  fromIntToLiteral,
  fromIntToWidth,
  when,
  Bits,
  Bool,
  Clock,
  Data,
  MemBase,
  Module,
  Printable,
  Record,
  RegNext,
  Reset,
  SInt,
  UInt,
  Vec,
  Wire,
  WireDefault,
}
import chisel3.printf.Printf
import chisel3.util.Cat
import chisel3.experimental.{annotate, requireIsHardware, BaseModule, ChiselAnnotation}
import firrtl.RenameMap
import firrtl.annotations.{
  Annotation,
  ComponentName,
  HasSerializationHints,
  InstanceTarget,
  ModuleTarget,
  ReferenceTarget,
  SingleTargetAnnotation,
}

/** These are consumed by [[midas.passes.AutoILATransform]] to directly instantiate an ILA at the top of simulator's
  * design hierarchy (the PlatformShim level).
  */
case class FpgaDebugAnnotation(target: Data) extends ChiselAnnotation {
  def toFirrtl = FirrtlFpgaDebugAnnotation(target.toNamed)
}

case class FirrtlFpgaDebugAnnotation(target: ComponentName) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName) = this.copy(target = n)
}

object FpgaDebug {
  def apply(targets: Data*): Unit = {
    targets.foreach { requireIsHardware(_, "Target passed to FpgaDebug:") }
    targets.map({ t => annotate(FpgaDebugAnnotation(t)) })
  }
}

private[midas] class ReferenceTargetRenamer(renames: RenameMap) {
  // TODO: determine order for multiple renames, or just check of == 1 rename?
  def exactRename(rt: ReferenceTarget): ReferenceTarget = {
    val renameMatches = renames.get(rt).getOrElse(Seq(rt)).collect({ case rt: ReferenceTarget => rt })
    assert(
      renameMatches.length <= 1,
      s"${rt} should be renamed exactly once (or not at all). Suggested renames: ${renameMatches}",
    )
    renameMatches.headOption.getOrElse(rt)
  }

  def apply(rt: ReferenceTarget): Seq[ReferenceTarget] = {
    renames.get(rt).getOrElse(Seq(rt)).collect({ case rt: ReferenceTarget => rt })
  }
}

private[midas] case class SynthPrintfAnnotation(
  target: ReferenceTarget
) extends firrtl.annotations.SingleTargetAnnotation[ReferenceTarget] {

  def duplicate(newTarget: ReferenceTarget) = this.copy(newTarget)
}

object SynthesizePrintf {

  /** Annotates a chisel printf as a candidate for synthesis. The printf is only synthesized if Printf synthesis is
    * enabled in Golden Gate.
    *
    * See: https://docs.fires.im/en/stable/search.html?q=Printf+Synthesis&check_keywords=yes&area=default
    *
    * @param printf
    *   The printf statement to be synthesized.
    *
    * @return
    *   The original input, so that this annotator may be applied inline if desired.
    */
  def apply(printf: Printf): Printf = {
    annotate(new ChiselAnnotation {
      def toFirrtl = SynthPrintfAnnotation(printf.toTarget)
    })
    printf
  }

  private def generateAnnotations(format: String, args: Seq[Bits], name: Option[String]): Printable = {
    Module.currentModule.getOrElse(throw new RuntimeException("Cannot annotate a printf outside of a Module"))

    // To preserve the behavior of the printf parameter annotator, generate a
    // secondary printf and annotate that, instead of the user's printf, which
    // will be given an empty string. This will be removed with the apply methods in 1.15.
    val printf = SynthesizePrintf(chisel3.printf(Printable.pack(format, args: _*)))
    name.foreach { n => printf.suggestName(n) }
    Printable.pack("")
  }

  /** Annotates* a printf by intercepting the parameters to a chisel printf, and returning a printable. As a side
    * effect, this function generates a ChiselSynthPrintfAnnotation with the format string and references to each of the
    * args.
    *
    * *Note: this isn't actually annotating the statement but instead the arguments. This is a vestige from earlier
    * versions of chisel / firrtl in which print statements were unnamed, and thus not referenceable from annotations.
    *
    * @param format
    *   The format string for the printf
    * @param args
    *   Hardware references to populate the format string.
    */

  @deprecated("This method will be removed. Annotate the printf statement directly", "FireSim 1.14")
  def apply(format: String, args: Bits*): Printable = generateAnnotations(format, args, None)

  /** Like the other apply method, but provides an optional name which can be used by synthesized hardware / bridge.
    * Generally, users deploy the nameless form.
    *
    * @param name
    *   A descriptive name for this printf instance.
    * @param format
    *   The format string for the printf
    * @param args
    *   Hardware references to populate the format string.
    */
  @deprecated("This method will be removed. Annotate the printf statement directly", "FireSim 1.14")
  def apply(name: String, format: String, args: Bits*): Printable =
    generateAnnotations(format, args, Some(name))
}

/** A mixed-in ancestor trait for all FAME annotations, useful for type-casing.
  */
trait FAMEAnnotation {
  this: Annotation =>
}

/** This labels an instance so that it is extracted as a separate FAME model.
  */
case class FAMEModelAnnotation(target: BaseModule) extends ChiselAnnotation {
  def toFirrtl: FirrtlFAMEModelAnnotation = {
    val parent = ModuleTarget(target.toNamed.circuit.name, target.parentModName)
    FirrtlFAMEModelAnnotation(parent.instOf(target.instanceName, target.name))
  }
}

case class FirrtlFAMEModelAnnotation(
  target: InstanceTarget
) extends SingleTargetAnnotation[InstanceTarget]
    with FAMEAnnotation {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

/** This specifies that the module should be automatically multi-threaded (Chisel annotator).
  */
case class EnableModelMultiThreadingAnnotation(target: BaseModule) extends ChiselAnnotation {
  def toFirrtl: FirrtlEnableModelMultiThreadingAnnotation = {
    val parent = ModuleTarget(target.toNamed.circuit.name, target.parentModName)
    FirrtlEnableModelMultiThreadingAnnotation(parent.instOf(target.instanceName, target.name))
  }
}

/** This specifies that the module should be automatically multi-threaded (FIRRTL annotation).
  */
case class FirrtlEnableModelMultiThreadingAnnotation(
  target: InstanceTarget
) extends SingleTargetAnnotation[InstanceTarget]
    with FAMEAnnotation {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

/** This labels a target Mem so that it is extracted and replaced with a separate model.
  */
case class MemModelAnnotation[T <: Data](target: MemBase[T]) extends ChiselAnnotation {
  def toFirrtl = FirrtlMemModelAnnotation(target.toNamed.toTarget)
}

case class FirrtlMemModelAnnotation(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(rt: ReferenceTarget) = this.copy(target = rt)
}

case class ExcludeInstanceAssertsAnnotation(target: (String, String)) extends firrtl.annotations.NoTargetAnnotation {
  def duplicate(n: (String, String)) = this.copy(target = n)
}
// TODO: Actually use a real target and not strings.
object ExcludeInstanceAsserts {
  def apply(target: (String, String)): ChiselAnnotation =
    new ChiselAnnotation {
      def toFirrtl = ExcludeInstanceAssertsAnnotation(target)
    }
}

/** TraceDoctor annotations. Do not emit the FIRRTL annotations unless you are writing a target transformation, use the
  * Chisel-side [[TraceDoctor]] object instead.
  */
case class TraceDoctorFirrtlAnnotation(
  target:         ReferenceTarget,
  clock:          ReferenceTarget,
  reset:          ReferenceTarget,
  label:          String,
  description:    String,
  coverGenerated: Boolean           = false,
) extends firrtl.annotations.Annotation
    //with HasSerializationHints
    {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedTarget = renamer.exactRename(target)
    val renamedClock  = renamer.exactRename(clock)
    val renamedReset  = renamer.exactRename(reset)
    Seq(this.copy(target = renamedTarget, clock = renamedClock, reset = renamedReset))
  }
  // The TraceDoctor tranform will reject this annotation if it's not enclosed
  def shouldBeIncluded(modList: Seq[String]): Boolean = !coverGenerated || modList.contains(target.module)
  def enclosingModule(): String             = target.module
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, enclosingModule())
  //def typeHints: Seq[Class[_]]              = Seq(opType.getClass)
}

object TraceDoctorTarget {
  private def emitAnnotation(
    target:      UInt,
    clock:       Clock,
    reset:       Reset,
    label:       String,
    description: String,
  ): Unit = {
    println(s"    TraceDoctorTarget.emitAnnotation ${label} ${description}")
    requireIsHardware(target, "Target passed to PerfCounter:")
    requireIsHardware(clock, "Clock passed to PerfCounter:")
    requireIsHardware(reset, "Reset passed to PerfCounter:")
    annotate(new ChiselAnnotation {
      def toFirrtl =
        TraceDoctorFirrtlAnnotation(target.toTarget, clock.toTarget, reset.toTarget, label, description)
    })
  }

  /** Labels a signal as an event for which an host-side tracer (an "TraceDoctorTarget") should be generated). Events can be
    * multi-bit to encode multiple occurances in a cycle (e.g., the number of instructions retired in a superscalar
    * processor). NB: Golden Gate will not generate the coutner unless TraceDoctor is enabled in your the platform
    * config. See the docs.fires.im for end-to-end usage information.
    *
    * @param target
    *   The number of occurrences of the event (in the current cycle)
    *
    * @param clock
    *   The clock to which this event is sychronized.
    *
    * @param reset
    *   If the event is asserted while under the provide reset, it is not counted. TODO: This should be made optional.
    *
    * @param label
    *   A verilog-friendly identifier for the event signal
    *
    * @param description
    *   A human-friendly description of the event.
    */
  def apply(
    target:      UInt,
    clock:       Clock,
    reset:       Reset,
    label:       String,
    description: String,
  ): Unit =
    emitAnnotation(target, clock, reset, label, description)

  /** A simplified variation of the full apply method above that uses the implicit clock and reset.
    */
  def apply(target: UInt, label: String, description: String): Unit = {
    println(s"    TraceDoctorTarget.apply ${label} ${description}")
    emitAnnotation(target, Module.clock, Module.reset, label, description)
  }
}

/** A field in the flattened target, with its original name, width, bit offset, and signedness. */
case class AutoTraceField(path: String, width: Int, offset: Int, signed: Boolean) {
  require(path.nonEmpty && width > 0 && offset >= 0, "Invalid AutoTrace field layout")
}

/** Preserves field names and offsets when a Bundle or Vec target is flattened into a UInt.
  * The host uses this schema to reconstruct the original fields.
  */
case class AutoTraceSchema(fields: Seq[AutoTraceField]) {
  require(fields.nonEmpty, "AutoTrace targets must not be empty")
  require(fields.map(_.path).distinct.size == fields.size, "AutoTrace field paths must be unique")
  private val totalWidth = fields.map(_.width.toLong).sum
  require(totalWidth <= Int.MaxValue, "AutoTrace target is too wide")
  val width: Int = totalWidth.toInt
  fields.foldLeft(0) { (offset, field) =>
    require(field.offset == offset, s"AutoTrace field ${field.path} must be packed contiguously")
    offset + field.width
  }
}

/** AutoTrace annotation. Use the Chisel-side AutoTrace API to annotate a target.
  * Each enabled cycle requests a record; the annotation imposes no retirement or squash filtering.
  */
case class AutoTraceFirrtlAnnotation(
  target:      ReferenceTarget,         // Flattened signal or Bundle to observe.
  enable:      ReferenceTarget,        // Capture condition or sampling pulse.
  clock:       ReferenceTarget,
  reset:       ReferenceTarget,
  label:       String,
  description: String,
  schema:      AutoTraceSchema,
) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedTarget = renamer.exactRename(target)
    val renamedEnable = renamer.exactRename(enable)
    val renamedClock  = renamer.exactRename(clock)
    val renamedReset  = renamer.exactRename(reset)
    Seq(this.copy(target = renamedTarget, enable = renamedEnable, clock = renamedClock, reset = renamedReset))
  }

  def enclosingModule(): String = target.module
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, enclosingModule())
}

/** Annotates hardware targets for full-record tracing, event occurrences, or periodic sampling.
  * Like PerfCounter, this API emits annotations for a compiler transform and host bridge.
  * Only combinational observation wires are added here; no target-side trace state is inserted.
  */
object AutoTrace {
  // a field in the unflattened target
  private case class TargetSignal(path: String, signal: Bits)
  
  private def flattenTarget(target: Data, path: String): Seq[TargetSignal] = target match {
    // Explicit packing order: alphabetical Record fields, numeric Vec indices.
    case record: Record => record.elements.toSeq.sortBy(_._1).flatMap { case (name, child) =>
      flattenTarget(child, if (path.isEmpty) name else s"$path.$name")
    }
    case vector: Vec[_] => vector.zipWithIndex.flatMap { case (child, index) =>
      flattenTarget(child, s"$path[$index]")
    }.toSeq
    case signal: UInt => Seq(TargetSignal(if (path.isEmpty) "value" else path, signal))
    case signal: SInt => Seq(TargetSignal(if (path.isEmpty) "value" else path, signal))
    case other => throw new IllegalArgumentException(
      s"AutoTrace target $path has unsupported type ${other.getClass.getSimpleName}; use Bool, UInt, SInt, Record, or Vec")
  }

  private def emitAnnotation(
    target: Data, enable: Bool, clock: Clock, reset: Reset,
    label: String, description: String, eventOnly: Boolean,
  ): Unit = {
    require(label.nonEmpty, "AutoTrace labels must not be empty")
    requireIsHardware(target, "Target passed to AutoTrace:")
    requireIsHardware(enable, "Enable passed to AutoTrace:")
    requireIsHardware(clock, "Clock passed to AutoTrace:")
    requireIsHardware(reset, "Reset passed to AutoTrace:")
    val targetSignals = flattenTarget(target, "")
    require(targetSignals.nonEmpty, "AutoTrace targets must not be empty")
    val fields = targetSignals.foldLeft(Vector.empty[AutoTraceField]) { (fields, targetSignal) =>
      require(targetSignal.signal.isWidthKnown && targetSignal.signal.getWidth > 0,
        s"AutoTrace field ${targetSignal.path} needs a positive explicit width before annotation")
      val offset = fields.lastOption.map(f => Math.addExact(f.offset, f.width)).getOrElse(0)
      fields :+ AutoTraceField(if (eventOnly) "occurred" else targetSignal.path,
        targetSignal.signal.getWidth, offset, targetSignal.signal.isInstanceOf[SInt])
    }
    val schema = AutoTraceSchema(fields)
    val packedTarget = Wire(UInt(schema.width.W)).suggestName("autotrace_target")
    // First schema field goes in the low bits. Native aggregate asUInt has a different order.
    packedTarget := Cat(targetSignals.reverse.map(_.signal.asUInt))
    val captureEnable = Wire(Bool()).suggestName("autotrace_enable")
    captureEnable := enable
    dontTouch(packedTarget)
    dontTouch(captureEnable)
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = AutoTraceFirrtlAnnotation(
        packedTarget.toTarget, captureEnable.toTarget, clock.toTarget, reset.toTarget,
        label, description, schema)
    })
  }

  /** Annotates a target for capture on every enabled cycle outside reset.
    *
    * @param target
    *   The signal, Bundle, or Vec whose fields should be recorded.
    * @param enable
    *   Capture this cycle when high; a high level across several cycles captures several records.
    * @param clock
    *   The clock to which the target and enable are synchronized.
    * @param reset
    *   Suppresses capture while asserted.
    * @param label
    *   A name identifying the annotated target.
    * @param description
    *   A human-readable description of the target.
    */
  def apply(
    target: Data, enable: Bool, clock: Clock, reset: Reset, label: String, description: String,
  ): Unit = emitAnnotation(target, enable, clock, reset, label, description, eventOnly = false)

  def apply(target: Data, enable: Bool, label: String, description: String): Unit =
    apply(target, enable, Module.clock, Module.reset, label, description)

  def apply(target: Data, enable: Bool, label: String): Unit = apply(target, enable, label, "")

  // Records the target every cycle outside reset. 
  def apply(target: Data, label: String, description: String = ""): Unit =
    apply(target, true.B, label, description)

  // Did the event occur this cycle?
  def event(enable: Bool, label: String, description: String = ""): Unit =
    emitAnnotation(1.U(1.W), enable, Module.clock, Module.reset, label, description, eventOnly = true)

  // Samples the signal at the rate mentioned in enable
  def sample(target: Data, enable: Bool, label: String, description: String = ""): Unit =
    apply(target, enable, label, description)
}

sealed trait PerfCounterOpType

object PerfCounterOps {

  /** Takes the annotated UInt and adds it to an accumulation register generated in the bridge
    */
  case object Accumulate extends PerfCounterOpType

  /** Takes the annotated UInt and exposes it directly to the driver NB: Fields longer than 64b are not supported, and
    * must be divided into smaller segments that are sepearate annotated
    */
  case object Identity extends PerfCounterOpType
}

/** AutoCounter annotations. Do not emit the FIRRTL annotations unless you are writing a target transformation, use the
  * Chisel-side [[PerfCounter]] object instead.
  */
case class AutoCounterFirrtlAnnotation(
  target:         ReferenceTarget,
  clock:          ReferenceTarget,
  reset:          ReferenceTarget,
  label:          String,
  description:    String,
  opType:         PerfCounterOpType = PerfCounterOps.Accumulate,
  coverGenerated: Boolean           = false,
) extends firrtl.annotations.Annotation
    with HasSerializationHints {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedTarget = renamer.exactRename(target)
    val renamedClock  = renamer.exactRename(clock)
    val renamedReset  = renamer.exactRename(reset)
    Seq(this.copy(target = renamedTarget, clock = renamedClock, reset = renamedReset))
  }
  // The AutoCounter tranform will reject this annotation if it's not enclosed
  def shouldBeIncluded(modList: Seq[String]): Boolean = !coverGenerated || modList.contains(target.module)
  def enclosingModule(): String             = target.module
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, enclosingModule())
  def typeHints: Seq[Class[_]]              = Seq(opType.getClass)
}

case class AutoCounterCoverModuleFirrtlAnnotation(target: ModuleTarget)
    extends SingleTargetAnnotation[ModuleTarget]
    with FAMEAnnotation {
  def duplicate(n: ModuleTarget) = this.copy(target = n)
}

case class AutoCounterCoverModuleAnnotation(target: ModuleTarget) extends ChiselAnnotation {
  def toFirrtl = AutoCounterCoverModuleFirrtlAnnotation(target)
}

object PerfCounter {
  private def emitAnnotation(
    target:      UInt,
    clock:       Clock,
    reset:       Reset,
    label:       String,
    description: String,
    opType:      PerfCounterOpType,
  ): Unit = {
    requireIsHardware(target, "Target passed to PerfCounter:")
    requireIsHardware(clock, "Clock passed to PerfCounter:")
    requireIsHardware(reset, "Reset passed to PerfCounter:")
    annotate(new ChiselAnnotation {
      def toFirrtl =
        AutoCounterFirrtlAnnotation(target.toTarget, clock.toTarget, reset.toTarget, label, description, opType)
    })
  }

  /** Labels a signal as an event for which an host-side counter (an "AutoCounter") should be generated). Events can be
    * multi-bit to encode multiple occurances in a cycle (e.g., the number of instructions retired in a superscalar
    * processor). NB: Golden Gate will not generate the coutner unless AutoCounter is enabled in your the platform
    * config. See the docs.fires.im for end-to-end usage information.
    *
    * @param target
    *   The number of occurances of the event (in the current cycle)
    *
    * @param clock
    *   The clock to which this event is sychronized.
    *
    * @param reset
    *   If the event is asserted while under the provide reset, it is not counted. TODO: This should be made optional.
    *
    * @param label
    *   A verilog-friendly identifier for the event signal
    *
    * @param description
    *   A human-friendly description of the event.
    *
    * @param opType
    *   Defines how the bridge should be aggregated into a performance counter.
    */
  def apply(
    target:      UInt,
    clock:       Clock,
    reset:       Reset,
    label:       String,
    description: String,
    opType:      PerfCounterOpType = PerfCounterOps.Accumulate,
  ): Unit =
    emitAnnotation(target, clock, reset, label, description, opType)

  /** A simplified variation of the full apply method above that uses the implicit clock and reset.
    */
  def apply(target: UInt, label: String, description: String): Unit =
    emitAnnotation(target, Module.clock, Module.reset, label, description, PerfCounterOps.Accumulate)

  /** Passes the annotated UInt through to the driver without accumulation. Use cases:
    *   - Custom accumulation / counting logic not supported by the driver
    *   - Providing runtime metadata along side standard accumulation registers
    *
    * Note: Under reset, the passthrough value is set to 0. This keeps event handling uniform in the transform.
    */
  def identity(target: UInt, label: String, description: String): Unit = {
    require(
      target.getWidth <= 64,
      s"""|PerfCounter.identity can only accept fields <= 64b wide. Provided target for label:
          |  $label
          |was ${target.getWidth}b.""".stripMargin,
    )
    emitAnnotation(target, Module.clock, Module.reset, label, description, opType = PerfCounterOps.Identity)
  }
}

case class PlusArgFirrtlAnnotation(
  target: InstanceTarget
) extends SingleTargetAnnotation[InstanceTarget]
    with FAMEAnnotation {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

object PlusArg {
  private def emitAnnotation(
    target: BaseModule
  ): Unit = {
    annotate(new ChiselAnnotation {
      def toFirrtl = {
        val parent = ModuleTarget(target.toNamed.circuit.name, target.parentModName)
        PlusArgFirrtlAnnotation(parent.instOf(target.instanceName, target.name))
      }
    })
  }

  /** Labels a Rocket Chip 'plusarg_reader' module to synthesize. Must be of the type found in
    * https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/PlusArg.scala
    *
    * @param target
    *   The 'plusarg_reader' module to synthesize
    */
  def apply(target: BaseModule): Unit = {
    emitAnnotation(target)
  }
}

// Need serialization utils to be upstreamed to FIRRTL before i can use these.
//sealed trait TriggerSourceType
//case object Credit extends TriggerSourceType
//case object Debit extends TriggerSourceType

case class TriggerSourceAnnotation(
  target:     ReferenceTarget,
  clock:      ReferenceTarget,
  reset:      Option[ReferenceTarget],
  sourceType: Boolean,
) extends Annotation
    with FAMEAnnotation {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedTarget = renamer.exactRename(target)
    val renamedClock  = renamer.exactRename(clock)
    val renamedReset  = reset.map(renamer.exactRename)
    Seq(this.copy(target = renamedTarget, clock = renamedClock, reset = renamedReset))
  }
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, target.module)
  def enclosingModule(): String             = target.module
}

case class TriggerSinkAnnotation(
  target: ReferenceTarget,
  clock:  ReferenceTarget,
) extends Annotation
    with FAMEAnnotation {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedTarget = renamer.exactRename(target)
    val renamedClock  = renamer.exactRename(clock)
    Seq(this.copy(target = renamedTarget, clock = renamedClock))
  }
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, target.module)
}

object TriggerSource {
  private def annotateTrigger(tpe: Boolean)(target: Bool, reset: Option[Bool]): Unit = {
    // Hack: Create dummy nodes until chisel-side instance annotations have been improved
    val clock = WireDefault(Module.clock)
    reset.map(dontTouch.apply)
    requireIsHardware(target, "Target passed to TriggerSource:")
    reset.foreach { requireIsHardware(_, "Reset passed to TriggerSource:") }
    annotate(new ChiselAnnotation {
      def toFirrtl =
        TriggerSourceAnnotation(target.toNamed.toTarget, clock.toNamed.toTarget, reset.map(_.toTarget), tpe)
    })
  }
  def annotateCredit = annotateTrigger(true) _
  def annotateDebit  = annotateTrigger(false) _

  /** Methods to annotate a Boolean as a trigger credit or debit. Credits and debits issued while the module's implicit
    * reset is asserted are not counted.
    */
  def credit(credit: Bool): Unit = annotateCredit(credit, Some(Module.reset.asBool))
  def debit(debit:   Bool): Unit = annotateDebit(debit, Some(Module.reset.asBool))
  def apply(creditSig: Bool, debitSig: Bool): Unit = {
    credit(creditSig)
    debit(debitSig)
  }

  /** Variations of the above methods that count credits and debits provided while the implicit reset is asserted.
    */
  def creditEvenUnderReset(credit: Bool): Unit = annotateCredit(credit, None)
  def debitEvenUnderReset(debit:   Bool): Unit = annotateDebit(debit, None)
  def evenUnderReset(creditSig: Bool, debitSig: Bool): Unit = {
    creditEvenUnderReset(creditSig)
    debitEvenUnderReset(debitSig)
  }

  /** Level sensitive trigger sources. Implemented using [[credit]] and [[debit]]. Note: This generated hardware in your
    * target design.
    *
    * @param src
    *   Enables the trigger when asserted. If no other credits have been issued since (e.g., a second level-sensitive
    *   enable was asserted), the trigger is disabled when src is desasserted.
    */
  def levelSensitiveEnable(src: Bool): Unit = {
    val srcLast = RegNext(src)
    credit(src && !srcLast)
    debit(!src && srcLast)
  }

}

object TriggerSink {

  /** Marks a bool as receiving the global trigger signal.
    *
    * @param target
    *   A Bool node that will be driven with the trigger
    *
    * @param noSourceDefault
    *   The value that the trigger signal should take on if no trigger soruces are found in the target. This is a
    *   temporary parameter required while this apply method generates a wire. Otherwise this can be punted to the
    *   target's RTL.
    */
  def apply(target: Bool, noSourceDefault: => Bool = true.B): Unit = {
    // Hack: Create dummy nodes until chisel-side instance annotations have been improved
    val targetWire = WireDefault(noSourceDefault)
    val clock      = Module.clock
    target := targetWire
    // Both the provided node and the generated one need to be dontTouched to stop
    // constProp from optimizing the down stream logic(?)
    dontTouch(target)
    annotate(new ChiselAnnotation {
      def toFirrtl = TriggerSinkAnnotation(targetWire.toTarget, clock.toTarget)
    })
  }

  /** Syntatic sugar for a when context that is predicated by a trigger sink. Example usage:
    * {{{
    * TriggerSink.whenEnabled {
    *   printf(<...>)
    * }
    * }}}
    *
    * @param noSourceDefault
    *   See [[TriggerSink.apply]].
    */
  def whenEnabled(noSourceDefault: => Bool = true.B)(elaborator: => Unit): Unit = {
    val sinkEnable = Wire(Bool())
    apply(sinkEnable, noSourceDefault)
    when(sinkEnable) { elaborator }
  }
}

case class RoCCBusyFirrtlAnnotation(
  target: ReferenceTarget,
  ready:  ReferenceTarget,
  valid:  ReferenceTarget,
) extends firrtl.annotations.Annotation
    with FAMEAnnotation {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer       = new ReferenceTargetRenamer(renames)
    val renamedReady  = renamer.exactRename(ready)
    val renamedValid  = renamer.exactRename(valid)
    val renamedTarget = renamer.exactRename(target)
    Seq(this.copy(target = renamedTarget, ready = renamedReady, valid = renamedValid))
  }
  def enclosingModuleTarget(): ModuleTarget = ModuleTarget(target.circuit, target.module)
  def enclosingModule(): String             = target.module
}

object MakeRoCCBusyLatencyInsensitive {
  def apply(
    target: Bool,
    ready:  Bool,
    valid:  Bool,
  ): Unit = {
    requireIsHardware(target, "Target passed to ..:")
    requireIsHardware(ready, "Ready passed to ..:")
    requireIsHardware(valid, "Valid passed to ..:")
    annotate(new ChiselAnnotation {
      def toFirrtl = RoCCBusyFirrtlAnnotation(target.toNamed.toTarget, ready.toNamed.toTarget, valid.toNamed.toTarget)
    })
  }
}

case class FirrtlPartWrapperParentAnnotation(
  target: InstanceTarget
) extends SingleTargetAnnotation[InstanceTarget]
    with FAMEAnnotation {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

case class FirrtlPortToNeighborRouterIdxAnno(
  target:             ReferenceTarget,
  extractNeighborIdx: Int,
  removeNeighborIdx:  Int,
) extends firrtl.annotations.Annotation
    with FAMEAnnotation {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer      = new ReferenceTargetRenamer(renames)
    val renameTarget = renamer.exactRename(target)
    Seq(this.copy(target = renameTarget))
  }
}

case class FirrtlCombLogicInsideModuleAnno(
  target: ReferenceTarget
) extends firrtl.annotations.Annotation
    with FAMEAnnotation {
  def update(renames: RenameMap): Seq[firrtl.annotations.Annotation] = {
    val renamer      = new ReferenceTargetRenamer(renames)
    val renameTarget = renamer.exactRename(target)
    Seq(this.copy(target = renameTarget))
  }
}
