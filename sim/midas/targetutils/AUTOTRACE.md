# AutoTrace history

Existing `AutoTrace(target, enable, label, description)` calls keep their current
behavior. To capture a window preceding a trigger, use:

```scala
// The 32 most recent valid PCs before an L1 miss.
AutoTrace.history(pc, pcValid, l1Miss, 32, "pcs_before_miss", "Recent valid PCs")

// The 32 most recent mispredicted branch PCs before an L1 miss.
AutoTrace.history(branchPc, branchMispredict, l1Miss, 32,
  "mispredicts_before_miss", "Recent mispredicted branches")
```

`sampleEnable` selects which cycles enter history; `trigger` requests a snapshot.
To sample every cycle, pass `true.B`. Each accepted target cycle with a high
trigger emits a snapshot, including consecutive triggers. The current cycle's
sample is excluded from that snapshot and becomes available to later triggers.
Snapshots do not clear the history.

The CSV row timestamp is the trigger cycle. Columns are `count` and
`samples[0].value` through `samples[N-1].value` for scalar targets, or
`samples[i].<field>` for aggregate targets. Samples are ordered oldest to newest.
`count` is the number of valid samples (at most N); only indices below `count`
are valid and remaining slots are zero. Local or global target reset clears the
history and suppresses snapshots. Sampling follows accepted target cycles,
independent of host stalls, the global trace trigger, and the driver's
`trace_enable`; those controls gate snapshot emission only.

The explicit clock/reset overload is:

```scala
AutoTrace.history(target, sampleEnable, trigger, depth,
  clock, reset, label, description)
```

History storage is generated in the FPGA host bridge, with no target registers
added. Storage grows with depth and target width; each queued snapshot contains
all N slots. History depth is independent of `AutoTraceBufferDepth`, which controls
the number of pending trigger-cycle batches. The existing backpressure mechanism
stalls target tokens when pending snapshots cannot be queued.
