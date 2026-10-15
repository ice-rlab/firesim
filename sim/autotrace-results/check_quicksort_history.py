#!/usr/bin/env python3
"""Compare BOOM L1D-acquire history snapshots with the independent branch trace."""

import argparse
from collections import Counter, defaultdict, deque
import csv
from itertools import groupby


def validate(path):
    histories = defaultdict(lambda: deque(maxlen=32))
    snapshots = Counter()
    triggers = Counter()
    counts = Counter()
    fields = ("branchMask", "mispredict", "pc", "robIdx", "taken")
    rows_checked = 0
    samples_checked = 0
    full_windows = 0
    previous_cycle = -1
    with open(path, newline="") as stream:
        reader = csv.DictReader(stream)
        assert "target.samples[31].pc" in reader.fieldnames, "Missing history columns"
        for cycle, batch in groupby(reader, key=lambda row: int(row["cycle"])):
            assert cycle >= previous_cycle, "Cycles went backwards"
            previous_cycle = cycle
            rows = list(batch)
            rows_checked += len(rows)
            acquire_instances = {
                row["instance_path"] for row in rows if row["label"] == "l1d_acquire"
            }
            for instance in acquire_instances:
                triggers[instance] += 1
            # Compare snapshots before inserting this cycle's branch resolutions.
            for row in rows:
                label = row["label"]
                instance = row["instance_path"]
                counts[label] += 1
                prefix = "branch_history_on_l1d_acquire_"
                if not label.startswith(prefix):
                    continue
                key = (instance, label[len(prefix):])
                assert instance in acquire_instances, f"Snapshot without trigger at {cycle}"
                expected = list(histories[key])
                count = int(row["target.count"])
                assert count == len(expected), f"History count mismatch at {cycle}: {key}"
                for index in range(32):
                    for field_index, field in enumerate(fields):
                        actual = int(row[f"target.samples[{index}].{field}"])
                        want = expected[index][field_index] if index < count else 0
                        assert actual == want, (
                            f"Mismatch at cycle {cycle}, {key}, sample {index}, {field}: "
                            f"got {actual}, expected {want}"
                        )
                snapshots[key] += 1
                samples_checked += count
                full_windows += count == 32
            for row in rows:
                if row["label"].startswith("branch_resolve_"):
                    key = (row["instance_path"], row["label"][len("branch_resolve_"):])
                    histories[key].append(tuple(int(row[f"target.{field}"]) for field in fields))
    assert snapshots, "No history snapshots emitted"
    assert full_windows, "No full 32-entry window exercised"
    for (instance, alu), n in snapshots.items():
        assert n == triggers[instance], f"Missing snapshots for ALU {alu}"
    for key in histories:
        assert key in snapshots, f"No history snapshots for {key}"
    print(f"PASS: {rows_checked} rows, {sum(triggers.values())} L1D acquires, "
          f"{sum(snapshots.values())} snapshots, {full_windows} full windows, "
          f"{samples_checked} valid samples; every field and zero-filled slot matched.")
    for label, count in sorted(counts.items()):
        print(f"  {label}: {count}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csv", nargs="?", default="quicksort0.csv")
    validate(parser.parse_args().csv)
