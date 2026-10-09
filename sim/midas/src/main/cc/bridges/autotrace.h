// See LICENSE for license details.
#ifndef __AUTOTRACE_H
#define __AUTOTRACE_H

#include "core/bridge_driver.h"
#include "core/clock_info.h"

#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>

struct AUTOTRACEBRIDGEMODULE_struct {
  uint64_t init_done;
  uint64_t trace_enable;
  uint64_t idle;
};

struct AutoTraceField {
  std::string path;
  uint32_t width;
  uint32_t offset;
  bool is_signed;
};

struct AutoTraceMetadata {
  uint32_t trace_id;
  std::string instance_path;
  std::string label;
  std::string description;
  uint32_t target_width;
  std::vector<AutoTraceField> fields;
};

/** Incremental decoder for AutoTraceRecord format version 1.
 * One CSV row represents one observation, with exact decimal values of any width.
 * Missing fields in heterogeneous observations are empty CSV cells.
 */
class autotrace_csv_t {
public:
  autotrace_csv_t(std::ostream &output,
                  const std::vector<AutoTraceMetadata> &metadata,
                  const ClockInfo &clock_info);
  void consume(const uint8_t *data, size_t size);
  void finish();

private:
  void emit_record(const AutoTraceMetadata &source, const uint8_t *record);
  std::ostream &output;
  std::vector<AutoTraceMetadata> metadata;
  ClockInfo clock_info;
  std::unordered_map<uint32_t, size_t> id_to_source;
  std::vector<std::string> columns;
  std::vector<uint8_t> pending;
  uint64_t last_cycle = 0;
  bool have_cycle = false;
};

class autotrace_t final : public streaming_bridge_driver_t {
public:
  static char KIND;
  autotrace_t(simif_t &sim,
               StreamEngine &stream,
               const AUTOTRACEBRIDGEMODULE_struct &mmio_addrs,
               unsigned tracerno,
               const std::vector<std::string> &args,
               unsigned stream_idx,
               unsigned stream_depth,
               const std::vector<AutoTraceMetadata> &metadata,
               const ClockInfo &clock_info);
  ~autotrace_t() override;
  void init() override;
  void tick() override;
  void finish() override;

private:
  size_t drain();
  const AUTOTRACEBRIDGEMODULE_struct mmio_addrs;
  const unsigned stream_idx;
  const size_t buffer_size;
  std::unique_ptr<uint8_t, decltype(&std::free)> buffer;
  std::ofstream output;
  std::unique_ptr<autotrace_csv_t> csv;
  bool enabled = true;
  bool finished = false;
};

#endif // __AUTOTRACE_H
