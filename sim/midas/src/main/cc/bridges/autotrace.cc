// See LICENSE for license details.
#include "autotrace.h"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <limits>
#include <stdexcept>
#include <gmp.h>

namespace {
std::string csv_quote(const std::string &value) {
  std::string result = "\"";
  for (char c : value) {
    if (c == '"')
      result += '"';
    result += c;
  }
  return result + '"';
}

uint64_t read_le(const uint8_t *data, unsigned bytes) {
  uint64_t result = 0;
  for (unsigned i = 0; i < bytes; ++i)
    result |= uint64_t(data[i]) << (8 * i);
  return result;
}

size_t record_size(uint32_t width) {
  return ((uint64_t(width) + 128 + 511) / 512) * 64;
}

std::string field_value(const uint8_t *target,
                        uint32_t target_width,
                        const AutoTraceField &field) {
  mpz_t value, modulus;
  mpz_inits(value, modulus, nullptr);
  mpz_import(value, (uint64_t(target_width) + 7) / 8, -1, 1, 0, 0, target);
  mpz_fdiv_q_2exp(value, value, field.offset);
  mpz_fdiv_r_2exp(value, value, field.width);
  if (field.is_signed && mpz_tstbit(value, field.width - 1)) {
    mpz_setbit(modulus, field.width);
    mpz_sub(value, value, modulus);
  }
  std::vector<char> text(mpz_sizeinbase(value, 10) + 3);
  mpz_get_str(text.data(), 10, value);
  mpz_clears(value, modulus, nullptr);
  return text.data();
}
} // namespace

autotrace_csv_t::autotrace_csv_t(
    std::ostream &output,
    const std::vector<AutoTraceMetadata> &metadata,
    const ClockInfo &clock_info)
    : output(output), metadata(metadata), clock_info(clock_info) {
  if (metadata.empty())
    throw std::invalid_argument("AutoTrace requires source metadata");
  for (size_t i = 0; i < metadata.size(); ++i) {
    const auto &source = metadata[i];
    if (!id_to_source.emplace(source.trace_id, i).second)
      throw std::invalid_argument("Duplicate AutoTrace trace ID");
    uint64_t offset = 0;
    std::vector<std::string> paths;
    for (const auto &field : source.fields) {
      if (field.path.empty() || !field.width || field.offset != offset ||
          std::find(paths.begin(), paths.end(), field.path) != paths.end())
        throw std::invalid_argument("Invalid AutoTrace field schema");
      offset += field.width;
      paths.push_back(field.path);
      if (std::find(columns.begin(), columns.end(), field.path) == columns.end())
        columns.push_back(field.path);
    }
    if (!offset || offset != source.target_width)
      throw std::invalid_argument("AutoTrace schema width does not match target");
  }
  output << "clock_domain,clock_multiplier,clock_divisor,cycle,trace_id,"
            "instance_path,label,description";
  for (const auto &column : columns)
    output << ',' << csv_quote("target." + column);
  output << '\n';
}

void autotrace_csv_t::emit_record(const AutoTraceMetadata &source,
                                 const uint8_t *record) {
  const uint64_t cycle = read_le(record + 8, 8);
  if (have_cycle && cycle < last_cycle)
    throw std::runtime_error("AutoTrace cycle order went backwards");
  have_cycle = true;
  last_cycle = cycle;
  output << csv_quote(clock_info.domain_name) << ',' << clock_info.multiplier
         << ',' << clock_info.divisor << ',' << cycle << ',' << source.trace_id
         << ',' << csv_quote(source.instance_path) << ','
         << csv_quote(source.label) << ',' << csv_quote(source.description);
  for (const auto &column : columns) {
    output << ',';
    const auto field = std::find_if(
        source.fields.begin(), source.fields.end(),
        [&](const AutoTraceField &field) { return field.path == column; });
    if (field != source.fields.end())
      output << field_value(record + 16, source.target_width, *field);
  }
  output << '\n';
  if (!output)
    throw std::runtime_error("Could not write AutoTrace CSV");
}

void autotrace_csv_t::consume(const uint8_t *data, size_t size) {
  if (!size)
    return;
  pending.insert(pending.end(), data, data + size);
  size_t consumed = 0;
  while (pending.size() - consumed >= 16) {
    const auto *record = pending.data() + consumed;
    const auto id = uint32_t(read_le(record, 4));
    const auto found = id_to_source.find(id);
    if (found == id_to_source.end())
      throw std::runtime_error("Unknown AutoTrace trace ID: " + std::to_string(id));
    const auto &source = metadata[found->second];
    if (read_le(record + 4, 4) != source.target_width)
      throw std::runtime_error("AutoTrace record width does not match trace-ID schema");
    const size_t bytes = record_size(source.target_width);
    if (pending.size() - consumed < bytes)
      break;
    emit_record(source, record);
    consumed += bytes;
  }
  pending.erase(pending.begin(), pending.begin() + consumed);
}

void autotrace_csv_t::finish() {
  if (!pending.empty())
    throw std::runtime_error("Truncated AutoTrace record at end of stream");
  output.flush();
  if (!output)
    throw std::runtime_error("Could not flush AutoTrace CSV");
}

char autotrace_t::KIND;

autotrace_t::autotrace_t(
    simif_t &sim,
    StreamEngine &stream,
    const AUTOTRACEBRIDGEMODULE_struct &mmio_addrs,
    unsigned tracerno,
    const std::vector<std::string> &args,
    unsigned stream_idx,
    unsigned stream_depth,
    const std::vector<AutoTraceMetadata> &metadata,
    const ClockInfo &clock_info)
    : streaming_bridge_driver_t(sim, stream, &KIND), mmio_addrs(mmio_addrs),
      stream_idx(stream_idx), buffer_size(size_t(stream_depth) * STREAM_WIDTH_BYTES),
      buffer(nullptr, &std::free) {
  if (!stream_depth || buffer_size > std::numeric_limits<size_t>::max() - page_size)
    throw std::invalid_argument("Invalid AutoTrace stream depth");
  const size_t allocated = ((buffer_size + page_size - 1) / page_size) * page_size;
  buffer.reset(static_cast<uint8_t *>(std::aligned_alloc(page_size, allocated)));
  if (!buffer)
    throw std::bad_alloc();
  std::string filename_base = "AUTOTRACEFILE";
  const std::string filename_arg = "+autotrace-filename-base=";
  for (const auto &arg : args) {
    if (arg == "+autotrace-disable")
      enabled = false;
    if (arg.find(filename_arg) == 0)
      filename_base = arg.substr(filename_arg.size());
  }
  if (enabled) {
    if (filename_base.empty())
      throw std::invalid_argument("AutoTrace filename base must not be empty");
    const auto filename = filename_base + std::to_string(tracerno) + ".csv";
    output.open(filename);
    if (!output)
      throw std::runtime_error("Could not open AutoTrace CSV: " + filename);
    csv = std::make_unique<autotrace_csv_t>(output, metadata, clock_info);
  }
}

autotrace_t::~autotrace_t() = default;

void autotrace_t::init() {
  write(mmio_addrs.trace_enable, enabled);
  write(mmio_addrs.init_done, 1);
}

size_t autotrace_t::drain() {
  const size_t bytes = pull(stream_idx, buffer.get(), buffer_size, STREAM_WIDTH_BYTES);
  if (bytes > buffer_size)
    throw std::runtime_error("AutoTrace stream returned more bytes than requested");
  if (csv && bytes)
    csv->consume(buffer.get(), bytes);
  return bytes;
}

void autotrace_t::tick() {
  if (enabled && !finished)
    drain();
}

void autotrace_t::finish() {
  if (finished)
    return;
  // Freeze capture and target-token acceptance; the serializer keeps running.
  write(mmio_addrs.trace_enable, 0);
  write(mmio_addrs.init_done, 0);
  auto last_progress = std::chrono::steady_clock::now();
  while (!read(mmio_addrs.idle)) {
    pull_flush(stream_idx);
    if (drain())
      last_progress = std::chrono::steady_clock::now();
    if (std::chrono::steady_clock::now() - last_progress > std::chrono::seconds(30))
      throw std::runtime_error("AutoTrace did not drain its hardware buffers");
  }
  // An empty pull while hardware is still serializing is not an end-of-stream.
  pull_flush(stream_idx);
  while (drain()) {}
  if (csv)
    csv->finish();
  finished = true;
}
