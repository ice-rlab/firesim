# See LICENSE for license details.

################################################################################
# Target-Specific  Configuration
################################################################################

# Target-project that imports a specific makefrag for it's own chisel generator
TARGET_PROJECT ?=

# Root name for generated binaries
DESIGN ?=

# The host config package and class string
PLATFORM_CONFIG_PACKAGE ?= firesim.midasexamples
PLATFORM_CONFIG ?= DefaultF1Config

# The host platform type, currently only f1 is supported
PLATFORM ?=

# Driver source files
DRIVER_CC ?=
DRIVER_H ?=

# Target-specific CXX and LD flags for compiling the driver and meta-simulators
# These should be platform independent should be governed by the target-specific makefrag
TARGET_CXX_FLAGS ?=
TARGET_LD_FLAGS ?=

################################################################################
# File and directory setup
################################################################################

# The prefix used for all Golden Gate-generated files
BASE_FILE_NAME := FireSim-generated

name_quintuplet := $(PLATFORM)-$(TARGET_PROJECT)-$(DESIGN)-$(TARGET_CONFIG)-$(PLATFORM_CONFIG)
long_name := $(DESIGN_PACKAGE).$(DESIGN).$(TARGET_CONFIG)

# The directory into which generated verilog and headers will be dumped
# RTL simulations will also be built here
BUILD_DIR := $(firesim_base_dir)/generated-src
GENERATED_DIR ?= $(BUILD_DIR)/$(PLATFORM)/$(name_quintuplet)
# Results from RTL simulations live here
OUTPUT_DIR ?= $(firesim_base_dir)/output/$(PLATFORM)/$(name_quintuplet)

# The target's FIRRTL and associated anotations; inputs to Golden Gate
FIRRTL_FILE := $(GENERATED_DIR)/$(long_name).fir
ANNO_FILE := $(GENERATED_DIR)/$(long_name).anno.json



FIRRTL_FILE_CIRCT_IMPORT_MLIR ?= $(GENERATED_DIR)/$(long_name).firrtl_import.mlir
CIRCT_IMPORT_FIRRTL_LOG_FILE  ?= $(FIRRTL_FILE_CIRCT_IMPORT_MLIR).log


CIRCT_OPT_FLAGS_FILE := $(GENERATED_DIR)/$(long_name).circt-flags
CIRCT_COMPILER_FLAGS_FILE := $(GENERATED_DIR)/$(long_name).compiler_flags

CIRCT_DEBUG_DIR := $(GENERATED_DIR)/circt_debug

FIRRTL_FILE_CIRCT_OPT_MLIR    ?= $(GENERATED_DIR)/$(long_name).firrtl_opt.mlir
CIRCT_OPT_MLIR_LOG_FILE       ?= $(FIRRTL_FILE_CIRCT_OPT_MLIR).log
CIRCT_OPT_MLIR_STATS_FILE       ?= $(FIRRTL_FILE_CIRCT_OPT_MLIR).stats

FIRRTL_FILE_POST_CIRCT := $(GENERATED_DIR)/$(long_name).post_circt.fir
FIRRTL_FILE_POST_CIRCT_LOG_FILE ?= $(GENERATED_DIR)/$(long_name).circt-translate.log


# LO_FIRRTL_FILE_POST_CIRCT := $(GENERATED_DIR)/$(long_name).post_circt.lo.fir

# LO_CIRCT_ANNO_FILE := $(GENERATED_DIR)/$(long_name).post_circt.lo.anno.json



CIRCT_ANNO_FILE := $(GENERATED_DIR)/$(long_name).post_circt.anno.json
# CIRCT_OPT_ARGS ?=  -pass-pipeline='builtin.module(firrtl.circuit(firrtl.module(perf-insert-counter{targets=Rocket:wb_valid})),perf-emit-autocounter{file=$(CIRCT_ANNO_FILE)})' --debug-only=perf-insert-counter
CIRCT_OPT_ARGS ?=
CIRCT_OPT_FLAG_FILE := $(GENERATED_DIR)/$(long_name).circt_opt_flags.txt


CIRCT_FINAL_ANNO_FILE := $(GENERATED_DIR)/$(long_name)_final.anno.json




################################################################################
# Set up a fully-qualified classpath for the target.
################################################################################

# Rocket Chip stage requires a fully qualified classname for each fragment, whereas Chipyard's does not.
# This retains a consistent TARGET_CONFIG naming convention across the different target projects.
subst_prefix :=,$(TARGET_CONFIG_PACKAGE).

TARGET_CONFIG_QUALIFIED := $(TARGET_CONFIG_PACKAGE).$(subst _,$(subst_prefix),$(TARGET_CONFIG))
