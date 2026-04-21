# See LICENSE for license details.

SHELL := /bin/bash

FORCE:
.PHONY: FORCE

################################################################################
# Helpers for logging and running makefile commands
################################################################################

define run_command
	$(info )
	$(info MAKE :: $(1))
	$(info $(2))
	$(info )
	@$(2)
endef

################################################################################
# CIRCT flag stamp files to force recompilation when flags change
################################################################################

CIRCT_OPT_FLAGS_CMD = \
{ \
  echo "CIRCT_FLAGS=$(CIRCT_FLAGS)"; \
} > $@.tmp; \
cmp -s $@.tmp $@ || mv $@.tmp $@; \
rm -f $@.tmp

CIRCT_COMPILER_FLAGS_CMD = \
{ \
  echo "CIRCT_COMPILER_FLAGS=$(CIRCT_COMPILER_FLAGS)"; \
  echo "CIRCT_DEBUG_DIR=$(CIRCT_DEBUG_DIR)"; \
} > $@.tmp; \
cmp -s $@.tmp $@ || mv $@.tmp $@; \
rm -f $@.tmp

$(CIRCT_OPT_FLAGS_FILE): FORCE
	@mkdir -p $(dir $@)
	$(call run_command,CIRCT_OPT_FLAGS_FILE,$(CIRCT_OPT_FLAGS_CMD))

$(CIRCT_COMPILER_FLAGS_FILE): FORCE
	@mkdir -p $(dir $@)
	$(call run_command,CIRCT_COMPILER_FLAGS_FILE,$(CIRCT_COMPILER_FLAGS_CMD))

################################################################################
# FIRRTL -> MLIR FIR (via circt-translate)
################################################################################

CIRCT_IMPORT_CMD = \
	set -o pipefail; \
	firtool "$(FIRRTL_FILE)" \
		--parse-only \
		--disable-annotation-unknown \
		--annotation-file "$(ANNO_FILE)" \
		-o "$@" \
	|& tee "$(CIRCT_IMPORT_FIRRTL_LOG_FILE)"

$(FIRRTL_FILE_CIRCT_IMPORT_MLIR): $(FIRRTL_FILE) $(ANNO_FILE)
	@mkdir -p $(dir $@)
	$(call run_command,FIRRTL_FILE_CIRCT_IMPORT_MLIR,$(CIRCT_IMPORT_CMD))

################################################################################
# MLIR FIR -> MLIR FIR (via circt-opt)
################################################################################

CIRCT_FLAGS_EXPANDED = $(subst __CIRCT_DEBUG_DIR__,$(CIRCT_DEBUG_DIR),$(CIRCT_FLAGS))

CIRCT_FLAGS_ARG := $(if $(strip $(CIRCT_FLAGS_EXPANDED)),"$(strip $(CIRCT_FLAGS_EXPANDED))",)

CIRCT_OPT_CMD = \
set -o pipefail; \
mkdir -p "$(CIRCT_DEBUG_DIR)"; \
circt-opt "$(FIRRTL_FILE_CIRCT_IMPORT_MLIR)" $(CIRCT_FLAGS_ARG) \
	-o "$@" \
|& tee "$(CIRCT_OPT_MLIR_LOG_FILE)"

$(FIRRTL_FILE_CIRCT_OPT_MLIR): $(FIRRTL_FILE_CIRCT_IMPORT_MLIR) $(CIRCT_OPT_FLAGS_FILE) $(CIRCT_COMPILER_FLAGS_FILE)
	@mkdir -p $(dir $@)
	$(call run_command,FIRRTL_FILE_CIRCT_OPT_MLIR,$(CIRCT_OPT_CMD))

################################################################################
# MLIR FIR -> FIRRTL (via circt-translate --export-firrtl)
################################################################################

CIRCT_EXPORT_CMD = \
set -o pipefail; \
circt-translate "$(FIRRTL_FILE_CIRCT_OPT_MLIR)" \
	--export-firrtl \
	-o "$(FIRRTL_FILE_POST_CIRCT)" \
|& tee "$(FIRRTL_FILE_POST_CIRCT_LOG_FILE)"

$(FIRRTL_FILE_POST_CIRCT) $(CIRCT_ANNO_FILE): $(FIRRTL_FILE_CIRCT_OPT_MLIR)
	@mkdir -p $(dir $(FIRRTL_FILE_POST_CIRCT))
	$(call run_command,FIRRTL_FILE_POST_CIRCT,$(CIRCT_EXPORT_CMD))

################################################################################
# Merge CIRCT-generated annotations back into original annotation file
################################################################################

CIRCT_MERGE_ANNO_CMD = \
set -o pipefail; \
circt-opt "$(FIRRTL_FILE_CIRCT_OPT_MLIR)" \
  --pass-pipeline='builtin.module(firrtl.circuit(firrtl-emit-legacy-annotations{file=$(CIRCT_FINAL_ANNO_FILE)}))' \
  > /dev/null

$(CIRCT_FINAL_ANNO_FILE): $(FIRRTL_FILE_CIRCT_OPT_MLIR)
	$(call run_command,CIRCT_FINAL_ANNO_FILE,$(CIRCT_MERGE_ANNO_CMD))

# REMOVE_ANNO_CLASSES ?= \
# 	firrtl.transforms.DontTouchAnnotation \
# 	midas.targetutils.AutoCounterFirrtlAnnotation \
# 	midas.targetutils.PerfCounterOps$$Accumulate$$

# REMOVE_ANNO_CLASSES_JSON := $(shell \
# 	for c in $(REMOVE_ANNO_CLASSES); do printf '%s\n' "$$c"; done | jq -R . | jq -s -c . \
# )

# CIRCT_MERGE_ANNO_CMD = \
# jq --argjson remove '$(REMOVE_ANNO_CLASSES_JSON)' \
#    'map(select((.class // "") as $$c | ($$remove | index($$c)) | not))' \
#    "$(ANNO_FILE)" > "$@.filtered"; \
# if [ -f "$(CIRCT_ANNO_FILE)" ]; then \
# 	jq -s 'map(select(type == "array")) | add' "$@.filtered" "$(CIRCT_ANNO_FILE)" > "$@"; \
# else \
# 	echo "WARNING: CIRCT annotation file '\''$(CIRCT_ANNO_FILE)'\'' not found; using filtered ANNO_FILE" >&2; \
# 	cp "$@.filtered" "$@"; \
# fi; \
# rm -f "$@.filtered"

# $(CIRCT_FINAL_ANNO_FILE): $(ANNO_FILE) $(CIRCT_ANNO_FILE)
# 	$(call run_command,CIRCT_FINAL_ANNO_FILE,$(CIRCT_MERGE_ANNO_CMD))