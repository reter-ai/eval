# Makefile — Bootstrap build for eval with chibi-ffi stub compilation
#
# Three-phase build:
#   1. bootstrap: Build eval.exe without stub-generated libraries
#   2. ffi:       Use bootstrap eval.exe to compile .stub → .c via chibi-ffi
#   3. final:     Rebuild eval.exe with all stub-generated libraries
#
# Usage:
#   make              — full three-phase build
#   make bootstrap    — phase 1 only
#   make ffi          — phase 2 only (requires bootstrap)
#   make final        — phase 3 only (requires ffi)
#   make clean        — remove build artifacts
#   make test         — run all tests
#
# Requirements:
#   - CMake 3.17+
#   - MSVC (cl.exe) on Windows, gcc/clang on Unix
#   - No external chibi-scheme installation needed (self-bootstrapping)

# ---- Configuration ----
BUILD_DIR       := build_standalone
BOOTSTRAP_DIR   := build_bootstrap
EVAL_BOOTSTRAP  := $(BOOTSTRAP_DIR)/Release/eval.exe
EVAL_FINAL      := $(BUILD_DIR)/Release/eval.exe
CHIBI_FFI       := chibi-scheme/tools/chibi-ffi
CMAKE_BUILD_TYPE := Release

# ---- Stub files to compile ----
# Windows-compatible stubs (with cond-expand or pure C)
STUBS := \
    chibi-scheme/lib/chibi/filesystem.stub \
    chibi-scheme/lib/chibi/io/io.stub \
    chibi-scheme/lib/chibi/net.stub \
    chibi-scheme/lib/chibi/time.stub \
    chibi-scheme/lib/chibi/crypto/crypto.stub \
    chibi-scheme/lib/chibi/win32/process-win32.stub \
    chibi-scheme/lib/scheme/bytevector.stub \
    chibi-scheme/lib/srfi/144/math.stub \
    chibi-scheme/lib/srfi/160/uvprims.stub

# Generated .c files (same path, .stub → .c)
STUB_GENERATED := $(STUBS:.stub=.c)

# ---- Phony targets ----
.PHONY: all bootstrap ffi final clean test test-http

all: final

# ---- Phase 1: Bootstrap eval.exe ----
# Build eval without stub-generated libs (same as current build)
bootstrap: $(EVAL_BOOTSTRAP)

$(EVAL_BOOTSTRAP):
	@echo "==== Phase 1: Building bootstrap eval.exe ===="
	cmake -S . -B $(BOOTSTRAP_DIR) \
		-DBUILD_PYTHON_MODULE=OFF \
		-DBUILD_EVAL_STANDALONE=ON
	cmake --build $(BOOTSTRAP_DIR) --config $(CMAKE_BUILD_TYPE)
	@echo "==== Bootstrap eval.exe ready ===="

# ---- Phase 2: Compile .stub → .c using chibi-ffi ----
ffi: $(STUB_GENERATED)

# Pattern rule: any .stub → .c via chibi-ffi
# The bootstrap eval.exe runs chibi-ffi (a Scheme program) which reads
# the .stub file and writes the generated .c file
%.c: %.stub $(EVAL_BOOTSTRAP)
	@echo "  FFI $<"
	@$(EVAL_BOOTSTRAP) $(CHIBI_FFI) $< 2>/dev/null

# ---- Phase 3: Final build with all stub-generated libs ----
final: ffi
	@echo "==== Phase 3: Building final eval.exe ===="
	cmake -S . -B $(BUILD_DIR) \
		-DBUILD_PYTHON_MODULE=OFF \
		-DBUILD_EVAL_STANDALONE=ON
	cmake --build $(BUILD_DIR) --config $(CMAKE_BUILD_TYPE)
	@echo "==== Final eval.exe ready ===="

# ---- Test ----
test: final
	@echo "==== Running tests ===="
	@pass=0; fail=0; \
	for t in tests/eval/test_*.eval; do \
		name=$$(basename $$t .eval); \
		if $(EVAL_FINAL) "$$t" > /dev/null 2>&1; then \
			echo "  PASS $$name"; \
			pass=$$((pass + 1)); \
		else \
			echo "  FAIL $$name"; \
			fail=$$((fail + 1)); \
		fi; \
	done; \
	echo "==== $$pass passed, $$fail failed ===="

test-http: final
	$(EVAL_FINAL) tests/eval/test_http.eval

# ---- Clean ----
clean:
	rm -rf $(BOOTSTRAP_DIR)
	rm -f $(STUB_GENERATED)
