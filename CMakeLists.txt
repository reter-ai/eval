cmake_minimum_required(VERSION 3.17)
project(chibi_eval C CXX)

option(BUILD_PYTHON_MODULE "Build the Python C extension module" ON)
option(BUILD_EVAL_STANDALONE "Build the standalone eval executable" ON)
option(BUILD_GRAMMAR_JIT "Build with LLVM JIT for runtime grammar compilation" ON)

if(BUILD_PYTHON_MODULE)
    find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)
endif()

# ---------- Generate install.h from platform detection ----------
include(CheckTypeSize)
include(CheckIncludeFile)

# Read chibi-scheme version info
file(READ ${CMAKE_SOURCE_DIR}/chibi-scheme/VERSION chibi_version)
string(STRIP "${chibi_version}" chibi_version)
file(READ ${CMAKE_SOURCE_DIR}/chibi-scheme/RELEASE chibi_release)
string(STRIP "${chibi_release}" chibi_release)

# Detect platform for install.h
if(WIN32)
    set(platform "windows")
    set(default_module_path "")
elseif(APPLE)
    set(platform "macosx")
    set(default_module_path "")
elseif(UNIX)
    set(platform "linux")
    set(default_module_path "")
else()
    set(platform "unix")
    set(default_module_path "")
endif()

set(architecture "${CMAKE_SYSTEM_PROCESSOR}")

set(version "${chibi_version}")
set(release "${chibi_release}")

configure_file(
    ${CMAKE_SOURCE_DIR}/chibi-scheme/include/chibi/install.h.in
    ${CMAKE_BINARY_DIR}/include/chibi/install.h
)

# ---------- Common compile definitions ----------
set(CHIBI_COMPILE_DEFS
    SEXP_STATIC_LIBRARY=1
    SEXP_USE_STATIC_LIBS=0
    SEXP_USE_DL=0
    SEXP_USE_GREEN_THREADS=1
    SEXP_USE_MMAP=0
    SEXP_USE_BOEHM=0
    BUILDING_EVAL_POOL=1
    EVAL_EMBEDDED_SCM=1
    LEMON_LIB_HAS_EMBEDDED_TEMPLATE=1
)

# ---------- Chibi-scheme core sources (statically compiled in) ----------
set(CHIBI_SOURCES
    chibi-scheme/gc.c
    chibi-scheme/sexp.c
    chibi-scheme/bignum.c
    chibi-scheme/gc_heap.c
    chibi-scheme/opcodes.c
    chibi-scheme/vm.c
    chibi-scheme/eval.c
    chibi-scheme/simplify.c
)

# ---------- Chibi-scheme library sources (statically compiled in) ----------
set(CHIBI_LIB_SOURCES
    chibi-scheme/lib/scheme/time.c
    chibi-scheme/lib/chibi/json.c
    chibi-scheme/lib/chibi/weak.c
    chibi-scheme/lib/chibi/ast.c
    chibi-scheme/lib/chibi/heap-stats.c
    chibi-scheme/lib/chibi/disasm.c
    chibi-scheme/lib/chibi/optimize/profile.c
    chibi-scheme/lib/chibi/optimize/rest.c
    chibi-scheme/lib/srfi/18/threads.c
    chibi-scheme/lib/srfi/27/rand.c
    chibi-scheme/lib/srfi/39/param.c
    chibi-scheme/lib/srfi/69/hash.c
    chibi-scheme/lib/srfi/95/qsort.c
    chibi-scheme/lib/srfi/98/env.c
    chibi-scheme/lib/srfi/151/bit.c
)

# ---------- Common include directories ----------
set(CHIBI_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/chibi-scheme/include
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/src
)

# ---------- Detect stub-generated .c files (shared by Python + standalone) ----------
set(STUB_GENERATED_FILES
    chibi-scheme/lib/chibi/filesystem.c
    chibi-scheme/lib/chibi/io/io.c
    chibi-scheme/lib/chibi/net.c
    chibi-scheme/lib/chibi/time.c
    chibi-scheme/lib/chibi/crypto/crypto.c
    chibi-scheme/lib/scheme/bytevector.c
    chibi-scheme/lib/srfi/144/math.c
    chibi-scheme/lib/srfi/160/uvprims.c
)
if(WIN32)
    list(APPEND STUB_GENERATED_FILES chibi-scheme/lib/chibi/win32/process-win32.c)
endif()

set(HAVE_STUB_LIBS TRUE)
foreach(stubfile ${STUB_GENERATED_FILES})
    if(NOT EXISTS "${CMAKE_SOURCE_DIR}/${stubfile}")
        set(HAVE_STUB_LIBS FALSE)
        message(STATUS "Stub lib missing: ${stubfile} (run 'make ffi' to generate)")
        break()
    endif()
endforeach()

# ---------- Generate embedded scheme/eval files (shared by Python + standalone) ----------
set(EMBEDDED_SCM_C "${CMAKE_BINARY_DIR}/eval_embedded_scm.c")
execute_process(
    COMMAND ${CMAKE_COMMAND}
        -DLIB_DIR=${CMAKE_SOURCE_DIR}/chibi-scheme/lib
        -DEVAL_LIB_DIR=${CMAKE_SOURCE_DIR}/chibi_eval/lib
        -DOUTPUT=${EMBEDDED_SCM_C}
        -P ${CMAKE_SOURCE_DIR}/tools/generate_embedded.cmake
    RESULT_VARIABLE _embed_result
)
if(NOT _embed_result EQUAL 0)
    message(FATAL_ERROR "Failed to generate embedded files")
endif()

# ---------- Generate embedded lempar.c template for lemon library ----------
set(LEMPAR_EMBEDDED_H "${CMAKE_BINARY_DIR}/_eval_lempar_embedded.h")
if(NOT EXISTS "${LEMPAR_EMBEDDED_H}")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
            -DLEMPAR_FILE=${CMAKE_SOURCE_DIR}/tools/lempar.c
            -DOUTPUT=${LEMPAR_EMBEDDED_H}
            -P ${CMAKE_SOURCE_DIR}/tools/generate_lempar_embedded.cmake
        RESULT_VARIABLE _lempar_result
    )
    if(NOT _lempar_result EQUAL 0)
        message(FATAL_ERROR "Failed to generate embedded lempar template")
    endif()
endif()

# ---------- re2c core static library (DFA lexer generator) ----------
set(RE2C_DIR ${CMAKE_SOURCE_DIR}/vendor/re2c)
set(RE2C_BOOT ${RE2C_DIR}/build_bootstrap)

set(RE2C_CORE_SOURCES
    ${RE2C_DIR}/src/codegen/helpers.cc
    ${RE2C_DIR}/src/codegen/output.cc
    ${RE2C_DIR}/src/codegen/pass1_analyze.cc
    ${RE2C_DIR}/src/codegen/pass2_generate.cc
    ${RE2C_DIR}/src/codegen/pass3_fixup.cc
    ${RE2C_DIR}/src/codegen/pass4_render.cc
    ${RE2C_DIR}/src/options/opt.cc
    ${RE2C_DIR}/src/options/symtab.cc
    ${RE2C_DIR}/src/nfa/re_to_nfa.cc
    ${RE2C_DIR}/src/adfa/adfa.cc
    ${RE2C_DIR}/src/debug/dump_adfa.cc
    ${RE2C_DIR}/src/debug/dump_cfg.cc
    ${RE2C_DIR}/src/debug/dump_dfa.cc
    ${RE2C_DIR}/src/debug/dump_dfa_tree.cc
    ${RE2C_DIR}/src/debug/dump_interf.cc
    ${RE2C_DIR}/src/debug/dump_nfa.cc
    ${RE2C_DIR}/src/cfg/cfg.cc
    ${RE2C_DIR}/src/cfg/compact.cc
    ${RE2C_DIR}/src/cfg/dce.cc
    ${RE2C_DIR}/src/cfg/freeze.cc
    ${RE2C_DIR}/src/cfg/interfere.cc
    ${RE2C_DIR}/src/cfg/liveanal.cc
    ${RE2C_DIR}/src/cfg/normalize.cc
    ${RE2C_DIR}/src/cfg/optimize.cc
    ${RE2C_DIR}/src/cfg/rename.cc
    ${RE2C_DIR}/src/cfg/varalloc.cc
    ${RE2C_DIR}/src/dfa/closure.cc
    ${RE2C_DIR}/src/dfa/dead_rules.cc
    ${RE2C_DIR}/src/dfa/determinization.cc
    ${RE2C_DIR}/src/dfa/fallback_tags.cc
    ${RE2C_DIR}/src/dfa/fillpoints.cc
    ${RE2C_DIR}/src/dfa/find_state.cc
    ${RE2C_DIR}/src/dfa/minimization.cc
    ${RE2C_DIR}/src/dfa/tagver_table.cc
    ${RE2C_DIR}/src/dfa/tcmd.cc
    ${RE2C_DIR}/src/encoding/ebcdic.cc
    ${RE2C_DIR}/src/encoding/enc.cc
    ${RE2C_DIR}/src/encoding/range_suffix.cc
    ${RE2C_DIR}/src/encoding/utf8.cc
    ${RE2C_DIR}/src/encoding/utf16.cc
    ${RE2C_DIR}/src/msg/msg.cc
    ${RE2C_DIR}/src/msg/warn.cc
    ${RE2C_DIR}/src/regexp/ast_to_re.cc
    ${RE2C_DIR}/src/regexp/default_tags.cc
    ${RE2C_DIR}/src/regexp/fixed_tags.cc
    ${RE2C_DIR}/src/regexp/nullable.cc
    ${RE2C_DIR}/src/regexp/regexp.cc
    ${RE2C_DIR}/src/regexp/split_charset.cc
    ${RE2C_DIR}/src/skeleton/control_flow.cc
    ${RE2C_DIR}/src/skeleton/generate_code.cc
    ${RE2C_DIR}/src/skeleton/generate_data.cc
    ${RE2C_DIR}/src/skeleton/maxpath.cc
    ${RE2C_DIR}/src/skeleton/skeleton.cc
    ${RE2C_DIR}/src/parse/ast.cc
    ${RE2C_DIR}/src/parse/input.cc
    ${RE2C_DIR}/src/util/file_utils.cc
    ${RE2C_DIR}/src/util/string_utils.cc
    ${RE2C_DIR}/src/util/range.cc
    ${RE2C_DIR}/src/main.cc
    # Bootstrap-generated sources (pre-built lexers/parsers)
    ${RE2C_BOOT}/src/parse/lexer.cc
    ${RE2C_BOOT}/src/parse/parser.cc
    ${RE2C_BOOT}/src/parse/conf_lexer.cc
    ${RE2C_BOOT}/src/parse/conf_parser.cc
    ${RE2C_BOOT}/src/options/parse_opts.cc
    ${RE2C_BOOT}/src/msg/ver_to_vernum.cc
    ${RE2C_BOOT}/src/msg/help_re2c.cc
)

add_library(re2c_core STATIC ${RE2C_CORE_SOURCES})

target_include_directories(re2c_core PRIVATE
    ${RE2C_DIR}            # for "src/..." includes
    ${RE2C_BOOT}           # for config.h and default_syntax_*.h
    ${RE2C_BOOT}/src       # for generated headers (parser.h, lexer.h, conf_parser.h)
)

# re2c uses C++11
set_target_properties(re2c_core PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)

target_compile_definitions(re2c_core PRIVATE
    RE2C_AS_LIBRARY=1
    RE2C_STDLIB_DIR=""
    _CRT_SECURE_NO_WARNINGS
)

if(MSVC)
    target_compile_options(re2c_core PRIVATE
        /W0           # suppress all warnings in vendored code
        /wd4244 /wd4267 /wd4996 /wd4146 /wd4805
    )
else()
    target_compile_options(re2c_core PRIVATE -w)  # suppress all warnings
endif()

# ---------- Grammar system sources (Lark parser + codegen + lemon lib) ----------
set(GRAMMAR_SOURCES
    src/_lark_lexer.c
    src/_lark_parser.c
    src/lark_grammar.c
    src/_lark_codegen.c
    src/_eval_lemon_lib.c
    src/_eval_re2c_lib.cpp
    src/_eval_grammar_bridge.c
)

# ---------- LLVM JIT (for runtime grammar compilation) ----------
if(BUILD_GRAMMAR_JIT)
    find_package(LLVM CONFIG QUIET)
    find_package(Clang CONFIG QUIET)

    if(LLVM_FOUND AND Clang_FOUND)
        message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION} — JIT grammar compilation enabled")
        message(STATUS "  LLVM includes: ${LLVM_INCLUDE_DIRS}")

        list(APPEND GRAMMAR_SOURCES src/_eval_jit.cpp)
        set(GRAMMAR_JIT_DEFS EVAL_HAVE_JIT=1)
        set(GRAMMAR_JIT_INCLUDES ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

        # Get LLVM libraries
        llvm_map_components_to_libnames(LLVM_LIBS
            core support orcjit native
            irreader bitwriter bitreader
            transformutils scalaropts instcombine
            mcparser mc target
        )
        set(CLANG_LIBS
            clangFrontend clangCodeGen clangDriver clangSerialization
            clangParse clangSema clangAnalysis clangAST clangBasic
            clangEdit clangLex
        )
        set(GRAMMAR_JIT_LIBS ${CLANG_LIBS} ${LLVM_LIBS})
    else()
        message(STATUS "LLVM/Clang not found — JIT grammar compilation disabled")
        message(STATUS "  Install LLVM via vcpkg: vcpkg install llvm[clang,target-x86,enable-rtti]")
        message(STATUS "  Then pass -DCMAKE_TOOLCHAIN_FILE=<vcpkg>/scripts/buildsystems/vcpkg.cmake")
        set(BUILD_GRAMMAR_JIT OFF)
    endif()
endif()

if(NOT BUILD_GRAMMAR_JIT)
    set(GRAMMAR_JIT_DEFS "")
    set(GRAMMAR_JIT_INCLUDES "")
    set(GRAMMAR_JIT_LIBS "")
endif()

# ---------- Cap'n Proto serialization ----------
find_package(CapnProto CONFIG QUIET)
if(CapnProto_FOUND)
    message(STATUS "Found Cap'n Proto — binary serialization enabled")
    set(CAPNP_SOURCES src/_capnp_bridge.cpp)
    set(CAPNP_DEFS EVAL_HAVE_CAPNP=1)
    set(CAPNP_LIBS CapnProto::capnp CapnProto::capnpc CapnProto::kj)
else()
    message(STATUS "Cap'n Proto not found — serialization disabled")
    set(CAPNP_SOURCES "")
    set(CAPNP_DEFS "")
    set(CAPNP_LIBS "")
endif()

# ---------- Apache Arrow (optional columnar data / Parquet / CSV) ----------
find_package(Arrow CONFIG QUIET)
find_package(ArrowCompute CONFIG QUIET)
find_package(Parquet CONFIG QUIET)
if(Arrow_FOUND)
    message(STATUS "Found Apache Arrow — columnar data processing enabled")
    set(ARROW_SOURCES src/_eval_arrow.cpp)
    set(ARROW_DEFS EVAL_HAVE_ARROW=1)
    set(ARROW_LIBS Arrow::arrow_shared)
    if(ArrowCompute_FOUND)
        message(STATUS "  Compute kernels enabled")
        list(APPEND ARROW_LIBS ArrowCompute::arrow_compute_shared)
    endif()
    if(Parquet_FOUND)
        message(STATUS "  Parquet I/O enabled")
        list(APPEND ARROW_LIBS Parquet::parquet_shared)
        list(APPEND ARROW_DEFS EVAL_HAVE_PARQUET=1)
    endif()
else()
    message(STATUS "Apache Arrow not found — columnar data disabled")
    set(ARROW_SOURCES "")
    set(ARROW_DEFS "")
    set(ARROW_LIBS "")
endif()

# ---------- TensorFlow C API (optional GPU-accelerated tensor ops) ----------
find_path(TF_INCLUDE_DIR tensorflow/c/c_api.h
    HINTS ${CMAKE_SOURCE_DIR}/vcpkg_installed/x64-windows/include
          ${CMAKE_SOURCE_DIR}/vcpkg_installed/${VCPKG_TARGET_TRIPLET}/include)
find_library(TF_LIBRARY NAMES tensorflow
    HINTS ${CMAKE_SOURCE_DIR}/vcpkg_installed/x64-windows/lib
          ${CMAKE_SOURCE_DIR}/vcpkg_installed/${VCPKG_TARGET_TRIPLET}/lib)
if(TF_INCLUDE_DIR AND TF_LIBRARY)
    message(STATUS "Found TensorFlow — GPU-accelerated tensor ops enabled")
    set(TF_SOURCES src/_eval_tf.c)
    set(TF_DEFS EVAL_HAVE_TF=1)
    set(TF_LIBS ${TF_LIBRARY})
    set(TF_INCLUDES ${TF_INCLUDE_DIR})
else()
    message(STATUS "TensorFlow not found — using native tensor ops")
    set(TF_SOURCES "")
    set(TF_DEFS "")
    set(TF_LIBS "")
    set(TF_INCLUDES "")
endif()

# ============================================================
# Python C extension module
# ============================================================
if(BUILD_PYTHON_MODULE)
    set(EXT_SOURCES
        src/_chibi_module.c
        src/_chibi_context.c
        src/_chibi_sexp.c
        src/_chibi_convert.c
        src/_chibi_pyobject_type.c
        src/_chibi_bridge.c
        src/_eval_writer.c
        src/_eval_lexer.c
        src/eval_grammar.c
        src/_eval_parser_helpers.c
        src/_chibi_serialize.c
        src/_eval_pool.c
        src/_eval_concurrent.c
        src/_eval_lib_init.c
        src/_eval_rete.c
        src/_eval_datetime.c
        src/_eval_decimal.c
        src/_eval_quantity.c
        src/_eval_ad.c
        src/eval_embedded_extract.c
        ${EMBEDDED_SCM_C}
    )

    if(HAVE_STUB_LIBS)
        list(APPEND EXT_SOURCES src/_eval_stub_libs.c)
    endif()

    python_add_library(_chibi MODULE ${CHIBI_SOURCES} ${CHIBI_LIB_SOURCES} ${EXT_SOURCES} ${GRAMMAR_SOURCES} ${CAPNP_SOURCES} ${ARROW_SOURCES} ${TF_SOURCES})

    target_include_directories(_chibi PRIVATE ${CHIBI_INCLUDE_DIRS} ${CMAKE_BINARY_DIR} ${GRAMMAR_JIT_INCLUDES} ${TF_INCLUDES})
    target_link_libraries(_chibi PRIVATE re2c_core)

    if(WIN32)
        target_include_directories(_chibi PRIVATE
            ${CMAKE_SOURCE_DIR}/src/win_compat
        )
    endif()

    target_compile_definitions(_chibi PRIVATE ${CHIBI_COMPILE_DEFS} ${GRAMMAR_JIT_DEFS} ${CAPNP_DEFS} ${ARROW_DEFS} ${TF_DEFS})

    if(HAVE_STUB_LIBS)
        target_compile_definitions(_chibi PRIVATE EVAL_HAVE_STUB_LIBS=1)
    endif()

    if(WIN32)
        target_link_libraries(_chibi PRIVATE ws2_32 ${GRAMMAR_JIT_LIBS} ${CAPNP_LIBS} ${ARROW_LIBS} ${TF_LIBS})
        target_compile_definitions(_chibi PRIVATE
            _CRT_SECURE_NO_WARNINGS
            SEXP_USE_STRING_STREAMS=0
        )
        if(TF_LIBRARY AND MSVC)
            target_link_libraries(_chibi PRIVATE delayimp.lib)
            target_link_options(_chibi PRIVATE /DELAYLOAD:tensorflow.dll)
        endif()
    elseif(UNIX)
        target_link_libraries(_chibi PRIVATE re2c_core m ${GRAMMAR_JIT_LIBS} ${CAPNP_LIBS} ${ARROW_LIBS} ${TF_LIBS})
    endif()

    if(BUILD_GRAMMAR_JIT)
        set_source_files_properties(src/_eval_jit.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(CapnProto_FOUND)
        set_source_files_properties(src/_capnp_bridge.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(Arrow_FOUND)
        set_source_files_properties(src/_eval_arrow.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(MSVC)
        target_compile_options(_chibi PRIVATE /W3 /wd4244 /wd4267 /wd4996)
    else()
        target_compile_options(_chibi PRIVATE -Wno-unused-parameter -Wno-sign-compare)
    endif()

    install(TARGETS _chibi LIBRARY DESTINATION chibi_eval)
endif()

# ============================================================
# Standalone eval executable
# ============================================================
if(BUILD_EVAL_STANDALONE)
    set(STANDALONE_SOURCES
        src/eval_main.c
        src/eval_embedded_extract.c
        ${EMBEDDED_SCM_C}
        src/_eval_writer.c
        src/_eval_lexer.c
        src/eval_grammar.c
        src/_eval_parser_helpers.c
        src/_chibi_serialize.c
        src/_eval_pool.c
        src/_eval_concurrent.c
        src/_eval_lib_init.c
        src/_eval_rete.c
        src/_eval_datetime.c
        src/_eval_decimal.c
        src/_eval_quantity.c
        src/_eval_ad.c
    )

    if(HAVE_STUB_LIBS)
        list(APPEND STANDALONE_SOURCES src/_eval_stub_libs.c)
        message(STATUS "Including stub-generated libraries (chibi/net, chibi/filesystem, etc.)")
    endif()

    add_executable(eval_standalone
        ${CHIBI_SOURCES}
        ${CHIBI_LIB_SOURCES}
        ${STANDALONE_SOURCES}
        ${GRAMMAR_SOURCES}
        ${CAPNP_SOURCES}
        ${ARROW_SOURCES}
        ${TF_SOURCES}
    )

    set_target_properties(eval_standalone PROPERTIES OUTPUT_NAME eval)

    target_include_directories(eval_standalone PRIVATE ${CHIBI_INCLUDE_DIRS} ${CMAKE_BINARY_DIR} ${GRAMMAR_JIT_INCLUDES} ${TF_INCLUDES})

    if(WIN32)
        target_include_directories(eval_standalone PRIVATE
            ${CMAKE_SOURCE_DIR}/src/win_compat
        )
    endif()

    target_compile_definitions(eval_standalone PRIVATE
        ${CHIBI_COMPILE_DEFS}
        ${GRAMMAR_JIT_DEFS}
        ${CAPNP_DEFS}
        ${ARROW_DEFS}
        ${TF_DEFS}
        EVAL_STANDALONE=1
    )

    if(HAVE_STUB_LIBS)
        target_compile_definitions(eval_standalone PRIVATE EVAL_HAVE_STUB_LIBS=1)
    endif()

    if(WIN32)
        target_link_libraries(eval_standalone PRIVATE re2c_core ws2_32 ${GRAMMAR_JIT_LIBS} ${CAPNP_LIBS} ${ARROW_LIBS} ${TF_LIBS})
        target_compile_definitions(eval_standalone PRIVATE
            _CRT_SECURE_NO_WARNINGS
            SEXP_USE_STRING_STREAMS=0
        )
        # Delay-load TF so the exe starts without tensorflow.dll present;
        # if the DLL is next to the exe or on PATH it gets loaded on first use.
        if(TF_LIBRARY AND MSVC)
            target_link_libraries(eval_standalone PRIVATE delayimp.lib)
            target_link_options(eval_standalone PRIVATE /DELAYLOAD:tensorflow.dll)
        endif()
    elseif(UNIX)
        target_link_libraries(eval_standalone PRIVATE re2c_core m pthread ${GRAMMAR_JIT_LIBS} ${CAPNP_LIBS} ${ARROW_LIBS} ${TF_LIBS})
    endif()

    if(BUILD_GRAMMAR_JIT)
        set_source_files_properties(src/_eval_jit.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(CapnProto_FOUND)
        set_source_files_properties(src/_capnp_bridge.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(Arrow_FOUND)
        set_source_files_properties(src/_eval_arrow.cpp PROPERTIES
            COMPILE_FLAGS "$<IF:$<CXX_COMPILER_ID:MSVC>,/std:c++17,-std=c++17>")
    endif()

    if(MSVC)
        target_compile_options(eval_standalone PRIVATE /W3 /wd4244 /wd4267 /wd4996)
    else()
        target_compile_options(eval_standalone PRIVATE -Wno-unused-parameter -Wno-sign-compare)
    endif()

    # Copy TensorFlow DLL next to executable so it loads at runtime
    if(TF_LIBRARY AND WIN32)
        get_filename_component(TF_LIB_DIR "${TF_LIBRARY}" DIRECTORY)
        get_filename_component(TF_LIB_DIR "${TF_LIB_DIR}" DIRECTORY)  # up from lib/ to prefix/
        set(TF_DLL "${TF_LIB_DIR}/bin/tensorflow.dll")
        if(EXISTS "${TF_DLL}")
            add_custom_command(TARGET eval_standalone POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${TF_DLL}" "$<TARGET_FILE_DIR:eval_standalone>"
                COMMENT "Copying tensorflow.dll to output directory"
            )
        endif()
    endif()

    install(TARGETS eval_standalone RUNTIME DESTINATION bin)
endif()
