cmake_minimum_required(VERSION 3.17)
project(chibi_eval C)

option(BUILD_PYTHON_MODULE "Build the Python C extension module" ON)
option(BUILD_EVAL_STANDALONE "Build the standalone eval executable" ON)

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
        src/_eval_lib_init.c
        src/eval_embedded_extract.c
        ${EMBEDDED_SCM_C}
    )

    if(HAVE_STUB_LIBS)
        list(APPEND EXT_SOURCES src/_eval_stub_libs.c)
    endif()

    python_add_library(_chibi MODULE ${CHIBI_SOURCES} ${CHIBI_LIB_SOURCES} ${EXT_SOURCES})

    target_include_directories(_chibi PRIVATE ${CHIBI_INCLUDE_DIRS})

    if(WIN32)
        target_include_directories(_chibi PRIVATE
            ${CMAKE_SOURCE_DIR}/src/win_compat
        )
    endif()

    target_compile_definitions(_chibi PRIVATE ${CHIBI_COMPILE_DEFS})

    if(HAVE_STUB_LIBS)
        target_compile_definitions(_chibi PRIVATE EVAL_HAVE_STUB_LIBS=1)
    endif()

    if(WIN32)
        target_link_libraries(_chibi PRIVATE ws2_32)
        target_compile_definitions(_chibi PRIVATE
            _CRT_SECURE_NO_WARNINGS
            SEXP_USE_STRING_STREAMS=0
        )
    elseif(UNIX)
        target_link_libraries(_chibi PRIVATE m)
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
        src/_eval_lib_init.c
    )

    if(HAVE_STUB_LIBS)
        list(APPEND STANDALONE_SOURCES src/_eval_stub_libs.c)
        message(STATUS "Including stub-generated libraries (chibi/net, chibi/filesystem, etc.)")
    endif()

    add_executable(eval_standalone
        ${CHIBI_SOURCES}
        ${CHIBI_LIB_SOURCES}
        ${STANDALONE_SOURCES}
    )

    set_target_properties(eval_standalone PROPERTIES OUTPUT_NAME eval)

    target_include_directories(eval_standalone PRIVATE ${CHIBI_INCLUDE_DIRS})

    if(WIN32)
        target_include_directories(eval_standalone PRIVATE
            ${CMAKE_SOURCE_DIR}/src/win_compat
        )
    endif()

    target_compile_definitions(eval_standalone PRIVATE
        ${CHIBI_COMPILE_DEFS}
        EVAL_STANDALONE=1
    )

    if(HAVE_STUB_LIBS)
        target_compile_definitions(eval_standalone PRIVATE EVAL_HAVE_STUB_LIBS=1)
    endif()

    if(WIN32)
        target_link_libraries(eval_standalone PRIVATE ws2_32)
        target_compile_definitions(eval_standalone PRIVATE
            _CRT_SECURE_NO_WARNINGS
            SEXP_USE_STRING_STREAMS=0
        )
    elseif(UNIX)
        target_link_libraries(eval_standalone PRIVATE m pthread)
    endif()

    if(MSVC)
        target_compile_options(eval_standalone PRIVATE /W3 /wd4244 /wd4267 /wd4996)
    else()
        target_compile_options(eval_standalone PRIVATE -Wno-unused-parameter -Wno-sign-compare)
    endif()

    install(TARGETS eval_standalone RUNTIME DESTINATION bin)
endif()
