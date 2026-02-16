cmake_minimum_required(VERSION 3.17)
project(chibi_eval C)

find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)

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

# ---------- Extension module sources ----------
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
)

# ---------- Build the Python C extension ----------
python_add_library(_chibi MODULE ${CHIBI_SOURCES} ${EXT_SOURCES})

target_include_directories(_chibi PRIVATE
    ${CMAKE_SOURCE_DIR}/chibi-scheme/include
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/src
)

target_compile_definitions(_chibi PRIVATE
    SEXP_STATIC_LIBRARY=1
    SEXP_USE_STATIC_LIBS=0
    SEXP_USE_DL=0
    SEXP_USE_GREEN_THREADS=1
    SEXP_USE_MMAP=0
    SEXP_USE_BOEHM=0
)

# Platform-specific settings
if(WIN32)
    target_link_libraries(_chibi PRIVATE ws2_32)
    target_compile_definitions(_chibi PRIVATE
        _CRT_SECURE_NO_WARNINGS
        SEXP_USE_STRING_STREAMS=0
    )
elseif(UNIX)
    target_link_libraries(_chibi PRIVATE m)
endif()

# Suppress warnings for chibi-scheme C code
if(MSVC)
    target_compile_options(_chibi PRIVATE /W3 /wd4244 /wd4267 /wd4996)
else()
    target_compile_options(_chibi PRIVATE -Wno-unused-parameter -Wno-sign-compare)
endif()

# ---------- Install into Python package ----------
install(TARGETS _chibi LIBRARY DESTINATION chibi_eval)

# Install scheme library files as package data
install(DIRECTORY ${CMAKE_SOURCE_DIR}/chibi-scheme/lib/
    DESTINATION chibi_eval/_scheme_lib
    PATTERN "*.c" EXCLUDE
    PATTERN "*.stub" EXCLUDE
    PATTERN "*test.sld" EXCLUDE
)

# Install init files needed by chibi
install(FILES
    ${CMAKE_SOURCE_DIR}/chibi-scheme/lib/init-7.scm
    DESTINATION chibi_eval/_scheme_lib
)
