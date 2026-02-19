# generate_lempar_embedded.cmake
# Embeds tools/lempar.c as a C string constant in _eval_lempar_embedded.h
#
# Usage: cmake -DLEMPAR_FILE=tools/lempar.c -DOUTPUT=src/_eval_lempar_embedded.h -P generate_lempar_embedded.cmake

if(NOT DEFINED LEMPAR_FILE OR NOT DEFINED OUTPUT)
    message(FATAL_ERROR "Usage: cmake -DLEMPAR_FILE=<path> -DOUTPUT=<path> -P generate_lempar_embedded.cmake")
endif()

file(READ "${LEMPAR_FILE}" content)

# Escape for C string
string(REPLACE "\\" "\\\\" content "${content}")
string(REPLACE "\"" "\\\"" content "${content}")
string(REPLACE "\t" "\\t" content "${content}")
string(REPLACE "\n" "\\n\"\n\"" content "${content}")
string(REPLACE "\r" "" content "${content}")

file(SIZE "${LEMPAR_FILE}" fsize)

file(WRITE "${OUTPUT}" "/* Auto-generated from lempar.c — do not edit */\n")
file(APPEND "${OUTPUT}" "#define LEMON_LIB_HAS_EMBEDDED_TEMPLATE 1\n")
file(APPEND "${OUTPUT}" "static const char lempar_template[] =\n")
file(APPEND "${OUTPUT}" "\"${content}\";\n")
file(APPEND "${OUTPUT}" "static const int lempar_template_len = ${fsize};\n")

message(STATUS "Generated ${OUTPUT} from ${LEMPAR_FILE} (${fsize} bytes)")
