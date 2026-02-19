/*
 * _eval_re2c_lib.cpp — re2c DFA lexer generator as an in-process library.
 *
 * Wraps the vendored re2c source to compile .re format strings to C lexer code.
 * Fully in-memory: no temp files are created.
 */

#include "_eval_re2c_lib.h"

#include <cstdlib>
#include <cstring>
#include <string>

/* re2c in-memory compile function (declared in vendor/re2c/src/main.cc) */
namespace re2c {
    enum class Ret { OK, FAIL, EXIT };
    Ret compile_buffer(const char* input_data, size_t input_len,
                       std::string& output_code);
}

extern "C" Re2cOutput* re2c_lib_compile(const char* re_source) {
    if (!re_source) return nullptr;

    Re2cOutput* out = (Re2cOutput*)calloc(1, sizeof(Re2cOutput));
    if (!out) return nullptr;

    std::string output_code;
    re2c::Ret ret = re2c::compile_buffer(re_source, strlen(re_source), output_code);

    if (ret == re2c::Ret::OK && !output_code.empty()) {
        out->c_code = strdup(output_code.c_str());
        out->success = 1;
    } else {
        out->error_msg = strdup("re2c: compilation failed");
        out->success = 0;
    }

    return out;
}

extern "C" void re2c_lib_free(Re2cOutput* out) {
    if (!out) return;
    free(out->c_code);
    free(out->error_msg);
    free(out);
}
