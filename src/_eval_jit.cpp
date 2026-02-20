/*
 * _eval_jit.cpp — LLVM ORC JIT + Clang in-memory C compilation.
 *
 * Compiles C source strings to native code entirely in-process:
 *   C string → Clang → LLVM IR → ORC JIT → executable memory
 *
 * No temp files, no external tools, no DLLs.
 */

#include "_eval_jit.h"

#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Basic/TargetOptions.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Lex/PreprocessorOptions.h>

#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/VirtualFileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Host.h>

#include <memory>
#include <string>
#include <atomic>
#include <cstdlib>
#include <cstring>

/* ---- Internals ---- */

struct EvalJIT {
    std::unique_ptr<llvm::orc::LLJIT> jit;
    std::atomic<int> module_counter{0};
};

static std::atomic<bool> llvm_initialized{false};

static void ensure_llvm_init(void) {
    bool expected = false;
    if (llvm_initialized.compare_exchange_strong(expected, true)) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
    }
}

static char* make_error(const std::string& msg) {
    char* s = (char*)malloc(msg.size() + 1);
    if (s) memcpy(s, msg.c_str(), msg.size() + 1);
    return s;
}

static char* make_error_from_llvm(llvm::Error err) {
    std::string msg;
    llvm::raw_string_ostream os(msg);
    os << err;
    return make_error(os.str());
}

/* Compile C source to LLVM IR Module using Clang in-memory. */
static std::unique_ptr<llvm::Module>
compile_c_to_ir(llvm::LLVMContext& llvm_ctx,
                const char* c_source,
                const std::string& triple,
                std::string& error_out)
{
    /* Set up diagnostics → string.
     * All objects heap-allocated and ref-counted so CompilerInstance
     * destructor doesn't delete stack objects. */
    auto diag_opts = llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions>(
        new clang::DiagnosticOptions());
    std::string diag_str;
    llvm::raw_string_ostream diag_os(diag_str);
    auto diag_printer = new clang::TextDiagnosticPrinter(diag_os, diag_opts.get());
    auto diag_ids = llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs>(
        new clang::DiagnosticIDs());
    auto diags = llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine>(
        new clang::DiagnosticsEngine(diag_ids, diag_opts, diag_printer));

    /* Build compiler invocation.
     * Generated code is freestanding (no system headers) for cross-platform JIT. */
    std::vector<const char*> args = {
        "-x", "c",           /* language: C */
        "-std=c11",
        "-O2",
#ifndef _WIN32
        "-fPIC",
#endif
        "-ffreestanding",    /* no implicit libc assumptions */
        "-DNDEBUG",          /* disable asserts and lemon tracing */
        "-Wno-everything",   /* suppress warnings in generated code */
    };

    auto invocation = std::make_shared<clang::CompilerInvocation>();
    bool ok = clang::CompilerInvocation::CreateFromArgs(
        *invocation, args, *diags);
    if (!ok) {
        error_out = "Failed to create Clang invocation: " + diag_str;
        return nullptr;
    }

    /* Set target triple */
    invocation->getTargetOpts().Triple = triple;

    /* Feed source as in-memory buffer */
    auto source_buf = llvm::MemoryBuffer::getMemBufferCopy(c_source, "input.c");
    invocation->getPreprocessorOpts().addRemappedFile("input.c", source_buf.release());
    invocation->getFrontendOpts().Inputs.clear();
    invocation->getFrontendOpts().Inputs.push_back(
        clang::FrontendInputFile("input.c", clang::Language::C));

    /* Create compiler instance on the heap and intentionally leak it.
     *
     * WORKAROUND: On Windows with LLVM 18, the CompilerInstance destructor
     * corrupts the process heap when called repeatedly.  Subsequent memory
     * allocations (malloc, new, etc.) crash or hang at random points.
     * Heap-allocating and never deleting avoids the destructor entirely.
     * The per-compilation leak (~1-2 MB) is acceptable for grammar JIT
     * which typically compiles only a handful of parsers per session. */
    auto* compiler = new clang::CompilerInstance();
    compiler->setInvocation(invocation);
    compiler->setDiagnostics(diags.get());
    compiler->createFileManager(llvm::vfs::getRealFileSystem());

    /* Run EmitLLVMOnlyAction to get Module */
    auto action = std::make_unique<clang::EmitLLVMOnlyAction>(&llvm_ctx);
    bool exec_ok = compiler->ExecuteAction(*action);
    if (!exec_ok) {
        error_out = "Clang compilation failed: " + diag_str;
        return nullptr;  /* compiler leaked — see note above */
    }

    auto module = action->takeModule();
    if (!module) {
        error_out = "Clang produced no LLVM module";
        return nullptr;  /* compiler leaked — see note above */
    }

    /* compiler intentionally leaked — destructor not called */
    return module;
}

/* ---- Public API ---- */

extern "C" EvalJIT* eval_jit_create(void) {
    ensure_llvm_init();

    auto builder = llvm::orc::LLJITBuilder();
    auto jit_or_err = builder.create();
    if (!jit_or_err) {
        llvm::consumeError(jit_or_err.takeError());
        return nullptr;
    }

    auto* ej = new EvalJIT();
    ej->jit = std::move(*jit_or_err);

    /* Allow JIT'd code to resolve symbols from the host process
     * (malloc, free, memcpy, etc.) */
    auto gen_or_err = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        ej->jit->getDataLayout().getGlobalPrefix());
    if (gen_or_err) {
        ej->jit->getMainJITDylib().addGenerator(std::move(*gen_or_err));
    }

    return ej;
}

extern "C" void* eval_jit_compile(EvalJIT* ej, const char* c_source,
                                   const char* symbol_name, char** error) {
    if (!ej || !c_source || !symbol_name) {
        if (error) *error = make_error("NULL argument to eval_jit_compile");
        return nullptr;
    }

    /* Get target triple from LLJIT */
    std::string triple = ej->jit->getTargetTriple().str();
    int id = ej->module_counter.fetch_add(1);

    /* Compile C to LLVM IR */
    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    std::string compile_error;
    auto module = compile_c_to_ir(*ts_ctx, c_source, triple, compile_error);
    if (!module) {
        if (error) *error = make_error(compile_error);
        return nullptr;
    }

    /* Create a unique dylib for this compilation to avoid symbol clashes */
    std::string dylib_name = "__jit_module_" + std::to_string(id);

    auto& es = ej->jit->getExecutionSession();
    auto dylib_or_err = es.createJITDylib(dylib_name);
    if (!dylib_or_err) {
        if (error) *error = make_error_from_llvm(dylib_or_err.takeError());
        return nullptr;
    }
    auto& dylib = *dylib_or_err;
    /* Link to main dylib so JIT'd code can find process symbols (malloc, etc.) */
    dylib.addToLinkOrder(ej->jit->getMainJITDylib());

    /* Add module to the new dylib */
    auto tsm = llvm::orc::ThreadSafeModule(std::move(module), std::move(ts_ctx));
    if (auto err = ej->jit->addIRModule(dylib, std::move(tsm))) {
        if (error) *error = make_error_from_llvm(std::move(err));
        return nullptr;
    }

    /* Look up the requested symbol */
    auto sym_or_err = ej->jit->lookup(dylib, symbol_name);
    if (!sym_or_err) {
        if (error) *error = make_error_from_llvm(sym_or_err.takeError());
        return nullptr;
    }
    return (void*)sym_or_err->getValue();
}

extern "C" void* eval_jit_lookup(EvalJIT* ej, const char* symbol_name,
                                  char** error) {
    if (!ej || !symbol_name) {
        if (error) *error = make_error("NULL argument to eval_jit_lookup");
        return nullptr;
    }

    auto sym_or_err = ej->jit->lookup(symbol_name);
    if (!sym_or_err) {
        if (error) *error = make_error_from_llvm(sym_or_err.takeError());
        return nullptr;
    }

    return (void*)sym_or_err->getValue();
}

extern "C" void eval_jit_destroy(EvalJIT* ej) {
    delete ej;
}
