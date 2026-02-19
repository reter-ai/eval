/*
 * _capnp_bridge.cpp — Bridge between Cap'n Proto and chibi-scheme/Eval.
 *
 * Provides:
 *   - capnp_schema type (wraps SchemaParser + ParsedSchema)
 *   - capnp_reader type (wraps FlatArrayMessageReader + backing bytes)
 *   - Bridge functions: __capnp_schema_create__, __capnp_build__,
 *     __capnp_read__, __capnp_reader_get__, __capnp_reader_field_names__,
 *     __capnp_schema_struct_names__, __capnp_save__, __capnp_mmap__,
 *     __capnp_reader_close__
 *
 * Registration: types must be registered in the same order in the
 * main Python context (_chibi_context.c), worker threads (_eval_pool.c),
 * and standalone binary (eval_main.c).
 */

#include <capnp/schema-parser.h>
#include <capnp/message.h>
#include <capnp/dynamic.h>
#include <capnp/schema.h>
#include <capnp/serialize.h>
#include <kj/filesystem.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <memory>

/* chibi-scheme headers (C linkage) */
extern "C" {
#include "chibi/eval.h"
#include "chibi/sexp.h"
#include "chibi/bignum.h"
}

/* Windows defines VOID as a macro, conflicting with capnp::DynamicValue::VOID */
#ifdef VOID
#undef VOID
#endif

/* Platform-specific mmap headers (after chibi headers to avoid macro conflicts) */
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

/* ================================================================
 * Type tags (module-level, set during registration)
 * ================================================================ */

static sexp_tag_t capnp_schema_type_tag = 0;
static sexp_tag_t capnp_reader_type_tag = 0;

/* ================================================================
 * C++ wrapper structs
 * ================================================================ */

struct EvalCapnpSchema {
    capnp::SchemaParser parser;
    capnp::ParsedSchema parsed;
    kj::Own<const kj::ReadableDirectory> dir;  /* must outlive parsed */
};

/* RAII wrapper for FILE*. Non-copyable. */
class FileHandle {
public:
    FileHandle() = default;
    explicit FileHandle(FILE* f) : f_(f) {}
    ~FileHandle() { if (f_) fclose(f_); }

    FileHandle(const FileHandle&) = delete;
    FileHandle& operator=(const FileHandle&) = delete;

    FileHandle(FileHandle&& o) noexcept : f_(o.f_) { o.f_ = nullptr; }
    FileHandle& operator=(FileHandle&& o) noexcept {
        if (this != &o) { if (f_) fclose(f_); f_ = o.f_; o.f_ = nullptr; }
        return *this;
    }

    FILE* get() const { return f_; }
    explicit operator bool() const { return f_ != nullptr; }

private:
    FILE* f_ = nullptr;
};

/* RAII wrapper for a read-only memory-mapped file.
 * Non-copyable, movable. Destructor unmaps + closes handles. */
class MmapHandle {
public:
    MmapHandle() = default;

    MmapHandle(const MmapHandle&) = delete;
    MmapHandle& operator=(const MmapHandle&) = delete;

    MmapHandle(MmapHandle&& o) noexcept
        : addr_(o.addr_), size_(o.size_)
#ifdef _WIN32
        , fileHandle_(o.fileHandle_), mappingHandle_(o.mappingHandle_)
#else
        , fd_(o.fd_)
#endif
    {
        o.addr_ = nullptr;
#ifdef _WIN32
        o.fileHandle_ = INVALID_HANDLE_VALUE;
        o.mappingHandle_ = NULL;
#else
        o.fd_ = -1;
#endif
    }

    MmapHandle& operator=(MmapHandle&& o) noexcept {
        if (this != &o) { close(); swap(o); }
        return *this;
    }

    ~MmapHandle() { close(); }

    /* Open and map a file. Returns error message on failure, nullptr on success. */
    const char* open(const char* path) {
#ifdef _WIN32
        fileHandle_ = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ,
                                  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (fileHandle_ == INVALID_HANDLE_VALUE)
            return "cannot open file";

        LARGE_INTEGER li;
        if (!GetFileSizeEx(fileHandle_, &li))
            { close(); return "cannot get file size"; }
        size_ = (size_t)li.QuadPart;

        mappingHandle_ = CreateFileMappingA(fileHandle_, NULL, PAGE_READONLY, 0, 0, NULL);
        if (!mappingHandle_)
            { close(); return "CreateFileMapping failed"; }

        addr_ = MapViewOfFile(mappingHandle_, FILE_MAP_READ, 0, 0, 0);
        if (!addr_)
            { close(); return "MapViewOfFile failed"; }
#else
        fd_ = ::open(path, O_RDONLY);
        if (fd_ < 0)
            return "cannot open file";

        struct stat st;
        if (fstat(fd_, &st) < 0)
            { close(); return "cannot stat file"; }
        size_ = (size_t)st.st_size;

        addr_ = ::mmap(NULL, size_, PROT_READ, MAP_PRIVATE, fd_, 0);
        if (addr_ == MAP_FAILED)
            { addr_ = nullptr; close(); return "mmap failed"; }
#endif
        return nullptr;  /* success */
    }

    void*  addr() const { return addr_; }
    size_t size() const { return size_; }

private:
    void close() {
#ifdef _WIN32
        if (addr_)           { UnmapViewOfFile(addr_); addr_ = nullptr; }
        if (mappingHandle_)  { CloseHandle(mappingHandle_); mappingHandle_ = NULL; }
        if (fileHandle_ != INVALID_HANDLE_VALUE) { CloseHandle(fileHandle_); fileHandle_ = INVALID_HANDLE_VALUE; }
#else
        if (addr_)           { ::munmap(addr_, size_); addr_ = nullptr; }
        if (fd_ >= 0)        { ::close(fd_); fd_ = -1; }
#endif
        size_ = 0;
    }

    void swap(MmapHandle& o) noexcept {
        std::swap(addr_, o.addr_);
        std::swap(size_, o.size_);
#ifdef _WIN32
        std::swap(fileHandle_, o.fileHandle_);
        std::swap(mappingHandle_, o.mappingHandle_);
#else
        std::swap(fd_, o.fd_);
#endif
    }

    void*  addr_ = nullptr;
    size_t size_ = 0;
#ifdef _WIN32
    HANDLE fileHandle_    = INVALID_HANDLE_VALUE;
    HANDLE mappingHandle_ = NULL;
#else
    int fd_ = -1;
#endif
};

struct EvalCapnpReader {
    kj::Array<capnp::word> words;   /* mode 1: heap-copied bytes   */
    MmapHandle mmap;                /* mode 2: memory-mapped file  */

    /* reader must be destroyed before the backing memory (words or mmap) */
    std::unique_ptr<capnp::FlatArrayMessageReader> reader;
    capnp::DynamicStruct::Reader root;
    capnp::StructSchema schema;

    /* Copy-based constructor */
    EvalCapnpReader(kj::Array<capnp::word>&& w, capnp::StructSchema s)
        : words(kj::mv(w)), schema(s)
    {
        reader = std::make_unique<capnp::FlatArrayMessageReader>(words);
        root = reader->getRoot<capnp::DynamicStruct>(schema);
    }

    /* Mmap-based constructor — takes ownership of the MmapHandle */
    EvalCapnpReader(MmapHandle&& m, capnp::StructSchema s)
        : mmap(std::move(m)), schema(s)
    {
        auto wordPtr = kj::arrayPtr(
            reinterpret_cast<const capnp::word*>(mmap.addr()),
            mmap.size() / sizeof(capnp::word));
        reader = std::make_unique<capnp::FlatArrayMessageReader>(wordPtr);
        root = reader->getRoot<capnp::DynamicStruct>(schema);
    }

    ~EvalCapnpReader() {
        reader.reset();  /* release reader before backing memory */
    }
};

/* ================================================================
 * Schema type: wraps EvalCapnpSchema*
 * ================================================================ */

static sexp capnp_schema_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalCapnpSchema* s = (EvalCapnpSchema*)sexp_cpointer_value(obj);
    if (s) {
        delete s;
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

extern "C" void register_capnp_schema_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "capnp-schema", -1);
    type = sexp_register_c_type(ctx, name, capnp_schema_finalize);

    if (sexp_typep(type)) {
        capnp_schema_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_capnp_schema(sexp ctx, EvalCapnpSchema* s) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), capnp_schema_type_tag);
    sexp_cpointer_value(result) = (void*)s;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalCapnpSchema* unwrap_capnp_schema(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == capnp_schema_type_tag)
        return (EvalCapnpSchema*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Reader type: wraps EvalCapnpReader*
 * ================================================================ */

static sexp capnp_reader_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalCapnpReader* r = (EvalCapnpReader*)sexp_cpointer_value(obj);
    if (r) {
        delete r;
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

extern "C" void register_capnp_reader_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "capnp-reader", -1);
    type = sexp_register_c_type(ctx, name, capnp_reader_finalize);

    if (sexp_typep(type)) {
        capnp_reader_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_capnp_reader(sexp ctx, EvalCapnpReader* r) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), capnp_reader_type_tag);
    sexp_cpointer_value(result) = (void*)r;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalCapnpReader* unwrap_capnp_reader(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == capnp_reader_type_tag)
        return (EvalCapnpReader*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Helpers: Convert between capnp DynamicValue and chibi sexp
 * ================================================================ */

static sexp dynamic_value_to_sexp(sexp ctx, capnp::DynamicValue::Reader val);

static sexp dynamic_value_to_sexp(sexp ctx, capnp::DynamicValue::Reader val) {
    switch (val.getType()) {
        case capnp::DynamicValue::VOID:
            return SEXP_VOID;

        case capnp::DynamicValue::BOOL:
            return val.as<bool>() ? SEXP_TRUE : SEXP_FALSE;

        case capnp::DynamicValue::INT:
            return sexp_make_integer(ctx, (sexp_sint_t)val.as<int64_t>());

        case capnp::DynamicValue::UINT:
            return sexp_make_unsigned_integer(ctx, (sexp_uint_t)val.as<uint64_t>());

        case capnp::DynamicValue::FLOAT:
            return sexp_make_flonum(ctx, val.as<double>());

        case capnp::DynamicValue::TEXT: {
            capnp::Text::Reader text = val.as<capnp::Text>();
            return sexp_c_string(ctx, text.cStr(), text.size());
        }

        case capnp::DynamicValue::DATA: {
            capnp::Data::Reader data = val.as<capnp::Data>();
            sexp bv = sexp_make_bytes(ctx, sexp_make_fixnum(data.size()), SEXP_VOID);
            memcpy(sexp_bytes_data(bv), data.begin(), data.size());
            return bv;
        }

        case capnp::DynamicValue::LIST: {
            capnp::DynamicList::Reader list = val.as<capnp::DynamicList>();
            sexp result = SEXP_NULL;
            /* Build list in reverse, then we have it in order */
            for (int i = (int)list.size() - 1; i >= 0; i--) {
                sexp_gc_var1(elem);
                sexp_gc_preserve1(ctx, elem);
                elem = dynamic_value_to_sexp(ctx, list[i]);
                result = sexp_cons(ctx, elem, result);
                sexp_gc_release1(ctx);
            }
            return result;
        }

        case capnp::DynamicValue::ENUM: {
            capnp::DynamicEnum e = val.as<capnp::DynamicEnum>();
            KJ_IF_MAYBE(enumerant, e.getEnumerant()) {
                auto name = enumerant->getProto().getName();
                return sexp_intern(ctx, name.cStr(), (int)name.size());
            }
            return sexp_make_fixnum(e.getRaw());
        }

        case capnp::DynamicValue::STRUCT: {
            /* For nested structs, we don't create a full reader object.
             * Instead convert to an alist ((field1 . val1) (field2 . val2) ...) */
            capnp::DynamicStruct::Reader s = val.as<capnp::DynamicStruct>();
            sexp result = SEXP_NULL;
            auto fields = s.getSchema().getFields();
            for (int i = (int)fields.size() - 1; i >= 0; i--) {
                auto field = fields[i];
                if (s.has(field)) {
                    sexp_gc_var3(key, fval, pair);
                    sexp_gc_preserve3(ctx, key, fval, pair);
                    auto fname = field.getProto().getName();
                    key = sexp_intern(ctx, fname.cStr(), fname.size());
                    fval = dynamic_value_to_sexp(ctx, s.get(field));
                    pair = sexp_cons(ctx, key, fval);
                    result = sexp_cons(ctx, pair, result);
                    sexp_gc_release3(ctx);
                }
            }
            return result;
        }

        default:
            return SEXP_VOID;
    }
}

/* Convert a Scheme value to a capnp DynamicValue for setting fields.
 * The field schema tells us what type is expected. */
static void set_dynamic_field(sexp ctx,
                              capnp::DynamicStruct::Builder& builder,
                              capnp::StructSchema::Field field,
                              sexp val)
{
    auto type = field.getType();

    switch (type.which()) {
        case capnp::schema::Type::VOID:
            builder.set(field, capnp::VOID);
            break;

        case capnp::schema::Type::BOOL:
            if (val == SEXP_TRUE)
                builder.set(field, true);
            else if (val == SEXP_FALSE)
                builder.set(field, false);
            else
                builder.set(field, val != SEXP_FALSE);
            break;

        case capnp::schema::Type::INT8:
        case capnp::schema::Type::INT16:
        case capnp::schema::Type::INT32:
        case capnp::schema::Type::INT64:
            if (sexp_exact_integerp(val))
                builder.set(field, (int64_t)sexp_sint_value(val));
            else if (sexp_flonump(val))
                builder.set(field, (int64_t)sexp_flonum_value(val));
            break;

        case capnp::schema::Type::UINT8:
        case capnp::schema::Type::UINT16:
        case capnp::schema::Type::UINT32:
        case capnp::schema::Type::UINT64:
            if (sexp_exact_integerp(val))
                builder.set(field, (uint64_t)sexp_uint_value(val));
            else if (sexp_flonump(val))
                builder.set(field, (uint64_t)sexp_flonum_value(val));
            break;

        case capnp::schema::Type::FLOAT32:
        case capnp::schema::Type::FLOAT64:
            builder.set(field, sexp_to_double(ctx, val));
            break;

        case capnp::schema::Type::TEXT:
            if (sexp_stringp(val))
                builder.set(field, capnp::Text::Reader(
                    sexp_string_data(val), sexp_string_size(val)));
            break;

        case capnp::schema::Type::DATA:
            if (sexp_bytesp(val))
                builder.set(field, capnp::Data::Reader(
                    (const kj::byte*)sexp_bytes_data(val),
                    sexp_bytes_length(val)));
            break;

        case capnp::schema::Type::LIST: {
            /* val should be a Scheme list; count elements first */
            if (!sexp_pairp(val) && val != SEXP_NULL) break;
            int count = 0;
            sexp p = val;
            while (sexp_pairp(p)) { count++; p = sexp_cdr(p); }

            auto listBuilder = builder.init(field, count).as<capnp::DynamicList>();
            auto listSchema = type.asList();
            auto elemType = listSchema.getElementType();

            p = val;
            for (int i = 0; i < count && sexp_pairp(p); i++, p = sexp_cdr(p)) {
                sexp elem = sexp_car(p);
                switch (elemType.which()) {
                    case capnp::schema::Type::BOOL:
                        listBuilder.set(i, elem != SEXP_FALSE);
                        break;
                    case capnp::schema::Type::INT8:
                    case capnp::schema::Type::INT16:
                    case capnp::schema::Type::INT32:
                    case capnp::schema::Type::INT64:
                        if (sexp_exact_integerp(elem))
                            listBuilder.set(i, (int64_t)sexp_sint_value(elem));
                        else if (sexp_flonump(elem))
                            listBuilder.set(i, (int64_t)sexp_flonum_value(elem));
                        break;
                    case capnp::schema::Type::UINT8:
                    case capnp::schema::Type::UINT16:
                    case capnp::schema::Type::UINT32:
                    case capnp::schema::Type::UINT64:
                        if (sexp_exact_integerp(elem))
                            listBuilder.set(i, (uint64_t)sexp_uint_value(elem));
                        else if (sexp_flonump(elem))
                            listBuilder.set(i, (uint64_t)sexp_flonum_value(elem));
                        break;
                    case capnp::schema::Type::FLOAT32:
                    case capnp::schema::Type::FLOAT64:
                        listBuilder.set(i, sexp_to_double(ctx, elem));
                        break;
                    case capnp::schema::Type::TEXT:
                        if (sexp_stringp(elem))
                            listBuilder.set(i, capnp::Text::Reader(
                                sexp_string_data(elem), sexp_string_size(elem)));
                        break;
                    case capnp::schema::Type::DATA:
                        if (sexp_bytesp(elem))
                            listBuilder.set(i, capnp::Data::Reader(
                                (const kj::byte*)sexp_bytes_data(elem),
                                sexp_bytes_length(elem)));
                        break;
                    default:
                        break;
                }
            }
            break;
        }

        case capnp::schema::Type::ENUM:
            if (sexp_symbolp(val)) {
                sexp str = sexp_symbol_to_string(ctx, val);
                if (sexp_stringp(str))
                    builder.set(field, sexp_string_data(str));
            } else if (sexp_fixnump(val)) {
                builder.set(field, (uint16_t)sexp_unbox_fixnum(val));
            }
            break;

        default:
            break;
    }
}

/* ================================================================
 * Bridge functions
 * ================================================================ */

/* __capnp_schema_create__(text) -> schema object */
static sexp bridge_capnp_schema_create(sexp ctx, sexp self, sexp_sint_t n,
                                        sexp text) {
    if (!sexp_stringp(text))
        return sexp_user_exception(ctx, self,
            "__capnp_schema_create__: expected string", text);

    try {
        EvalCapnpSchema* s = new EvalCapnpSchema();

        /* Create in-memory directory with the schema file */
        auto dir = kj::newInMemoryDirectory(kj::nullClock());
        auto path = kj::Path({"schema.capnp"});
        dir->openFile(path, kj::WriteMode::CREATE)
           ->writeAll(kj::StringPtr(sexp_string_data(text),
                                     sexp_string_size(text)));

        s->dir = kj::mv(dir);
        s->parsed = s->parser.parseFromDirectory(
            *s->dir, kj::mv(path), nullptr);

        return wrap_capnp_schema(ctx, s);
    } catch (kj::Exception& e) {
        std::string msg = "capnp schema error: ";
        msg += std::string(e.getDescription().cStr(),
                           e.getDescription().size());
        return sexp_user_exception(ctx, self, msg.c_str(), text);
    } catch (std::exception& e) {
        std::string msg = "capnp schema error: ";
        msg += e.what();
        return sexp_user_exception(ctx, self, msg.c_str(), text);
    }
}

/* __capnp_schema_struct_names__(schema) -> list of struct name strings */
static sexp bridge_capnp_schema_struct_names(sexp ctx, sexp self,
                                              sexp_sint_t n, sexp schema_sexp) {
    EvalCapnpSchema* s = unwrap_capnp_schema(schema_sexp);
    if (!s) return sexp_user_exception(ctx, self,
        "__capnp_schema_struct_names__: not a schema", schema_sexp);

    try {
        sexp result = SEXP_NULL;
        auto nested = s->parsed.getAllNested();
        for (auto node : nested) {
            auto proto = node.getProto();
            if (proto.isStruct()) {
                auto name = proto.getDisplayName();
                /* DisplayName may include "schema.capnp:" prefix; strip it */
                kj::StringPtr shortName = name;
                KJ_IF_MAYBE(colonPos, name.findFirst(':')) {
                    shortName = name.slice(*colonPos + 1);
                }
                sexp_gc_var1(str);
                sexp_gc_preserve1(ctx, str);
                str = sexp_c_string(ctx, shortName.cStr(), shortName.size());
                result = sexp_cons(ctx, str, result);
                sexp_gc_release1(ctx);
            }
        }
        return result;
    } catch (kj::Exception& e) {
        return sexp_user_exception(ctx, self, e.getDescription().cStr(),
                                   schema_sexp);
    }
}

/* __capnp_build__(schema, struct_name, field1, val1, field2, val2, ...)
 * -> bytevector (serialized Cap'n Proto message) */
static sexp bridge_capnp_build(sexp ctx, sexp self, sexp_sint_t n,
                                sexp args) {
    /* args is a rest list: (schema struct_name field1 val1 ...) */
    if (!sexp_pairp(args))
        return sexp_user_exception(ctx, self,
            "__capnp_build__: expected arguments", SEXP_NULL);

    sexp schema_sexp = sexp_car(args);
    args = sexp_cdr(args);
    if (!sexp_pairp(args))
        return sexp_user_exception(ctx, self,
            "__capnp_build__: expected struct name", SEXP_NULL);

    sexp name_sexp = sexp_car(args);
    args = sexp_cdr(args);

    EvalCapnpSchema* s = unwrap_capnp_schema(schema_sexp);
    if (!s) return sexp_user_exception(ctx, self,
        "__capnp_build__: not a schema", schema_sexp);
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_build__: struct name must be string", name_sexp);

    try {
        /* Look up struct schema by name */
        capnp::StructSchema structSchema =
            s->parsed.getNested(sexp_string_data(name_sexp)).asStruct();

        /* Create message and init root */
        capnp::MallocMessageBuilder message;
        auto builder = message.initRoot<capnp::DynamicStruct>(structSchema);

        /* Set fields from pairs: field_name, value, field_name, value, ... */
        while (sexp_pairp(args)) {
            sexp field_name = sexp_car(args);
            args = sexp_cdr(args);
            if (!sexp_pairp(args))
                return sexp_user_exception(ctx, self,
                    "__capnp_build__: missing value for field", field_name);
            sexp val = sexp_car(args);
            args = sexp_cdr(args);

            const char* fname;
            if (sexp_stringp(field_name))
                fname = sexp_string_data(field_name);
            else if (sexp_symbolp(field_name)) {
                sexp str = sexp_symbol_to_string(ctx, field_name);
                fname = sexp_string_data(str);
            } else {
                return sexp_user_exception(ctx, self,
                    "__capnp_build__: field name must be string or symbol",
                    field_name);
            }

            /* Find field in schema */
            KJ_IF_MAYBE(field, structSchema.findFieldByName(fname)) {
                set_dynamic_field(ctx, builder, *field, val);
            } else {
                return sexp_user_exception(ctx, self,
                    "__capnp_build__: unknown field", field_name);
            }
        }

        /* Serialize to flat array */
        kj::Array<capnp::word> words = capnp::messageToFlatArray(message);
        auto bytes = words.asBytes();

        /* Return as Scheme bytevector */
        sexp bv = sexp_make_bytes(ctx, sexp_make_fixnum(bytes.size()), SEXP_VOID);
        memcpy(sexp_bytes_data(bv), bytes.begin(), bytes.size());
        return bv;

    } catch (kj::Exception& e) {
        std::string msg = "capnp build error: ";
        msg += std::string(e.getDescription().cStr(),
                           e.getDescription().size());
        return sexp_user_exception(ctx, self, msg.c_str(), name_sexp);
    } catch (std::exception& e) {
        std::string msg = "capnp build error: ";
        msg += e.what();
        return sexp_user_exception(ctx, self, msg.c_str(), name_sexp);
    }
}

/* __capnp_read__(schema, struct_name, bytevector) -> reader object */
static sexp bridge_capnp_read(sexp ctx, sexp self, sexp_sint_t n,
                               sexp schema_sexp, sexp name_sexp, sexp bytes_sexp) {
    EvalCapnpSchema* s = unwrap_capnp_schema(schema_sexp);
    if (!s) return sexp_user_exception(ctx, self,
        "__capnp_read__: not a schema", schema_sexp);
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_read__: struct name must be string", name_sexp);
    if (!sexp_bytesp(bytes_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_read__: expected bytevector", bytes_sexp);

    try {
        capnp::StructSchema structSchema =
            s->parsed.getNested(sexp_string_data(name_sexp)).asStruct();

        /* Copy bytes into word-aligned array */
        size_t byte_len = sexp_bytes_length(bytes_sexp);
        size_t word_count = byte_len / sizeof(capnp::word);
        auto words = kj::heapArray<capnp::word>(word_count);
        memcpy(words.begin(), sexp_bytes_data(bytes_sexp), byte_len);

        EvalCapnpReader* reader = new EvalCapnpReader(kj::mv(words), structSchema);
        return wrap_capnp_reader(ctx, reader);

    } catch (kj::Exception& e) {
        std::string msg = "capnp read error: ";
        msg += std::string(e.getDescription().cStr(),
                           e.getDescription().size());
        return sexp_user_exception(ctx, self, msg.c_str(), name_sexp);
    } catch (std::exception& e) {
        std::string msg = "capnp read error: ";
        msg += e.what();
        return sexp_user_exception(ctx, self, msg.c_str(), name_sexp);
    }
}

/* __capnp_reader_get__(reader, field_name) -> scheme value */
static sexp bridge_capnp_reader_get(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp reader_sexp, sexp field_sexp) {
    EvalCapnpReader* r = unwrap_capnp_reader(reader_sexp);
    if (!r) return sexp_user_exception(ctx, self,
        "__capnp_reader_get__: not a reader", reader_sexp);

    const char* fname;
    if (sexp_stringp(field_sexp))
        fname = sexp_string_data(field_sexp);
    else if (sexp_symbolp(field_sexp)) {
        sexp str = sexp_symbol_to_string(ctx, field_sexp);
        fname = sexp_string_data(str);
    } else {
        return sexp_user_exception(ctx, self,
            "__capnp_reader_get__: field name must be string or symbol",
            field_sexp);
    }

    try {
        auto val = r->root.get(fname);
        return dynamic_value_to_sexp(ctx, val);
    } catch (kj::Exception& e) {
        std::string msg = "capnp reader error: ";
        msg += std::string(e.getDescription().cStr(),
                           e.getDescription().size());
        return sexp_user_exception(ctx, self, msg.c_str(), field_sexp);
    }
}

/* __capnp_reader_field_names__(reader) -> list of field name strings */
static sexp bridge_capnp_reader_field_names(sexp ctx, sexp self,
                                             sexp_sint_t n, sexp reader_sexp) {
    EvalCapnpReader* r = unwrap_capnp_reader(reader_sexp);
    if (!r) return sexp_user_exception(ctx, self,
        "__capnp_reader_field_names__: not a reader", reader_sexp);

    sexp result = SEXP_NULL;
    auto fields = r->schema.getFields();
    for (int i = (int)fields.size() - 1; i >= 0; i--) {
        auto name = fields[i].getProto().getName();
        sexp_gc_var1(str);
        sexp_gc_preserve1(ctx, str);
        str = sexp_c_string(ctx, name.cStr(), name.size());
        result = sexp_cons(ctx, str, result);
        sexp_gc_release1(ctx);
    }
    return result;
}

/* __capnp_save__(filename, bytevector) -> void */
static sexp bridge_capnp_save(sexp ctx, sexp self, sexp_sint_t n,
                               sexp filename_sexp, sexp bytes_sexp) {
    if (!sexp_stringp(filename_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_save__: expected string filename", filename_sexp);
    if (!sexp_bytesp(bytes_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_save__: expected bytevector", bytes_sexp);

    const char* path = sexp_string_data(filename_sexp);
    FileHandle fh(fopen(path, "wb"));
    if (!fh) {
        std::string msg = "capnp save: cannot open file: ";
        msg += path;
        return sexp_user_exception(ctx, self, msg.c_str(), filename_sexp);
    }

    size_t len = sexp_bytes_length(bytes_sexp);
    size_t written = fwrite(sexp_bytes_data(bytes_sexp), 1, len, fh.get());
    if (written != len) {
        std::string msg = "capnp save: write error: ";
        msg += path;
        return sexp_user_exception(ctx, self, msg.c_str(), filename_sexp);
    }

    return SEXP_VOID;
}

/* __capnp_mmap__(schema, struct_name, filename) -> reader object */
static sexp bridge_capnp_mmap(sexp ctx, sexp self, sexp_sint_t n,
                               sexp schema_sexp, sexp name_sexp,
                               sexp filename_sexp) {
    EvalCapnpSchema* s = unwrap_capnp_schema(schema_sexp);
    if (!s) return sexp_user_exception(ctx, self,
        "__capnp_mmap__: not a schema", schema_sexp);
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_mmap__: struct name must be string", name_sexp);
    if (!sexp_stringp(filename_sexp))
        return sexp_user_exception(ctx, self,
            "__capnp_mmap__: expected string filename", filename_sexp);

    const char* path = sexp_string_data(filename_sexp);

    try {
        capnp::StructSchema structSchema =
            s->parsed.getNested(sexp_string_data(name_sexp)).asStruct();

        MmapHandle mh;
        if (const char* err = mh.open(path)) {
            std::string msg = "capnp mmap: ";
            msg += err;
            msg += ": ";
            msg += path;
            return sexp_user_exception(ctx, self, msg.c_str(), filename_sexp);
        }

        EvalCapnpReader* reader = new EvalCapnpReader(std::move(mh), structSchema);
        return wrap_capnp_reader(ctx, reader);

    } catch (kj::Exception& e) {
        std::string msg = "capnp mmap error: ";
        msg += std::string(e.getDescription().cStr(),
                           e.getDescription().size());
        return sexp_user_exception(ctx, self, msg.c_str(), filename_sexp);
    } catch (std::exception& e) {
        std::string msg = "capnp mmap error: ";
        msg += e.what();
        return sexp_user_exception(ctx, self, msg.c_str(), filename_sexp);
    }
}

/* __capnp_reader_close__(reader) -> void
 * Explicitly release the reader and its backing memory (mmap or heap).
 * Safe to call multiple times — second call is a no-op. */
static sexp bridge_capnp_reader_close(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp reader_sexp) {
    EvalCapnpReader* r = unwrap_capnp_reader(reader_sexp);
    if (!r) return SEXP_VOID;  /* already closed or not a reader */
    delete r;
    sexp_cpointer_value(reader_sexp) = NULL;
    return SEXP_VOID;
}

/* ================================================================
 * Registration
 * ================================================================ */

extern "C" void register_capnp_bridge_functions(sexp ctx, sexp env) {
    sexp_define_foreign(ctx, env, "__capnp_schema_create__", 1,
                        (sexp_proc1)bridge_capnp_schema_create);
    sexp_define_foreign(ctx, env, "__capnp_schema_struct_names__", 1,
                        (sexp_proc1)bridge_capnp_schema_struct_names);
    sexp_define_foreign_proc_rest(ctx, env, "__capnp_build__", 0,
                                  (sexp_proc1)bridge_capnp_build);
    sexp_define_foreign(ctx, env, "__capnp_read__", 3,
                        (sexp_proc1)bridge_capnp_read);
    sexp_define_foreign(ctx, env, "__capnp_reader_get__", 2,
                        (sexp_proc1)bridge_capnp_reader_get);
    sexp_define_foreign(ctx, env, "__capnp_reader_field_names__", 1,
                        (sexp_proc1)bridge_capnp_reader_field_names);
    sexp_define_foreign(ctx, env, "__capnp_save__", 2,
                        (sexp_proc1)bridge_capnp_save);
    sexp_define_foreign(ctx, env, "__capnp_mmap__", 3,
                        (sexp_proc1)bridge_capnp_mmap);
    sexp_define_foreign(ctx, env, "__capnp_reader_close__", 1,
                        (sexp_proc1)bridge_capnp_reader_close);
}
