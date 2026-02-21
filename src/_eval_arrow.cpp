/*
 * _eval_arrow.cpp — Bridge between Apache Arrow and chibi-scheme/Eval.
 *
 * Provides:
 *   - arrow_table type (wraps shared_ptr<arrow::Table>)
 *   - arrow_array type (wraps shared_ptr<arrow::ChunkedArray>)
 *   - ~40 bridge functions for table/array creation, I/O, compute, group-by
 *
 * Registration: types must be registered in the same order in the
 * main Python context (_chibi_context.c), worker threads (_eval_pool.c),
 * and standalone binary (eval_main.c).
 */

#ifdef EVAL_HAVE_ARROW

#include <arrow/api.h>
#include <arrow/csv/api.h>
#include <arrow/io/api.h>
#include <arrow/ipc/api.h>
#include <arrow/compute/api.h>
#include <arrow/compute/initialize.h>

#ifdef EVAL_HAVE_PARQUET
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <algorithm>
#include <iomanip>

/* chibi-scheme headers (C linkage) */
extern "C" {
#include "chibi/eval.h"
#include "chibi/sexp.h"
#include "chibi/bignum.h"
}

/* ================================================================
 * Type tags (module-level, set during registration)
 * ================================================================ */

static sexp_tag_t arrow_table_type_tag = 0;
static sexp_tag_t arrow_array_type_tag = 0;

/* ================================================================
 * C++ wrapper structs
 * ================================================================ */

struct EvalArrowTable {
    std::shared_ptr<arrow::Table> table;
};

struct EvalArrowArray {
    std::shared_ptr<arrow::ChunkedArray> array;
    std::string name;
};

/* ================================================================
 * Table type: wraps EvalArrowTable*
 * ================================================================ */

static sexp arrow_table_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalArrowTable* t = (EvalArrowTable*)sexp_cpointer_value(obj);
    if (t) {
        delete t;
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

extern "C" void register_arrow_table_type(sexp ctx) {
    /* Initialize Arrow compute kernels (safe to call multiple times) */
    (void)arrow::compute::Initialize();

    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "arrow-table", -1);
    type = sexp_register_c_type(ctx, name, arrow_table_finalize);

    if (sexp_typep(type)) {
        arrow_table_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_arrow_table(sexp ctx, std::shared_ptr<arrow::Table> tbl) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    EvalArrowTable* wrapper = new EvalArrowTable{tbl};
    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), arrow_table_type_tag);
    sexp_cpointer_value(result) = (void*)wrapper;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalArrowTable* unwrap_arrow_table(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == arrow_table_type_tag)
        return (EvalArrowTable*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Array type: wraps EvalArrowArray*
 * ================================================================ */

static sexp arrow_array_finalize(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    EvalArrowArray* a = (EvalArrowArray*)sexp_cpointer_value(obj);
    if (a) {
        delete a;
        sexp_cpointer_value(obj) = NULL;
    }
    return SEXP_VOID;
}

extern "C" void register_arrow_array_type(sexp ctx) {
    sexp_gc_var2(name, type);
    sexp_gc_preserve2(ctx, name, type);

    name = sexp_c_string(ctx, "arrow-array", -1);
    type = sexp_register_c_type(ctx, name, arrow_array_finalize);

    if (sexp_typep(type)) {
        arrow_array_type_tag = sexp_type_tag(type);
        sexp_preserve_object(ctx, type);
    }

    sexp_gc_release2(ctx);
}

static sexp wrap_arrow_array(sexp ctx, std::shared_ptr<arrow::ChunkedArray> arr,
                             const std::string& name = "") {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    EvalArrowArray* wrapper = new EvalArrowArray{arr, name};
    result = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), arrow_array_type_tag);
    sexp_cpointer_value(result) = (void*)wrapper;
    sexp_cpointer_length(result) = 0;

    sexp_gc_release1(ctx);
    return result;
}

static EvalArrowArray* unwrap_arrow_array(sexp x) {
    if (sexp_pointerp(x) && sexp_pointer_tag(x) == arrow_array_type_tag)
        return (EvalArrowArray*)sexp_cpointer_value(x);
    return NULL;
}

/* ================================================================
 * Type predicates
 * ================================================================ */

static sexp bridge_arrow_table_p(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    (void)ctx; (void)self; (void)n;
    return unwrap_arrow_table(obj) ? SEXP_TRUE : SEXP_FALSE;
}

static sexp bridge_arrow_array_p(sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    (void)ctx; (void)self; (void)n;
    return unwrap_arrow_array(obj) ? SEXP_TRUE : SEXP_FALSE;
}

/* ================================================================
 * Helpers: Arrow scalar → sexp
 * ================================================================ */

static sexp arrow_scalar_to_sexp(sexp ctx, const std::shared_ptr<arrow::Scalar>& scalar) {
    if (!scalar || !scalar->is_valid) return SEXP_FALSE;

    switch (scalar->type->id()) {
        case arrow::Type::INT8:
            return sexp_make_fixnum(std::static_pointer_cast<arrow::Int8Scalar>(scalar)->value);
        case arrow::Type::INT16:
            return sexp_make_fixnum(std::static_pointer_cast<arrow::Int16Scalar>(scalar)->value);
        case arrow::Type::INT32:
            return sexp_make_fixnum(std::static_pointer_cast<arrow::Int32Scalar>(scalar)->value);
        case arrow::Type::INT64:
            return sexp_make_integer(ctx,
                (long long)std::static_pointer_cast<arrow::Int64Scalar>(scalar)->value);
        case arrow::Type::UINT8:
            return sexp_make_fixnum(std::static_pointer_cast<arrow::UInt8Scalar>(scalar)->value);
        case arrow::Type::UINT16:
            return sexp_make_fixnum(std::static_pointer_cast<arrow::UInt16Scalar>(scalar)->value);
        case arrow::Type::UINT32:
            return sexp_make_integer(ctx,
                (long long)std::static_pointer_cast<arrow::UInt32Scalar>(scalar)->value);
        case arrow::Type::UINT64:
            return sexp_make_integer(ctx,
                (long long)std::static_pointer_cast<arrow::UInt64Scalar>(scalar)->value);
        case arrow::Type::FLOAT:
            return sexp_make_flonum(ctx,
                (double)std::static_pointer_cast<arrow::FloatScalar>(scalar)->value);
        case arrow::Type::DOUBLE:
            return sexp_make_flonum(ctx,
                std::static_pointer_cast<arrow::DoubleScalar>(scalar)->value);
        case arrow::Type::STRING: {
            auto s = std::static_pointer_cast<arrow::StringScalar>(scalar);
            return sexp_c_string(ctx, (const char*)s->value->data(),
                                 (int)s->value->size());
        }
        case arrow::Type::BOOL:
            return std::static_pointer_cast<arrow::BooleanScalar>(scalar)->value
                   ? SEXP_TRUE : SEXP_FALSE;
        default:
            return SEXP_FALSE;
    }
}

/* Get element from a ChunkedArray at a flat index */
static sexp chunked_array_ref(sexp ctx, const std::shared_ptr<arrow::ChunkedArray>& arr,
                               int64_t index) {
    int64_t offset = 0;
    for (int c = 0; c < arr->num_chunks(); c++) {
        auto chunk = arr->chunk(c);
        if (index < offset + chunk->length()) {
            auto scalar = chunk->GetScalar(index - offset);
            if (scalar.ok()) return arrow_scalar_to_sexp(ctx, *scalar);
            return SEXP_FALSE;
        }
        offset += chunk->length();
    }
    return SEXP_FALSE;
}

/* Detect column type from a Scheme list of values */
enum ColType { COL_INT, COL_FLOAT, COL_STRING, COL_BOOL };

static ColType infer_column_type(sexp vals) {
    ColType type = COL_INT;
    for (sexp p = vals; sexp_pairp(p); p = sexp_cdr(p)) {
        sexp v = sexp_car(p);
        if (v == SEXP_NULL || v == SEXP_FALSE || v == SEXP_TRUE) {
            if (v == SEXP_TRUE) {
                /* Could be boolean, but only if all are bool */
            }
            continue;
        }
        if (sexp_stringp(v)) return COL_STRING;
        if (sexp_flonump(v)) type = COL_FLOAT;
        /* fixnum is compatible with both int and float */
    }
    return type;
}

/* ================================================================
 * Table creation from alist
 * ================================================================ */

static sexp bridge_arrow_make_table(sexp ctx, sexp self, sexp_sint_t n, sexp arg) {
    (void)self; (void)n;

    if (!sexp_listp(ctx, arg)) {
        return sexp_user_exception(ctx, self, "expected alist", arg);
    }

    try {
        std::vector<std::shared_ptr<arrow::Field>> fields;
        std::vector<std::shared_ptr<arrow::ChunkedArray>> columns;

        for (sexp p = arg; sexp_pairp(p); p = sexp_cdr(p)) {
            sexp pair = sexp_car(p);
            if (!sexp_pairp(pair)) continue;
            sexp name_sexp = sexp_car(pair);
            sexp vals = sexp_cdr(pair);

            std::string col_name;
            if (sexp_stringp(name_sexp)) {
                col_name = std::string(sexp_string_data(name_sexp),
                                       sexp_string_size(name_sexp));
            } else if (sexp_symbolp(name_sexp)) {
                sexp sym_str = sexp_symbol_to_string(ctx, name_sexp);
                col_name = std::string(sexp_string_data(sym_str),
                                       sexp_string_size(sym_str));
            } else {
                continue;
            }

            ColType ct = infer_column_type(vals);

            if (ct == COL_STRING) {
                arrow::StringBuilder builder;
                for (sexp q = vals; sexp_pairp(q); q = sexp_cdr(q)) {
                    sexp v = sexp_car(q);
                    if (v == SEXP_NULL || v == SEXP_FALSE) {
                        (void)builder.AppendNull();
                    } else if (sexp_stringp(v)) {
                        (void)builder.Append(
                            std::string(sexp_string_data(v), sexp_string_size(v)));
                    } else if (sexp_fixnump(v)) {
                        (void)builder.Append(std::to_string(sexp_unbox_fixnum(v)));
                    } else if (sexp_flonump(v)) {
                        (void)builder.Append(std::to_string(sexp_flonum_value(v)));
                    } else {
                        (void)builder.AppendNull();
                    }
                }
                auto result = builder.Finish();
                if (!result.ok())
                    return sexp_user_exception(ctx, self, "string column build failed", name_sexp);
                fields.push_back(arrow::field(col_name, arrow::utf8()));
                columns.push_back(std::make_shared<arrow::ChunkedArray>(*result));
            } else if (ct == COL_FLOAT) {
                arrow::DoubleBuilder builder;
                for (sexp q = vals; sexp_pairp(q); q = sexp_cdr(q)) {
                    sexp v = sexp_car(q);
                    if (v == SEXP_NULL || v == SEXP_FALSE) {
                        (void)builder.AppendNull();
                    } else if (sexp_flonump(v)) {
                        (void)builder.Append(sexp_flonum_value(v));
                    } else if (sexp_fixnump(v)) {
                        (void)builder.Append((double)sexp_unbox_fixnum(v));
                    } else {
                        (void)builder.AppendNull();
                    }
                }
                auto result = builder.Finish();
                if (!result.ok())
                    return sexp_user_exception(ctx, self, "float column build failed", name_sexp);
                fields.push_back(arrow::field(col_name, arrow::float64()));
                columns.push_back(std::make_shared<arrow::ChunkedArray>(*result));
            } else {
                /* COL_INT */
                arrow::Int64Builder builder;
                for (sexp q = vals; sexp_pairp(q); q = sexp_cdr(q)) {
                    sexp v = sexp_car(q);
                    if (v == SEXP_NULL || v == SEXP_FALSE) {
                        (void)builder.AppendNull();
                    } else if (sexp_fixnump(v)) {
                        (void)builder.Append((int64_t)sexp_unbox_fixnum(v));
                    } else if (sexp_flonump(v)) {
                        (void)builder.Append((int64_t)sexp_flonum_value(v));
                    } else {
                        (void)builder.AppendNull();
                    }
                }
                auto result = builder.Finish();
                if (!result.ok())
                    return sexp_user_exception(ctx, self, "int column build failed", name_sexp);
                fields.push_back(arrow::field(col_name, arrow::int64()));
                columns.push_back(std::make_shared<arrow::ChunkedArray>(*result));
            }
        }

        auto schema = arrow::schema(fields);
        auto table = arrow::Table::Make(schema, columns);
        return wrap_arrow_table(ctx, table);

    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arg);
    }
}

/* ================================================================
 * File I/O
 * ================================================================ */

/* __arrow_read_csv__(path) -> table */
static sexp bridge_arrow_read_csv(sexp ctx, sexp self, sexp_sint_t n, sexp path_sexp) {
    (void)n;
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto input_result = arrow::io::ReadableFile::Open(path);
        if (!input_result.ok())
            return sexp_user_exception(ctx, self, input_result.status().ToString().c_str(), path_sexp);

        auto read_options = arrow::csv::ReadOptions::Defaults();
        auto parse_options = arrow::csv::ParseOptions::Defaults();
        auto convert_options = arrow::csv::ConvertOptions::Defaults();

        auto reader_result = arrow::csv::TableReader::Make(
            arrow::io::default_io_context(), *input_result,
            read_options, parse_options, convert_options);
        if (!reader_result.ok())
            return sexp_user_exception(ctx, self, reader_result.status().ToString().c_str(), path_sexp);

        auto table_result = (*reader_result)->Read();
        if (!table_result.ok())
            return sexp_user_exception(ctx, self, table_result.status().ToString().c_str(), path_sexp);

        return wrap_arrow_table(ctx, *table_result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
}

/* __arrow_write_csv__(table, path) -> void */
static sexp bridge_arrow_write_csv(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp table_sexp, sexp path_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto output_result = arrow::io::FileOutputStream::Open(path);
        if (!output_result.ok())
            return sexp_user_exception(ctx, self, output_result.status().ToString().c_str(), path_sexp);

        auto write_options = arrow::csv::WriteOptions::Defaults();
        auto status = arrow::csv::WriteCSV(*t->table, write_options, output_result->get());
        if (!status.ok())
            return sexp_user_exception(ctx, self, status.ToString().c_str(), path_sexp);

        return SEXP_VOID;
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
}

/* __arrow_read_parquet__(path) -> table */
static sexp bridge_arrow_read_parquet(sexp ctx, sexp self, sexp_sint_t n, sexp path_sexp) {
    (void)n;
#ifdef EVAL_HAVE_PARQUET
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto input_result = arrow::io::ReadableFile::Open(path);
        if (!input_result.ok())
            return sexp_user_exception(ctx, self, "cannot open parquet file", path_sexp);

        auto reader_result = parquet::arrow::OpenFile(*input_result, arrow::default_memory_pool());
        if (!reader_result.ok())
            return sexp_user_exception(ctx, self, "cannot open parquet reader", path_sexp);

        std::shared_ptr<arrow::Table> table;
        auto status = (*reader_result)->ReadTable(&table);
        if (!status.ok())
            return sexp_user_exception(ctx, self, "cannot read parquet table", path_sexp);

        return wrap_arrow_table(ctx, table);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
#else
    return sexp_user_exception(ctx, self, "Parquet support not compiled", path_sexp);
#endif
}

/* __arrow_write_parquet__(table, path) -> void */
static sexp bridge_arrow_write_parquet(sexp ctx, sexp self, sexp_sint_t n,
                                        sexp table_sexp, sexp path_sexp) {
    (void)n;
#ifdef EVAL_HAVE_PARQUET
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto output_result = arrow::io::FileOutputStream::Open(path);
        if (!output_result.ok())
            return sexp_user_exception(ctx, self, output_result.status().ToString().c_str(), path_sexp);

        auto status = parquet::arrow::WriteTable(*t->table, arrow::default_memory_pool(),
                                                  *output_result, t->table->num_rows());
        if (!status.ok())
            return sexp_user_exception(ctx, self, status.ToString().c_str(), path_sexp);

        return SEXP_VOID;
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
#else
    return sexp_user_exception(ctx, self, "Parquet support not compiled", table_sexp);
#endif
}

/* __arrow_read_ipc__(path) -> table */
static sexp bridge_arrow_read_ipc(sexp ctx, sexp self, sexp_sint_t n, sexp path_sexp) {
    (void)n;
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto input_result = arrow::io::ReadableFile::Open(path);
        if (!input_result.ok())
            return sexp_user_exception(ctx, self, input_result.status().ToString().c_str(), path_sexp);

        auto reader_result = arrow::ipc::RecordBatchFileReader::Open(*input_result);
        if (!reader_result.ok())
            return sexp_user_exception(ctx, self, reader_result.status().ToString().c_str(), path_sexp);

        auto reader = *reader_result;
        std::vector<std::shared_ptr<arrow::RecordBatch>> batches;
        for (int i = 0; i < reader->num_record_batches(); i++) {
            auto batch_result = reader->ReadRecordBatch(i);
            if (batch_result.ok()) batches.push_back(*batch_result);
        }

        auto table_result = arrow::Table::FromRecordBatches(batches);
        if (!table_result.ok())
            return sexp_user_exception(ctx, self, table_result.status().ToString().c_str(), path_sexp);

        return wrap_arrow_table(ctx, *table_result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
}

/* __arrow_write_ipc__(table, path) -> void */
static sexp bridge_arrow_write_ipc(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp table_sexp, sexp path_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(path_sexp))
        return sexp_user_exception(ctx, self, "expected string path", path_sexp);

    try {
        std::string path(sexp_string_data(path_sexp), sexp_string_size(path_sexp));
        auto output_result = arrow::io::FileOutputStream::Open(path);
        if (!output_result.ok())
            return sexp_user_exception(ctx, self, output_result.status().ToString().c_str(), path_sexp);

        auto writer_result = arrow::ipc::MakeFileWriter(output_result->get(), t->table->schema());
        if (!writer_result.ok())
            return sexp_user_exception(ctx, self, writer_result.status().ToString().c_str(), path_sexp);

        auto writer = *writer_result;
        auto batches = t->table->CombineChunksToBatch();
        if (!batches.ok())
            return sexp_user_exception(ctx, self, batches.status().ToString().c_str(), path_sexp);

        auto status = writer->WriteRecordBatch(**batches);
        if (!status.ok())
            return sexp_user_exception(ctx, self, status.ToString().c_str(), path_sexp);

        status = writer->Close();
        if (!status.ok())
            return sexp_user_exception(ctx, self, status.ToString().c_str(), path_sexp);

        return SEXP_VOID;
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), path_sexp);
    }
}

/* ================================================================
 * Table properties
 * ================================================================ */

/* __arrow_table_num_rows__(table) -> int */
static sexp bridge_arrow_table_num_rows(sexp ctx, sexp self, sexp_sint_t n, sexp table_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    return sexp_make_integer(ctx, (long long)t->table->num_rows());
}

/* __arrow_table_num_columns__(table) -> int */
static sexp bridge_arrow_table_num_columns(sexp ctx, sexp self, sexp_sint_t n, sexp table_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    return sexp_make_fixnum(t->table->num_columns());
}

/* __arrow_table_column_names__(table) -> list of strings */
static sexp bridge_arrow_table_column_names(sexp ctx, sexp self, sexp_sint_t n, sexp table_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);

    sexp result = SEXP_NULL;
    auto names = t->table->ColumnNames();
    for (int i = (int)names.size() - 1; i >= 0; i--) {
        sexp_gc_var1(str);
        sexp_gc_preserve1(ctx, str);
        str = sexp_c_string(ctx, names[i].c_str(), (int)names[i].size());
        result = sexp_cons(ctx, str, result);
        sexp_gc_release1(ctx);
    }
    return result;
}

/* __arrow_table_schema__(table) -> list of (name . type-string) */
static sexp bridge_arrow_table_schema(sexp ctx, sexp self, sexp_sint_t n, sexp table_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);

    sexp result = SEXP_NULL;
    auto schema = t->table->schema();
    for (int i = schema->num_fields() - 1; i >= 0; i--) {
        auto field = schema->field(i);
        sexp_gc_var3(key, val, pair);
        sexp_gc_preserve3(ctx, key, val, pair);
        key = sexp_c_string(ctx, field->name().c_str(), (int)field->name().size());
        std::string type_str = field->type()->ToString();
        val = sexp_c_string(ctx, type_str.c_str(), (int)type_str.size());
        pair = sexp_cons(ctx, key, val);
        result = sexp_cons(ctx, pair, result);
        sexp_gc_release3(ctx);
    }
    return result;
}

/* __arrow_table_column__(table, name_or_index) -> arrow-array */
static sexp bridge_arrow_table_column(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp table_sexp, sexp name_or_idx) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);

    std::shared_ptr<arrow::ChunkedArray> col;
    std::string col_name;

    if (sexp_stringp(name_or_idx)) {
        std::string name(sexp_string_data(name_or_idx), sexp_string_size(name_or_idx));
        col = t->table->GetColumnByName(name);
        col_name = name;
        if (!col)
            return sexp_user_exception(ctx, self, "column not found", name_or_idx);
    } else if (sexp_fixnump(name_or_idx)) {
        int idx = (int)sexp_unbox_fixnum(name_or_idx);
        if (idx < 0 || idx >= t->table->num_columns())
            return sexp_user_exception(ctx, self, "column index out of range", name_or_idx);
        col = t->table->column(idx);
        col_name = t->table->schema()->field(idx)->name();
    } else {
        return sexp_user_exception(ctx, self, "expected string or fixnum", name_or_idx);
    }

    return wrap_arrow_array(ctx, col, col_name);
}

/* ================================================================
 * Table operations
 * ================================================================ */

/* __arrow_table_select__(table, names_list) -> new table */
static sexp bridge_arrow_table_select(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp table_sexp, sexp names_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);

    try {
        std::vector<std::string> names;
        for (sexp p = names_sexp; sexp_pairp(p); p = sexp_cdr(p)) {
            sexp s = sexp_car(p);
            if (sexp_stringp(s))
                names.push_back(std::string(sexp_string_data(s), sexp_string_size(s)));
        }

        std::vector<std::shared_ptr<arrow::Field>> fields;
        std::vector<std::shared_ptr<arrow::ChunkedArray>> columns;
        for (auto& name : names) {
            int idx = t->table->schema()->GetFieldIndex(name);
            if (idx >= 0) {
                fields.push_back(t->table->schema()->field(idx));
                columns.push_back(t->table->column(idx));
            }
        }

        auto schema = arrow::schema(fields);
        auto result = arrow::Table::Make(schema, columns);
        return wrap_arrow_table(ctx, result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_drop__(table, name) -> table without column */
static sexp bridge_arrow_table_drop(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp table_sexp, sexp name_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self, "expected string", name_sexp);

    try {
        std::string name(sexp_string_data(name_sexp), sexp_string_size(name_sexp));
        int idx = t->table->schema()->GetFieldIndex(name);
        if (idx < 0)
            return sexp_user_exception(ctx, self, "column not found", name_sexp);
        auto result = t->table->RemoveColumn(idx);
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), name_sexp);
        return wrap_arrow_table(ctx, *result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_filter__(table, col_name, op_symbol, value) -> filtered table */
static sexp bridge_arrow_table_filter(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp table_sexp, sexp col_sexp,
                                       sexp op_sexp, sexp val_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(col_sexp))
        return sexp_user_exception(ctx, self, "expected string column name", col_sexp);

    try {
        std::string col_name(sexp_string_data(col_sexp), sexp_string_size(col_sexp));
        auto col = t->table->GetColumnByName(col_name);
        if (!col)
            return sexp_user_exception(ctx, self, "column not found", col_sexp);

        /* Determine comparison function name */
        std::string op_str;
        if (sexp_symbolp(op_sexp)) {
            sexp sym_str = sexp_symbol_to_string(ctx, op_sexp);
            op_str = std::string(sexp_string_data(sym_str), sexp_string_size(sym_str));
        } else if (sexp_stringp(op_sexp)) {
            op_str = std::string(sexp_string_data(op_sexp), sexp_string_size(op_sexp));
        } else {
            return sexp_user_exception(ctx, self, "expected symbol or string operator", op_sexp);
        }

        std::string func_name;
        if (op_str == ">") func_name = "greater";
        else if (op_str == "<") func_name = "less";
        else if (op_str == ">=") func_name = "greater_equal";
        else if (op_str == "<=") func_name = "less_equal";
        else if (op_str == "==" || op_str == "=") func_name = "equal";
        else if (op_str == "!=") func_name = "not_equal";
        else return sexp_user_exception(ctx, self, "unknown comparison operator", op_sexp);

        /* Build scalar for comparison */
        std::shared_ptr<arrow::Scalar> scalar;
        if (sexp_fixnump(val_sexp)) {
            scalar = std::make_shared<arrow::Int64Scalar>((int64_t)sexp_unbox_fixnum(val_sexp));
        } else if (sexp_flonump(val_sexp)) {
            scalar = std::make_shared<arrow::DoubleScalar>(sexp_flonum_value(val_sexp));
        } else if (sexp_stringp(val_sexp)) {
            scalar = std::make_shared<arrow::StringScalar>(
                std::string(sexp_string_data(val_sexp), sexp_string_size(val_sexp)));
        } else {
            return sexp_user_exception(ctx, self, "unsupported filter value type", val_sexp);
        }

        /* Cast scalar to match column type if needed */
        auto col_type = col->type();
        if (!scalar->type->Equals(col_type)) {
            auto cast_result = scalar->CastTo(col_type);
            if (cast_result.ok()) scalar = *cast_result;
        }

        /* Compare using CallFunction */
        auto mask_result = arrow::compute::CallFunction(
            func_name, {arrow::Datum(col), arrow::Datum(scalar)});
        if (!mask_result.ok())
            return sexp_user_exception(ctx, self, "comparison failed", col_sexp);

        /* Filter */
        auto filter_result = arrow::compute::Filter(
            arrow::Datum(t->table), mask_result.ValueOrDie());
        if (!filter_result.ok())
            return sexp_user_exception(ctx, self, "filter failed", col_sexp);

        return wrap_arrow_table(ctx, filter_result->table());
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_sort__(table, col_name, direction) -> sorted table */
static sexp bridge_arrow_table_sort(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp table_sexp, sexp col_sexp, sexp dir_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(col_sexp))
        return sexp_user_exception(ctx, self, "expected string column name", col_sexp);

    try {
        std::string col_name(sexp_string_data(col_sexp), sexp_string_size(col_sexp));

        arrow::compute::SortOrder order = arrow::compute::SortOrder::Ascending;
        if (sexp_stringp(dir_sexp)) {
            std::string dir(sexp_string_data(dir_sexp), sexp_string_size(dir_sexp));
            if (dir == "desc" || dir == "descending" || dir == "DESC")
                order = arrow::compute::SortOrder::Descending;
        }

        arrow::compute::SortOptions sort_options(
            {arrow::compute::SortKey(col_name, order)});
        auto indices_result = arrow::compute::SortIndices(t->table, sort_options);
        if (!indices_result.ok())
            return sexp_user_exception(ctx, self, indices_result.status().ToString().c_str(), col_sexp);

        auto take_result = arrow::compute::Take(t->table, *indices_result);
        if (!take_result.ok())
            return sexp_user_exception(ctx, self, take_result.status().ToString().c_str(), col_sexp);

        return wrap_arrow_table(ctx, take_result->table());
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_head__(table, n) -> first n rows */
static sexp bridge_arrow_table_head(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp table_sexp, sexp count_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_fixnump(count_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum", count_sexp);

    int64_t count = sexp_unbox_fixnum(count_sexp);
    if (count > t->table->num_rows()) count = t->table->num_rows();
    return wrap_arrow_table(ctx, t->table->Slice(0, count));
}

/* __arrow_table_tail__(table, n) -> last n rows */
static sexp bridge_arrow_table_tail(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp table_sexp, sexp count_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_fixnump(count_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum", count_sexp);

    int64_t count = sexp_unbox_fixnum(count_sexp);
    int64_t rows = t->table->num_rows();
    if (count > rows) count = rows;
    return wrap_arrow_table(ctx, t->table->Slice(rows - count, count));
}

/* __arrow_table_slice__(table, offset, length) -> sliced table */
static sexp bridge_arrow_table_slice(sexp ctx, sexp self, sexp_sint_t n,
                                      sexp table_sexp, sexp offset_sexp, sexp length_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_fixnump(offset_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum offset", offset_sexp);
    if (!sexp_fixnump(length_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum length", length_sexp);

    int64_t offset = sexp_unbox_fixnum(offset_sexp);
    int64_t length = sexp_unbox_fixnum(length_sexp);
    return wrap_arrow_table(ctx, t->table->Slice(offset, length));
}

/* __arrow_table_add_column__(table, name, array) -> new table */
static sexp bridge_arrow_table_add_column(sexp ctx, sexp self, sexp_sint_t n,
                                           sexp table_sexp, sexp name_sexp, sexp arr_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(name_sexp))
        return sexp_user_exception(ctx, self, "expected string", name_sexp);
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    try {
        std::string name(sexp_string_data(name_sexp), sexp_string_size(name_sexp));
        auto field = arrow::field(name, a->array->type());
        auto result = t->table->AddColumn(t->table->num_columns(), field, a->array);
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), name_sexp);
        return wrap_arrow_table(ctx, *result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_remove_column__(table, name) -> table without column */
static sexp bridge_arrow_table_remove_column(sexp ctx, sexp self, sexp_sint_t n,
                                              sexp table_sexp, sexp name_sexp) {
    /* Same as drop */
    return bridge_arrow_table_drop(ctx, self, n, table_sexp, name_sexp);
}

/* __arrow_table_rename__(table, old_name, new_name) -> renamed table */
static sexp bridge_arrow_table_rename(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp table_sexp, sexp old_name_sexp, sexp new_name_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(old_name_sexp) || !sexp_stringp(new_name_sexp))
        return sexp_user_exception(ctx, self, "expected strings", old_name_sexp);

    try {
        std::string old_name(sexp_string_data(old_name_sexp), sexp_string_size(old_name_sexp));
        std::string new_name(sexp_string_data(new_name_sexp), sexp_string_size(new_name_sexp));

        int idx = t->table->schema()->GetFieldIndex(old_name);
        if (idx < 0)
            return sexp_user_exception(ctx, self, "column not found", old_name_sexp);

        auto new_field = t->table->schema()->field(idx)->WithName(new_name);
        auto result = t->table->SetColumn(idx, new_field, t->table->column(idx));
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), old_name_sexp);
        return wrap_arrow_table(ctx, *result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* __arrow_table_join__(t1, t2, key) -> joined table (hash join) */
static sexp bridge_arrow_table_join(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp t1_sexp, sexp t2_sexp, sexp key_sexp) {
    (void)n;
    EvalArrowTable* t1 = unwrap_arrow_table(t1_sexp);
    EvalArrowTable* t2 = unwrap_arrow_table(t2_sexp);
    if (!t1) return sexp_user_exception(ctx, self, "expected arrow-table (left)", t1_sexp);
    if (!t2) return sexp_user_exception(ctx, self, "expected arrow-table (right)", t2_sexp);
    if (!sexp_stringp(key_sexp))
        return sexp_user_exception(ctx, self, "expected string key", key_sexp);

    try {
        std::string key(sexp_string_data(key_sexp), sexp_string_size(key_sexp));

        /* Build hash map from t2 key column -> row index */
        auto t2_key_col = t2->table->GetColumnByName(key);
        if (!t2_key_col)
            return sexp_user_exception(ctx, self, "key column not found in right table", key_sexp);
        auto t1_key_col = t1->table->GetColumnByName(key);
        if (!t1_key_col)
            return sexp_user_exception(ctx, self, "key column not found in left table", key_sexp);

        /* Simple nested-loop join for now: for each row in t1, find matching rows in t2 */
        std::unordered_map<std::string, std::vector<int64_t>> t2_index;
        for (int64_t i = 0; i < t2->table->num_rows(); i++) {
            auto scalar = chunked_array_ref(ctx, t2_key_col, i);
            /* Use string representation as key */
            std::string key_val;
            if (sexp_fixnump(scalar))
                key_val = std::to_string(sexp_unbox_fixnum(scalar));
            else if (sexp_flonump(scalar))
                key_val = std::to_string(sexp_flonum_value(scalar));
            else if (sexp_stringp(scalar))
                key_val = std::string(sexp_string_data(scalar), sexp_string_size(scalar));
            else
                continue;
            t2_index[key_val].push_back(i);
        }

        /* Build result: all t1 columns + non-key t2 columns */
        arrow::Int64Builder left_indices, right_indices;
        for (int64_t i = 0; i < t1->table->num_rows(); i++) {
            auto scalar = chunked_array_ref(ctx, t1_key_col, i);
            std::string key_val;
            if (sexp_fixnump(scalar))
                key_val = std::to_string(sexp_unbox_fixnum(scalar));
            else if (sexp_flonump(scalar))
                key_val = std::to_string(sexp_flonum_value(scalar));
            else if (sexp_stringp(scalar))
                key_val = std::string(sexp_string_data(scalar), sexp_string_size(scalar));
            else
                continue;

            auto it = t2_index.find(key_val);
            if (it != t2_index.end()) {
                for (int64_t j : it->second) {
                    (void)left_indices.Append(i);
                    (void)right_indices.Append(j);
                }
            }
        }

        auto left_idx_result = left_indices.Finish();
        auto right_idx_result = right_indices.Finish();
        if (!left_idx_result.ok() || !right_idx_result.ok())
            return sexp_user_exception(ctx, self, "join index build failed", key_sexp);

        /* Take from both tables */
        auto t1_taken = arrow::compute::Take(t1->table, arrow::Datum(*left_idx_result));
        auto t2_taken = arrow::compute::Take(t2->table, arrow::Datum(*right_idx_result));
        if (!t1_taken.ok() || !t2_taken.ok())
            return sexp_user_exception(ctx, self, "join take failed", key_sexp);

        auto left_table = t1_taken->table();
        auto right_table = t2_taken->table();

        /* Combine: all left columns + non-key right columns */
        std::vector<std::shared_ptr<arrow::Field>> fields;
        std::vector<std::shared_ptr<arrow::ChunkedArray>> columns;

        for (int i = 0; i < left_table->num_columns(); i++) {
            fields.push_back(left_table->schema()->field(i));
            columns.push_back(left_table->column(i));
        }
        for (int i = 0; i < right_table->num_columns(); i++) {
            if (right_table->schema()->field(i)->name() != key) {
                fields.push_back(right_table->schema()->field(i));
                columns.push_back(right_table->column(i));
            }
        }

        auto schema = arrow::schema(fields);
        auto result = arrow::Table::Make(schema, columns);
        return wrap_arrow_table(ctx, result);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), t1_sexp);
    }
}

/* ================================================================
 * Array operations
 * ================================================================ */

/* __arrow_array_length__(arr) -> int */
static sexp bridge_arrow_array_length(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    return sexp_make_integer(ctx, (long long)a->array->length());
}

/* __arrow_array_null_count__(arr) -> int */
static sexp bridge_arrow_array_null_count(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    return sexp_make_integer(ctx, (long long)a->array->null_count());
}

/* __arrow_array_ref__(arr, i) -> value */
static sexp bridge_arrow_array_ref(sexp ctx, sexp self, sexp_sint_t n,
                                    sexp arr_sexp, sexp idx_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    if (!sexp_fixnump(idx_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum index", idx_sexp);

    int64_t idx = sexp_unbox_fixnum(idx_sexp);
    if (idx < 0 || idx >= a->array->length())
        return sexp_user_exception(ctx, self, "index out of range", idx_sexp);

    return chunked_array_ref(ctx, a->array, idx);
}

/* __arrow_array_to_list__(arr) -> scheme list */
static sexp bridge_arrow_array_to_list(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    sexp result = SEXP_NULL;
    for (int64_t i = a->array->length() - 1; i >= 0; i--) {
        sexp_gc_var1(elem);
        sexp_gc_preserve1(ctx, elem);
        elem = chunked_array_ref(ctx, a->array, i);
        result = sexp_cons(ctx, elem, result);
        sexp_gc_release1(ctx);
    }
    return result;
}

/* Helper for compute aggregations */
static sexp compute_aggregate(sexp ctx, sexp self, const std::shared_ptr<arrow::ChunkedArray>& arr,
                               const std::string& func_name) {
    try {
        auto result = arrow::compute::CallFunction(func_name, {arr});
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), SEXP_FALSE);

        auto scalar = result->scalar();
        return arrow_scalar_to_sexp(ctx, scalar);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), SEXP_FALSE);
    }
}

/* __arrow_array_sum__(arr) */
static sexp bridge_arrow_array_sum(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    return compute_aggregate(ctx, self, a->array, "sum");
}

/* __arrow_array_mean__(arr) */
static sexp bridge_arrow_array_mean(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    return compute_aggregate(ctx, self, a->array, "mean");
}

/* __arrow_array_min__(arr) */
static sexp bridge_arrow_array_min(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    try {
        arrow::compute::ScalarAggregateOptions opts(/*skip_nulls=*/true, /*min_count=*/1);
        auto result = arrow::compute::CallFunction("min_max", {a->array}, &opts);
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), arr_sexp);

        auto struct_scalar = std::static_pointer_cast<arrow::StructScalar>(result->scalar());
        return arrow_scalar_to_sexp(ctx, struct_scalar->value[0]);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arr_sexp);
    }
}

/* __arrow_array_max__(arr) */
static sexp bridge_arrow_array_max(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    try {
        arrow::compute::ScalarAggregateOptions opts(/*skip_nulls=*/true, /*min_count=*/1);
        auto result = arrow::compute::CallFunction("min_max", {a->array}, &opts);
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), arr_sexp);

        auto struct_scalar = std::static_pointer_cast<arrow::StructScalar>(result->scalar());
        return arrow_scalar_to_sexp(ctx, struct_scalar->value[1]);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arr_sexp);
    }
}

/* __arrow_array_count__(arr) */
static sexp bridge_arrow_array_count(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    return compute_aggregate(ctx, self, a->array, "count");
}

/* __arrow_array_unique__(arr) -> new array */
static sexp bridge_arrow_array_unique(sexp ctx, sexp self, sexp_sint_t n, sexp arr_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    try {
        auto result = arrow::compute::Unique(arrow::Datum(a->array));
        if (!result.ok())
            return sexp_user_exception(ctx, self, "unique failed", arr_sexp);

        auto unique_arr = *result;
        auto chunked = std::make_shared<arrow::ChunkedArray>(unique_arr);
        return wrap_arrow_array(ctx, chunked, a->name);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arr_sexp);
    }
}

/* __arrow_array_sort__(arr, direction) -> sorted array */
static sexp bridge_arrow_array_sort(sexp ctx, sexp self, sexp_sint_t n,
                                     sexp arr_sexp, sexp dir_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);

    try {
        arrow::compute::SortOrder order = arrow::compute::SortOrder::Ascending;
        if (sexp_stringp(dir_sexp)) {
            std::string dir(sexp_string_data(dir_sexp), sexp_string_size(dir_sexp));
            if (dir == "desc" || dir == "descending" || dir == "DESC")
                order = arrow::compute::SortOrder::Descending;
        }

        arrow::compute::ArraySortOptions opts(order);
        auto indices_result = arrow::compute::CallFunction("array_sort_indices", {a->array}, &opts);
        if (!indices_result.ok())
            return sexp_user_exception(ctx, self, indices_result.status().ToString().c_str(), arr_sexp);

        auto take_result = arrow::compute::Take(a->array, *indices_result);
        if (!take_result.ok())
            return sexp_user_exception(ctx, self, take_result.status().ToString().c_str(), arr_sexp);

        return wrap_arrow_array(ctx, take_result->chunked_array(), a->name);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arr_sexp);
    }
}

/* __arrow_array_filter__(arr, mask_arr) -> filtered array */
static sexp bridge_arrow_array_filter(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp arr_sexp, sexp mask_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    EvalArrowArray* m = unwrap_arrow_array(mask_sexp);
    if (!m) return sexp_user_exception(ctx, self, "expected arrow-array mask", mask_sexp);

    try {
        auto result = arrow::compute::Filter(a->array, m->array);
        if (!result.ok())
            return sexp_user_exception(ctx, self, result.status().ToString().c_str(), arr_sexp);
        return wrap_arrow_array(ctx, result->chunked_array(), a->name);
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), arr_sexp);
    }
}

/* __arrow_array_is_null__(arr, i) -> bool */
static sexp bridge_arrow_array_is_null(sexp ctx, sexp self, sexp_sint_t n,
                                        sexp arr_sexp, sexp idx_sexp) {
    (void)n;
    EvalArrowArray* a = unwrap_arrow_array(arr_sexp);
    if (!a) return sexp_user_exception(ctx, self, "expected arrow-array", arr_sexp);
    if (!sexp_fixnump(idx_sexp))
        return sexp_user_exception(ctx, self, "expected fixnum index", idx_sexp);

    int64_t idx = sexp_unbox_fixnum(idx_sexp);
    if (idx < 0 || idx >= a->array->length())
        return sexp_user_exception(ctx, self, "index out of range", idx_sexp);

    /* Walk chunks to find the right one */
    int64_t offset = 0;
    for (int c = 0; c < a->array->num_chunks(); c++) {
        auto chunk = a->array->chunk(c);
        if (idx < offset + chunk->length()) {
            return chunk->IsNull(idx - offset) ? SEXP_TRUE : SEXP_FALSE;
        }
        offset += chunk->length();
    }
    return SEXP_FALSE;
}

/* ================================================================
 * Group-by aggregation
 * ================================================================ */

/* __arrow_group_by_agg__(table, key_col, agg_alist) -> result table
 * agg_alist is a list of (column_name . agg_func_string) pairs
 * e.g. (("salary" . "sum") ("age" . "mean"))
 * Empty alist means count-only */
static sexp bridge_arrow_group_by_agg(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp table_sexp, sexp key_sexp, sexp agg_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);
    if (!sexp_stringp(key_sexp))
        return sexp_user_exception(ctx, self, "expected string key column", key_sexp);

    try {
        std::string key_col(sexp_string_data(key_sexp), sexp_string_size(key_sexp));
        auto key_array = t->table->GetColumnByName(key_col);
        if (!key_array)
            return sexp_user_exception(ctx, self, "key column not found", key_sexp);

        /* Parse aggregation specs */
        struct AggSpec {
            std::string col_name;
            std::string func_name;
        };
        std::vector<AggSpec> agg_specs;
        for (sexp p = agg_sexp; sexp_pairp(p); p = sexp_cdr(p)) {
            sexp pair = sexp_car(p);
            if (!sexp_pairp(pair)) continue;
            sexp col = sexp_car(pair);
            sexp func = sexp_cdr(pair);
            if (!sexp_stringp(col) || !sexp_stringp(func)) continue;
            agg_specs.push_back({
                std::string(sexp_string_data(col), sexp_string_size(col)),
                std::string(sexp_string_data(func), sexp_string_size(func))
            });
        }

        /* Sort by key column to find groups */
        arrow::compute::SortOptions sort_opts(
            {arrow::compute::SortKey(key_col, arrow::compute::SortOrder::Ascending)});
        auto sort_indices = arrow::compute::SortIndices(t->table, sort_opts);
        if (!sort_indices.ok())
            return sexp_user_exception(ctx, self, sort_indices.status().ToString().c_str(), key_sexp);

        auto sorted_result = arrow::compute::Take(t->table, *sort_indices);
        if (!sorted_result.ok())
            return sexp_user_exception(ctx, self, sorted_result.status().ToString().c_str(), key_sexp);
        auto sorted_table = sorted_result->table();
        auto sorted_key = sorted_table->GetColumnByName(key_col);

        /* Helper to get Arrow scalar from chunked array at flat index */
        auto get_scalar = [](const std::shared_ptr<arrow::ChunkedArray>& arr,
                             int64_t index) -> std::shared_ptr<arrow::Scalar> {
            int64_t offset = 0;
            for (int c = 0; c < arr->num_chunks(); c++) {
                auto chunk = arr->chunk(c);
                if (index < offset + chunk->length())
                    return chunk->GetScalar(index - offset).ValueOrDie();
                offset += chunk->length();
            }
            return nullptr;
        };

        /* Find group boundaries using Arrow scalar comparison */
        struct Group {
            int64_t start;
            int64_t length;
        };
        std::vector<Group> groups;
        int64_t nrows = sorted_table->num_rows();
        if (nrows > 0) {
            int64_t group_start = 0;
            for (int64_t i = 1; i <= nrows; i++) {
                bool boundary = (i == nrows);
                if (!boundary) {
                    auto prev_scalar = get_scalar(sorted_key, i - 1);
                    auto curr_scalar = get_scalar(sorted_key, i);
                    if (prev_scalar && curr_scalar)
                        boundary = !prev_scalar->Equals(*curr_scalar);
                    else
                        boundary = true;
                }
                if (boundary) {
                    groups.push_back({group_start, i - group_start});
                    group_start = i;
                }
            }
        }

        /* Build result columns */
        /* Key column: one value per group */
        std::vector<std::shared_ptr<arrow::Field>> result_fields;
        std::vector<std::shared_ptr<arrow::ChunkedArray>> result_columns;

        /* Determine key type and build key result */
        auto key_type = sorted_key->type();
        if (key_type->id() == arrow::Type::INT64) {
            arrow::Int64Builder kb;
            for (auto& g : groups)
                (void)kb.Append(std::static_pointer_cast<arrow::Int64Scalar>(
                    get_scalar(sorted_key, g.start))->value);
            auto r = kb.Finish();
            result_fields.push_back(arrow::field(key_col, arrow::int64()));
            result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
        } else if (key_type->id() == arrow::Type::DOUBLE) {
            arrow::DoubleBuilder kb;
            for (auto& g : groups)
                (void)kb.Append(std::static_pointer_cast<arrow::DoubleScalar>(
                    get_scalar(sorted_key, g.start))->value);
            auto r = kb.Finish();
            result_fields.push_back(arrow::field(key_col, arrow::float64()));
            result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
        } else {
            arrow::StringBuilder kb;
            for (auto& g : groups) {
                auto scalar = get_scalar(sorted_key, g.start);
                if (scalar->type->id() == arrow::Type::STRING) {
                    auto s = std::static_pointer_cast<arrow::StringScalar>(scalar);
                    (void)kb.Append(std::string((const char*)s->value->data(), s->value->size()));
                } else {
                    (void)kb.Append(scalar->ToString());
                }
            }
            auto r = kb.Finish();
            result_fields.push_back(arrow::field(key_col, arrow::utf8()));
            result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
        }

        /* Count column if no agg specs */
        if (agg_specs.empty()) {
            arrow::Int64Builder cb;
            for (auto& g : groups)
                (void)cb.Append(g.length);
            auto r = cb.Finish();
            result_fields.push_back(arrow::field("count", arrow::int64()));
            result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
        }

        /* Aggregated columns */
        for (auto& spec : agg_specs) {
            auto src_col = sorted_table->GetColumnByName(spec.col_name);
            if (!src_col) continue;

            bool is_float_result = (spec.func_name == "mean") ||
                                   src_col->type()->id() == arrow::Type::DOUBLE ||
                                   src_col->type()->id() == arrow::Type::FLOAT;

            if (is_float_result || spec.func_name == "mean") {
                arrow::DoubleBuilder ab;
                for (auto& g : groups) {
                    auto slice = sorted_table->Slice(g.start, g.length);
                    auto slice_col = slice->GetColumnByName(spec.col_name);
                    auto agg_result = arrow::compute::CallFunction(spec.func_name, {slice_col});
                    if (agg_result.ok()) {
                        auto scalar = agg_result->scalar();
                        if (scalar->is_valid) {
                            double val = 0;
                            if (scalar->type->id() == arrow::Type::DOUBLE)
                                val = std::static_pointer_cast<arrow::DoubleScalar>(scalar)->value;
                            else if (scalar->type->id() == arrow::Type::INT64)
                                val = (double)std::static_pointer_cast<arrow::Int64Scalar>(scalar)->value;
                            (void)ab.Append(val);
                        } else {
                            (void)ab.AppendNull();
                        }
                    } else {
                        (void)ab.AppendNull();
                    }
                }
                auto r = ab.Finish();
                result_fields.push_back(arrow::field(spec.col_name, arrow::float64()));
                result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
            } else {
                arrow::Int64Builder ab;
                for (auto& g : groups) {
                    auto slice = sorted_table->Slice(g.start, g.length);
                    auto slice_col = slice->GetColumnByName(spec.col_name);
                    auto agg_result = arrow::compute::CallFunction(spec.func_name, {slice_col});
                    if (agg_result.ok()) {
                        auto scalar = agg_result->scalar();
                        if (scalar->is_valid) {
                            int64_t val = 0;
                            if (scalar->type->id() == arrow::Type::INT64)
                                val = std::static_pointer_cast<arrow::Int64Scalar>(scalar)->value;
                            else if (scalar->type->id() == arrow::Type::DOUBLE)
                                val = (int64_t)std::static_pointer_cast<arrow::DoubleScalar>(scalar)->value;
                            (void)ab.Append(val);
                        } else {
                            (void)ab.AppendNull();
                        }
                    } else {
                        (void)ab.AppendNull();
                    }
                }
                auto r = ab.Finish();
                result_fields.push_back(arrow::field(spec.col_name, arrow::int64()));
                result_columns.push_back(std::make_shared<arrow::ChunkedArray>(*r));
            }
        }

        auto schema = arrow::schema(result_fields);
        auto result_table = arrow::Table::Make(schema, result_columns);
        return wrap_arrow_table(ctx, result_table);

    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* ================================================================
 * Display
 * ================================================================ */

/* __arrow_table_to_string__(table) -> formatted text */
static sexp bridge_arrow_table_to_string(sexp ctx, sexp self, sexp_sint_t n, sexp table_sexp) {
    (void)n;
    EvalArrowTable* t = unwrap_arrow_table(table_sexp);
    if (!t) return sexp_user_exception(ctx, self, "expected arrow-table", table_sexp);

    try {
        auto table = t->table;
        int ncols = table->num_columns();
        int64_t nrows = table->num_rows();
        int64_t display_rows = (nrows > 20) ? 20 : nrows;

        /* Compute column widths and collect string representations */
        std::vector<std::string> headers;
        std::vector<std::vector<std::string>> col_strings(ncols);
        std::vector<int> widths;

        for (int c = 0; c < ncols; c++) {
            std::string name = table->schema()->field(c)->name();
            headers.push_back(name);
            widths.push_back((int)name.size());

            auto col = table->column(c);
            for (int64_t r = 0; r < display_rows; r++) {
                auto scalar = chunked_array_ref(ctx, col, r);
                std::string val;
                if (scalar == SEXP_FALSE) val = "null";
                else if (sexp_fixnump(scalar)) {
                    char buf[32];
                    snprintf(buf, sizeof(buf), "%lld", (long long)sexp_unbox_fixnum(scalar));
                    val = buf;
                }
                else if (sexp_flonump(scalar)) {
                    char buf[32];
                    snprintf(buf, sizeof(buf), "%g", sexp_flonum_value(scalar));
                    val = buf;
                }
                else if (sexp_stringp(scalar))
                    val = std::string(sexp_string_data(scalar), sexp_string_size(scalar));
                else if (scalar == SEXP_TRUE) val = "true";
                else val = "?";

                col_strings[c].push_back(val);
                if ((int)val.size() > widths[c]) widths[c] = (int)val.size();
            }
        }

        std::ostringstream out;

        /* Header */
        for (int c = 0; c < ncols; c++) {
            if (c > 0) out << " | ";
            out << std::setw(widths[c]) << std::left << headers[c];
        }
        out << "\n";

        /* Separator */
        for (int c = 0; c < ncols; c++) {
            if (c > 0) out << "-+-";
            for (int j = 0; j < widths[c]; j++) out << "-";
        }
        out << "\n";

        /* Rows */
        for (int64_t r = 0; r < display_rows; r++) {
            for (int c = 0; c < ncols; c++) {
                if (c > 0) out << " | ";
                out << std::setw(widths[c]) << std::left << col_strings[c][r];
            }
            out << "\n";
        }

        if (nrows > 20) {
            out << "... (" << nrows << " rows total)\n";
        }

        std::string result = out.str();
        return sexp_c_string(ctx, result.c_str(), (int)result.size());
    } catch (std::exception& e) {
        return sexp_user_exception(ctx, self, e.what(), table_sexp);
    }
}

/* ================================================================
 * Registration
 * ================================================================ */

extern "C" void register_arrow_bridge_functions(sexp ctx, sexp env) {
    /* Type predicates */
    sexp_define_foreign(ctx, env, "%arrow-table?", 1,
                        (sexp_proc1)bridge_arrow_table_p);
    sexp_define_foreign(ctx, env, "%arrow-array?", 1,
                        (sexp_proc1)bridge_arrow_array_p);

    /* Table creation */
    sexp_define_foreign(ctx, env, "__arrow_make_table__", 1,
                        (sexp_proc1)bridge_arrow_make_table);

    /* File I/O */
    sexp_define_foreign(ctx, env, "__arrow_read_csv__", 1,
                        (sexp_proc1)bridge_arrow_read_csv);
    sexp_define_foreign(ctx, env, "__arrow_write_csv__", 2,
                        (sexp_proc1)bridge_arrow_write_csv);
    sexp_define_foreign(ctx, env, "__arrow_read_parquet__", 1,
                        (sexp_proc1)bridge_arrow_read_parquet);
    sexp_define_foreign(ctx, env, "__arrow_write_parquet__", 2,
                        (sexp_proc1)bridge_arrow_write_parquet);
    sexp_define_foreign(ctx, env, "__arrow_read_ipc__", 1,
                        (sexp_proc1)bridge_arrow_read_ipc);
    sexp_define_foreign(ctx, env, "__arrow_write_ipc__", 2,
                        (sexp_proc1)bridge_arrow_write_ipc);

    /* Table properties */
    sexp_define_foreign(ctx, env, "__arrow_table_num_rows__", 1,
                        (sexp_proc1)bridge_arrow_table_num_rows);
    sexp_define_foreign(ctx, env, "__arrow_table_num_columns__", 1,
                        (sexp_proc1)bridge_arrow_table_num_columns);
    sexp_define_foreign(ctx, env, "__arrow_table_column_names__", 1,
                        (sexp_proc1)bridge_arrow_table_column_names);
    sexp_define_foreign(ctx, env, "__arrow_table_schema__", 1,
                        (sexp_proc1)bridge_arrow_table_schema);
    sexp_define_foreign(ctx, env, "__arrow_table_column__", 2,
                        (sexp_proc1)bridge_arrow_table_column);

    /* Table operations */
    sexp_define_foreign(ctx, env, "__arrow_table_select__", 2,
                        (sexp_proc1)bridge_arrow_table_select);
    sexp_define_foreign(ctx, env, "__arrow_table_drop__", 2,
                        (sexp_proc1)bridge_arrow_table_drop);
    sexp_define_foreign(ctx, env, "__arrow_table_filter__", 4,
                        (sexp_proc1)bridge_arrow_table_filter);
    sexp_define_foreign(ctx, env, "__arrow_table_sort__", 3,
                        (sexp_proc1)bridge_arrow_table_sort);
    sexp_define_foreign(ctx, env, "__arrow_table_head__", 2,
                        (sexp_proc1)bridge_arrow_table_head);
    sexp_define_foreign(ctx, env, "__arrow_table_tail__", 2,
                        (sexp_proc1)bridge_arrow_table_tail);
    sexp_define_foreign(ctx, env, "__arrow_table_slice__", 3,
                        (sexp_proc1)bridge_arrow_table_slice);
    sexp_define_foreign(ctx, env, "__arrow_table_add_column__", 3,
                        (sexp_proc1)bridge_arrow_table_add_column);
    sexp_define_foreign(ctx, env, "__arrow_table_remove_column__", 2,
                        (sexp_proc1)bridge_arrow_table_remove_column);
    sexp_define_foreign(ctx, env, "__arrow_table_rename__", 3,
                        (sexp_proc1)bridge_arrow_table_rename);
    sexp_define_foreign(ctx, env, "__arrow_table_join__", 3,
                        (sexp_proc1)bridge_arrow_table_join);

    /* Array operations */
    sexp_define_foreign(ctx, env, "__arrow_array_length__", 1,
                        (sexp_proc1)bridge_arrow_array_length);
    sexp_define_foreign(ctx, env, "__arrow_array_null_count__", 1,
                        (sexp_proc1)bridge_arrow_array_null_count);
    sexp_define_foreign(ctx, env, "__arrow_array_ref__", 2,
                        (sexp_proc1)bridge_arrow_array_ref);
    sexp_define_foreign(ctx, env, "__arrow_array_to_list__", 1,
                        (sexp_proc1)bridge_arrow_array_to_list);
    sexp_define_foreign(ctx, env, "__arrow_array_sum__", 1,
                        (sexp_proc1)bridge_arrow_array_sum);
    sexp_define_foreign(ctx, env, "__arrow_array_mean__", 1,
                        (sexp_proc1)bridge_arrow_array_mean);
    sexp_define_foreign(ctx, env, "__arrow_array_min__", 1,
                        (sexp_proc1)bridge_arrow_array_min);
    sexp_define_foreign(ctx, env, "__arrow_array_max__", 1,
                        (sexp_proc1)bridge_arrow_array_max);
    sexp_define_foreign(ctx, env, "__arrow_array_count__", 1,
                        (sexp_proc1)bridge_arrow_array_count);
    sexp_define_foreign(ctx, env, "__arrow_array_unique__", 1,
                        (sexp_proc1)bridge_arrow_array_unique);
    sexp_define_foreign(ctx, env, "__arrow_array_sort__", 2,
                        (sexp_proc1)bridge_arrow_array_sort);
    sexp_define_foreign(ctx, env, "__arrow_array_filter__", 2,
                        (sexp_proc1)bridge_arrow_array_filter);
    sexp_define_foreign(ctx, env, "__arrow_array_is_null__", 2,
                        (sexp_proc1)bridge_arrow_array_is_null);

    /* Group-by */
    sexp_define_foreign(ctx, env, "__arrow_group_by_agg__", 3,
                        (sexp_proc1)bridge_arrow_group_by_agg);

    /* Display */
    sexp_define_foreign(ctx, env, "__arrow_table_to_string__", 1,
                        (sexp_proc1)bridge_arrow_table_to_string);
}

#endif /* EVAL_HAVE_ARROW */
