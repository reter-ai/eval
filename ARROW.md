# Arrow Columnar Data

Eval includes built-in support for [Apache Arrow](https://arrow.apache.org/), a columnar in-memory data format designed for efficient analytics. Create tables from dicts, filter/sort/group/join with zero-copy performance, and read/write CSV, Parquet, and Arrow IPC files — all with Eval's OO `->` syntax.

```
Eval dict
  --> Arrow C++ library
  --> Columnar Table (shared_ptr<arrow::Table>)
  --> Filter, sort, group-by, join via Arrow Compute
  --> Read/write CSV, Parquet, IPC
```

## Quick Start

```javascript
// Create a table from a dict of columns
define t = Table(dict(
    name:   ["Alice", "Bob", "Charlie", "Diana"],
    dept:   ["eng", "sales", "eng", "sales"],
    salary: [120, 80, 110, 90]
));

t->num_rows;       // => 4
t->num_columns;    // => 3
t->columns;        // => ("name" "dept" "salary")

// Filter, sort, take top result
define top = t->filter("salary", >, 100)->sort("salary", "desc")->head(1);
top->column("name")->ref(0);    // => "Alice"

// Group-by aggregation
define by_dept = t->group_by("dept")->mean("salary");
by_dept->num_rows;    // => 2  (one row per dept)

// Save to Parquet and read back
t->to_parquet("employees.parquet");
define t2 = read_parquet("employees.parquet");
t2->num_rows;    // => 4
```

## Table Creation

### `Table(dict)`

Creates an Arrow table from a dict where keys are column names and values are lists of column data. Type inference is automatic — integers become Int64, floats become Float64, strings become Utf8:

```javascript
// Integer and float columns
define t = Table(dict(x: [1, 2, 3], y: [4.0, 5.0, 6.0]));

// String columns
define people = Table(dict(
    name: ["Alice", "Bob", "Charlie"],
    age:  [30, 25, 35]
));

// Single column
define ids = Table(dict(id: [100, 200, 300]));
```

All column lists must have the same length. Mixed types within a column are not supported — each column is homogeneously typed.

## Table Properties

Properties return values directly, no parentheses needed:

```javascript
define t = Table(dict(x: [1, 2, 3], y: [4.0, 5.0, 6.0]));

t->num_rows;       // => 3
t->num_columns;    // => 2
t->columns;        // => ("x" "y")  — list of column name strings
t->schema;         // => (("x" "int64") ("y" "double"))  — list of (name, type) pairs
t->length;         // => 3  (alias for num_rows)
```

| Property | Type | Description |
|----------|------|-------------|
| `->num_rows` | integer | Number of rows |
| `->num_columns` | integer | Number of columns |
| `->columns` | list | Column name strings |
| `->schema` | list | List of `(name type)` pairs |
| `->length` | integer | Alias for `num_rows` |

## Column Access

### `table->column(name)` / `table->column(index)`

Retrieve a column by name (string) or by index (integer). Returns an Arrow array object:

```javascript
define t = Table(dict(x: [1, 2, 3], y: [4.0, 5.0, 6.0]));

define col_x = t->column("x");     // by name
define col_0 = t->column(0);       // by index (same column)

col_x->length;    // => 3
col_x->sum;       // => 6
```

## Array Properties

Array objects (columns) have aggregate properties computed by Arrow Compute:

```javascript
define col = t->column("x");    // [1, 2, 3]

col->length;       // => 3
col->sum;          // => 6
col->mean;         // => 2.0
col->min;          // => 1
col->max;          // => 3
col->count;        // => 3  (non-null count)
col->null_count;   // => 0
```

| Property | Type | Description |
|----------|------|-------------|
| `->length` | integer | Number of elements |
| `->sum` | number | Sum of all elements |
| `->mean` | float | Mean of all elements |
| `->min` | number | Minimum value |
| `->max` | number | Maximum value |
| `->count` | integer | Non-null count |
| `->null_count` | integer | Null count |

## Array Element Access

### `array->ref(index)`

Access individual elements by zero-based index:

```javascript
define col = t->column("x");    // [1, 2, 3]
col->ref(0);    // => 1
col->ref(1);    // => 2
col->ref(2);    // => 3

// String columns work too
define names = people->column("name");
names->ref(0);    // => "Alice"
names->ref(2);    // => "Charlie"
```

### `array->to_list()`

Convert an Arrow array to a regular Eval list:

```javascript
t->column("x")->to_list();       // => (1 2 3)
t->column("y")->to_list();       // => (4.0 5.0 6.0)
names->to_list();                 // => ("Alice" "Bob" "Charlie")
```

### `array->is_null(index)`

Check if a specific element is null:

```javascript
col->is_null(0);    // => false
```

## Filtering

### `table->filter(column, op, value)`

Filter rows where `column op value` is true. The operator can be any of the standard comparison operators — they are passed directly as first-class values:

```javascript
define t = Table(dict(x: [1, 2, 3, 4, 5], y: [10, 20, 30, 40, 50]));

t->filter("x", >, 3);       // rows where x > 3  (2 rows)
t->filter("x", <, 3);       // rows where x < 3  (2 rows)
t->filter("x", >=, 3);      // rows where x >= 3 (3 rows)
t->filter("x", <=, 3);      // rows where x <= 3 (3 rows)
t->filter("x", =, 3);       // rows where x == 3 (1 row)

// Float comparisons
t->filter("y", >, 25.0);    // rows where y > 25 (3 rows)
```

Operators are first-class values in Eval — `>`, `<`, `>=`, `<=`, `=` are passed directly (not as strings). The filter is executed by Arrow Compute for columnar performance.

## Sorting

### `table->sort(column, direction)`

Sort the table by a column. Direction is `"asc"` (default) or `"desc"`:

```javascript
define t = Table(dict(name: ["Charlie", "Alice", "Bob"], score: [85, 92, 78]));

// Sort by score ascending
define by_score = t->sort("score", "asc");
by_score->column("score")->ref(0);    // => 78

// Sort by name descending
define by_name = t->sort("name", "desc");
by_name->column("name")->ref(0);      // => "Charlie"

// Default is ascending
t->sort("score");    // same as sort("score", "asc")
```

## Head, Tail, Slice

```javascript
define t = Table(dict(x: [1, 2, 3, 4, 5]));

// First n rows
t->head(2)->num_rows;                    // => 2
t->head(2)->column("x")->ref(0);        // => 1

// Last n rows
t->tail(2)->num_rows;                    // => 2
t->tail(1)->column("x")->ref(0);        // => 5

// Slice: offset and length
t->slice(1, 3)->num_rows;               // => 3
t->slice(1, 3)->column("x")->ref(0);    // => 2
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->head(n)` | Table | First `n` rows |
| `->tail(n)` | Table | Last `n` rows |
| `->slice(offset, length)` | Table | `length` rows starting at `offset` |

## Select and Drop

### `table->select(column_list)`

Select specific columns by name:

```javascript
define t = Table(dict(x: [1, 2, 3], y: [4, 5, 6], z: [7, 8, 9]));

define xy = t->select(list("x", "y"));
xy->num_columns;    // => 2
xy->columns;        // => ("x" "y")
xy->num_rows;       // => 3 (rows preserved)
```

### `table->drop(column_name)`

Remove a single column by name:

```javascript
define no_z = t->drop("z");
no_z->num_columns;    // => 2
no_z->columns;        // => ("x" "y")
```

## Rename

### `table->rename(old_name, new_name)`

Rename a column:

```javascript
define t = Table(dict(x: [1, 2, 3], y: [4, 5, 6]));
define renamed = t->rename("x", "value");
renamed->columns;                        // => ("value" "y")
renamed->column("value")->ref(0);        // => 1
```

## Add and Remove Columns

### `table->add_column(name, array)`

Add a new column to the table.

### `table->remove_column(name)`

Remove a column by name (same as `drop`).

## Join

### `table->join(other_table, key_column)`

Inner join two tables on a shared key column:

```javascript
define left = Table(dict(id: [1, 2, 3], name: ["Alice", "Bob", "Charlie"]));
define right = Table(dict(id: [2, 3, 4], score: [85, 92, 78]));

define joined = left->join(right, "id");
joined->num_rows;    // => 2  (only ids 2 and 3 match)

// Result has columns from both tables
member("name", joined->columns);     // => ("name" ...)  (truthy)
member("score", joined->columns);    // => ("score" ...) (truthy)
```

The join matches rows where both tables have the same value in the key column. Only matching rows are kept (inner join).

## Group-by Aggregation

### `table->group_by(key_column)`

Group rows by a key column, then apply an aggregation:

```javascript
define t = Table(dict(
    dept:   ["eng", "eng", "sales", "sales", "eng"],
    salary: [100, 120, 80, 90, 110]
));

// Sum salaries by department
define g_sum = t->group_by("dept")->sum("salary");
g_sum->num_rows;       // => 2 (eng, sales)
g_sum->num_columns;    // => 2 (dept, salary)

// Count rows per department
define g_count = t->group_by("dept")->count();
g_count->num_rows;     // => 2

// Mean salary by department
define g_mean = t->group_by("dept")->mean("salary");
g_mean->num_rows;      // => 2

// Min and max
t->group_by("dept")->min("salary");
t->group_by("dept")->max("salary");
```

| Aggregation | Syntax | Description |
|-------------|--------|-------------|
| `->sum(col)` | `group->sum("salary")` | Sum values per group |
| `->mean(col)` | `group->mean("salary")` | Mean values per group |
| `->min(col)` | `group->min("salary")` | Minimum value per group |
| `->max(col)` | `group->max("salary")` | Maximum value per group |
| `->count()` | `group->count()` | Count rows per group |

## Array Operations

Arrays (columns) support their own set of operations beyond the aggregate properties:

### `array->unique()`

Return an array with duplicate values removed:

```javascript
define t = Table(dict(v: [3, 1, 2, 1, 3, 2]));
define col = t->column("v");
col->length;              // => 6

define uniq = col->unique();
uniq->length;             // => 3
```

### `array->sort(direction)`

Sort the array values. Direction is `"asc"` (default) or `"desc"`:

```javascript
define col = t->column("v");    // [3, 1, 2, 1, 3, 2]

define sorted = col->sort("asc");
sorted->ref(0);    // => 1
sorted->ref(5);    // => 3
```

### `array->filter(mask)`

Filter array elements using a boolean mask array.

## Display

### `table->to_string()`

Returns a formatted text representation of the table:

```javascript
define t = Table(dict(name: ["Alice", "Bob"], score: [95, 87]));
define str = t->to_string();
display(str);
```

The output is an aligned text table with column headers and values.

## File I/O

Arrow tables can be read from and written to three file formats: CSV, Parquet, and Arrow IPC.

### CSV

```javascript
// Write table to CSV
t->to_csv("data.csv");

// Read CSV into a table
define t = read_csv("data.csv");
t->num_rows;       // row count from file
t->num_columns;    // column count from file
```

### Parquet

[Parquet](https://parquet.apache.org/) is a columnar storage format optimized for analytics — compressed, efficient, and widely used in data engineering:

```javascript
// Write table to Parquet
t->to_parquet("data.parquet");

// Read Parquet into a table
define t = read_parquet("data.parquet");
```

### Arrow IPC

Arrow IPC (Inter-Process Communication) format preserves Arrow's in-memory layout for zero-deserialization reads:

```javascript
// Write table to Arrow IPC
t->to_ipc("data.arrow");

// Read Arrow IPC into a table
define t = read_ipc("data.arrow");
```

### Round-trip Example

```javascript
// Create, save, reload — all three formats
define t = Table(dict(x: [1, 2, 3], y: [4.0, 5.0, 6.0]));

t->to_csv("data.csv");
t->to_parquet("data.parquet");
t->to_ipc("data.arrow");

define from_csv     = read_csv("data.csv");
define from_parquet = read_parquet("data.parquet");
define from_ipc     = read_ipc("data.arrow");

// All three have the same data
from_csv->num_rows;                   // => 3
from_parquet->column("x")->ref(0);   // => 1
from_ipc->column("y")->to_list();    // => (4.0 5.0 6.0)
```

## Chaining

All table methods that return a table can be chained with `->`:

```javascript
define t = Table(dict(x: [1, 2, 3, 4, 5], y: [50, 40, 30, 20, 10]));

// Filter → sort → take top 2
t->filter("x", >, 2)->sort("y", "desc")->head(2);
// Table with x=[3,4], y=[30,20]

// Select columns → filter → get first value
t->select(list("x"))->filter("x", <=, 3)->column("x")->ref(0);
// => 1
```

Chaining works naturally because each operation returns a new table (or array), and the `->` operator dispatches on the result type.

## Data Analysis Example

A complete example showing typical data analysis workflow:

```javascript
// Employee dataset
define employees = Table(dict(
    name:   ["Alice", "Bob", "Charlie", "Diana", "Eve"],
    dept:   ["eng", "sales", "eng", "sales", "eng"],
    salary: [120, 80, 110, 90, 130]
));

// Who earns more than 100?
define high_earners = employees->filter("salary", >, 100);
high_earners->column("name")->to_list();
// => ("Alice" "Charlie" "Eve")

// Average salary by department
define avg_by_dept = employees->group_by("dept")->mean("salary");
display(avg_by_dept->to_string());

// Top earner
define top = employees->sort("salary", "desc")->head(1);
top->column("name")->ref(0);       // => "Eve"
top->column("salary")->ref(0);     // => 130

// Join with performance reviews
define reviews = Table(dict(
    name:  ["Alice", "Bob", "Charlie", "Diana", "Eve"],
    score: [92, 78, 88, 95, 85]
));
define combined = employees->join(reviews, "name");
combined->num_columns;    // => 4  (name, dept, salary, score)

// Save for later analysis
combined->to_parquet("employee_reviews.parquet");
```

## Complete Reference

### Free Functions

| Function | Arguments | Returns | Description |
|----------|-----------|---------|-------------|
| `Table(dict)` | dict of column lists | Table | Create table from dict |
| `read_csv(path)` | string | Table | Read CSV file |
| `read_parquet(path)` | string | Table | Read Parquet file |
| `read_ipc(path)` | string | Table | Read Arrow IPC file |

### Table Properties (no parentheses)

| Property | Type | Description |
|----------|------|-------------|
| `->num_rows` | integer | Number of rows |
| `->num_columns` | integer | Number of columns |
| `->columns` | list | Column name strings |
| `->schema` | list | List of `(name type)` pairs |
| `->length` | integer | Alias for `num_rows` |

### Table Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->column(name_or_index)` | Array | Get column by name or index |
| `->select(name_list)` | Table | Keep listed columns |
| `->drop(name)` | Table | Remove a column |
| `->filter(col, op, val)` | Table | Filter rows by comparison |
| `->sort(col, dir)` | Table | Sort by column (`"asc"` / `"desc"`) |
| `->head(n)` | Table | First n rows |
| `->tail(n)` | Table | Last n rows |
| `->slice(offset, length)` | Table | Subrange of rows |
| `->add_column(name, array)` | Table | Add a column |
| `->remove_column(name)` | Table | Remove a column |
| `->rename(old, new)` | Table | Rename a column |
| `->join(other, key)` | Table | Inner join on key column |
| `->group_by(key)` | GroupBy | Group rows by key |
| `->to_csv(path)` | void | Write to CSV file |
| `->to_parquet(path)` | void | Write to Parquet file |
| `->to_ipc(path)` | void | Write to Arrow IPC file |
| `->to_string()` | string | Formatted text display |

### GroupBy Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `->sum(col)` | Table | Sum per group |
| `->mean(col)` | Table | Mean per group |
| `->min(col)` | Table | Min per group |
| `->max(col)` | Table | Max per group |
| `->count()` | Table | Row count per group |

### Array Properties (no parentheses)

| Property | Type | Description |
|----------|------|-------------|
| `->length` | integer | Number of elements |
| `->sum` | number | Sum of all elements |
| `->mean` | float | Mean of all elements |
| `->min` | number | Minimum value |
| `->max` | number | Maximum value |
| `->count` | integer | Non-null count |
| `->null_count` | integer | Null count |

### Array Methods (call with parentheses)

| Method | Returns | Description |
|--------|---------|-------------|
| `->ref(index)` | value | Element at index |
| `->to_list()` | list | Convert to Eval list |
| `->unique()` | Array | Deduplicated values |
| `->sort(dir)` | Array | Sorted values |
| `->filter(mask)` | Array | Filter by boolean mask |
| `->is_null(index)` | boolean | Check if element is null |

## How It Works

The implementation bridges Apache Arrow's C++ library into Eval via ~40 C functions:

1. **Type registration**: Two new types are registered with chibi-scheme — `arrow-table` (wraps `shared_ptr<arrow::Table>`) and `arrow-array` (wraps `shared_ptr<arrow::ChunkedArray>`). Both have GC finalizers that release the underlying Arrow memory.

2. **Table creation**: `Table(dict(...))` converts an Eval dict to an Arrow table. The dict's keys become column names and the values (lists) are scanned for type — integers build `Int64Array`, floats build `DoubleArray`, strings build `StringBuilder`. All arrays are assembled into an `arrow::Table`.

3. **Compute operations**: Filter, sort, group-by, unique, and aggregate functions use Arrow Compute kernels (`CallFunction("greater", ...)`, `SortIndices`, etc.) for vectorized columnar performance.

4. **OO dispatch**: The `->` operator routes through `__send__`, which checks `%arrow-table?` and `%arrow-array?` type predicates and dispatches to the appropriate method. Properties (like `->num_rows`) return values directly; methods (like `->filter(...)`) return closures.

5. **File I/O**: CSV uses `arrow::csv::TableReader`, Parquet uses `parquet::arrow::FileReader/WriteTable`, and IPC uses `arrow::ipc::RecordBatchFileReader/Writer`. All operations go through Arrow's `arrow::io` layer.

## Comparison with Lists

| | Arrow Table | Eval Lists |
|---|---|---|
| **Layout** | Columnar (cache-friendly for analytics) | Linked list (cons cells) |
| **Type system** | Homogeneous typed columns (int64, float64, utf8) | Dynamically typed |
| **Aggregation** | Vectorized compute kernels (sum, mean, filter) | Loop-based fold/map |
| **Group-by** | Built-in with sort-based grouping | Manual implementation |
| **File I/O** | CSV, Parquet, Arrow IPC | Manual string parsing |
| **Memory** | Contiguous buffers, zero-copy slicing | GC-managed cons cells |
| **Use case** | Data analysis, ETL, columnar processing | General-purpose programming |

Use Arrow tables when working with structured, tabular data — especially for filtering, aggregation, and file I/O. Use Eval lists for general-purpose programming, recursive data structures, and functional transformations.

## Optional Feature

Arrow support is optional — it requires the `arrow` vcpkg package with `parquet`, `compute`, and `csv` features. The build system detects it automatically:

```
Found Arrow — columnar data enabled        (arrow installed)
Arrow not found — columnar data disabled   (arrow not installed)
```

All Arrow code is conditionally compiled behind `EVAL_HAVE_ARROW` (and `EVAL_HAVE_PARQUET` for Parquet). The rest of Eval works identically with or without it.

## See also

- [BINARY.md](BINARY.md) — Cap'n Proto binary serialization (another columnar-friendly format)
- [DICTS.md](DICTS.md) — Dict methods (dicts are used to create Arrow tables)
- [LISTS.md](LISTS.md) — List methods (arrays can be converted to lists with `->to_list()`)
- [DIFFERENTIAL.md](DIFFERENTIAL.md) — Differentiable programming with tensor operations
- [FILESYS.md](FILESYS.md) — Filesystem operations
- [TESTS.md](TESTS.md) — Test framework used by the Arrow test suite
