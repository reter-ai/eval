# Binary Serialization with Cap'n Proto

Eval includes built-in support for binary serialization using [Cap'n Proto](https://capnproto.org/), a high-performance zero-copy serialization format. You write a schema string, and the system parses it at runtime using Cap'n Proto's own C++ library — no code generation step, no external tools.

```
Cap'n Proto schema string
  --> capnp::SchemaParser (C++ library)
  --> capnp::StructSchema (runtime metadata)
  --> DynamicStruct::Builder / Reader
  --> Zero-copy serialization to/from bytevectors
```

The result is compact binary messages that are wire-compatible with any Cap'n Proto implementation in any language.

## Quick Start

```javascript
// Define a schema (parsed once, reused for all operations)
define s = Schema("""
  @0xdbb9ad1f14bf0b36;
  struct Person {
    name   @0 :Text;
    age    @1 :UInt32;
    active @2 :Bool;
  }
""");

// Serialize to binary
define msg = s->Person->build("name", "Alice", "age", 30, "active", true);
// msg is a bytevector containing the Cap'n Proto wire format

// Deserialize (zero-copy read)
define r = s->Person->read(msg);
display(r->name);     // "Alice"
display(r->age);      // 30
display(r->active);   // true
```

## API

### `Schema(text)`

Parses a Cap'n Proto schema string and returns a schema object. The schema is parsed once by Cap'n Proto's own `SchemaParser` — full schema syntax is supported.

The returned object dispatches struct names via `->`:

```javascript
define s = Schema("""
  @0xaabbccdd11223344;
  struct Color { r @0 :UInt8; g @1 :UInt8; b @2 :UInt8; }
  struct Pixel { x @0 :UInt32; y @1 :UInt32; }
""");

// Access struct types by name
s->Color    // struct accessor for Color
s->Pixel    // struct accessor for Pixel
```

A schema can contain multiple struct definitions. Each is accessible by name.

### `schema->StructName->build(field, value, ...)`

Builds a binary message from field-value pairs. Returns a bytevector containing the serialized Cap'n Proto message.

```javascript
define msg = s->Color->build("r", 255, "g", 128, "b", 0);
bytevector?(msg);    // true
```

Fields are specified as alternating name-value pairs. Omitted fields use their Cap'n Proto defaults (typically zero/false/empty).

```javascript
// Build with no fields — all defaults
define msg = s->Color->build();
define r = s->Color->read(msg);
r->r;    // 0
r->g;    // 0
r->b;    // 0
```

### `schema->StructName->read(bytevector)`

Reads a binary message and returns a reader object. The reader provides zero-copy access to fields via `->`:

```javascript
define r = s->Color->read(msg);
r->r;      // 255
r->g;      // 128
r->b;      // 0
```

## Supported Types

Cap'n Proto's full type system is supported:

### Integers

```javascript
define s = Schema("""
  @0xb2c3d4e5f6071829;
  struct Numbers {
    small  @0 :Int8;
    medium @1 :Int32;
    big    @2 :Int64;
    usmall @3 :UInt8;
    ubig   @4 :UInt64;
  }
""");
define msg = s->Numbers->build(
    "small", -42, "medium", 100000, "big", 999999999,
    "usmall", 255, "ubig", 12345678
);
define r = s->Numbers->read(msg);
r->small;     // -42
r->medium;    // 100000
r->big;       // 999999999
r->usmall;    // 255
r->ubig;      // 12345678
```

All integer widths are supported: Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64.

### Floats

```javascript
define s = Schema("""
  @0xa1b2c3d4e5f60718;
  struct Point {
    x @0 :Float64;
    y @1 :Float64;
  }
""");
define msg = s->Point->build("x", 3.14, "y", 2.72);
define r = s->Point->read(msg);
r->x;    // 3.14
r->y;    // 2.72
```

Both Float32 and Float64 are supported.

### Booleans

```javascript
define s = Schema("""
  @0xc3d4e5f60718293a;
  struct Flags {
    a @0 :Bool;
    b @1 :Bool;
  }
""");
define msg = s->Flags->build("a", true, "b", false);
define r = s->Flags->read(msg);
r->a;    // true
r->b;    // false
```

### Text and Data

Text fields are strings. Data fields are bytevectors.

```javascript
define s = Schema("""
  @0xdbb9ad1f14bf0b36;
  struct Person {
    name @0 :Text;
  }
""");
define msg = s->Person->build("name", "Alice");
define r = s->Person->read(msg);
r->name;    // "Alice"
```

### Lists

List fields serialize Eval lists to Cap'n Proto lists:

```javascript
define s = Schema("""
  @0xe5f60718293a4b5c;
  struct Scores {
    name   @0 :Text;
    values @1 :List(Int32);
  }
""");
define msg = s->Scores->build("name", "test", "values", [10, 20, 30]);
define r = s->Scores->read(msg);
r->name;      // "test"
r->values;    // [10, 20, 30]
```

List element types can be any supported primitive: `List(UInt8)`, `List(Float64)`, `List(Text)`, `List(Bool)`, etc.

### Enums

Enum values are read as symbols:

```
struct Status { status @0 :StatusEnum; }
enum StatusEnum { active @0; inactive @1; }
```

### Multiple Structs

A single schema can define multiple structs, all accessible from the same schema object:

```javascript
define s = Schema("""
  @0xd4e5f60718293a4b;
  struct Color {
    r @0 :UInt8;
    g @1 :UInt8;
    b @2 :UInt8;
  }
  struct Pixel {
    x @0 :UInt32;
    y @1 :UInt32;
  }
""");

define cm = s->Color->build("r", 255, "g", 128, "b", 0);
define pm = s->Pixel->build("x", 100, "y", 200);

define cr = s->Color->read(cm);
define pr = s->Pixel->read(pm);
cr->r;     // 255
pr->x;     // 100
```

## Schema Syntax

The full [Cap'n Proto schema language](https://capnproto.org/language.html) is supported, since parsing is done by Cap'n Proto's own `SchemaParser`. This includes:

- **File ID**: `@0xNNNNNNNNNNNNNNNN;` (required, unique per schema)
- **Structs**: `struct Name { field @N :Type; ... }`
- **Field numbering**: `@0`, `@1`, `@2`, ... (must be sequential, never reuse)
- **All primitives**: Void, Bool, Int8/16/32/64, UInt8/16/32/64, Float32/64, Text, Data
- **Lists**: `List(Type)` for any element type
- **Enums**: `enum Name { enumerant @N; ... }`
- **Nested structs**: struct definitions inside other structs
- **Default values**: `field @N :Type = default;`
- **Unions**: `union { field @N :Type; ... }`
- **Groups**: grouping related fields

## Memory-Mapped Files

Cap'n Proto's wire format is designed to be read directly from memory with no deserialization step. When you `mmap` a file, field reads are pointer arithmetic on the mapped region — this is the fastest possible way to load structured data.

### `schema->StructName->save(bytevector, filename)`

Writes a serialized message (from `build`) to a file:

```javascript
define s = Schema("""
  @0xdbb9ad1f14bf0b36;
  struct Person { name @0 :Text; age @1 :UInt32; }
""");

define msg = s->Person->build("name", "Alice", "age", 30);
s->Person->save(msg, "person.bin");
```

### `schema->StructName->mmap(filename)`

Memory-maps a file and returns a reader. No deserialization happens — the file's bytes are used directly:

```javascript
define r = s->Person->mmap("person.bin");
r->name;    // reads directly from mmap'd memory
r->age;     // pointer arithmetic on the mapped region
```

This is ideal for:
- **Large datasets** — only the pages you actually read are loaded from disk
- **Startup time** — no parsing, no allocation, instant access
- **Shared memory** — multiple processes can mmap the same file simultaneously
- **Database-style access** — random access to records without loading everything

### `reader->close()`

Explicitly releases the memory mapping (or heap-backed reader). Safe to call multiple times — second call is a no-op.

```javascript
define r = s->Person->mmap("person.bin");
r->name;      // "Alice"
r->close();   // release mmap immediately
```

On Windows, files cannot be deleted or overwritten while they are memory-mapped. Use `close()` or `with` to release the mapping when you're done.

### RAII with `with`

The `with` keyword automatically calls `->close()` on scope exit — even if an error is thrown:

```javascript
// Auto-closed when scope exits
define name = with(r = s->Person->mmap("person.bin")) {
    r->name
};
// r is already closed here, file is unmapped

// Also works with read (heap-backed)
define age = with(r = s->Person->read(msg)) {
    r->age
};

// Closes even on error (via dynamic-wind)
try {
    with(r = s->Person->mmap("person.bin")) {
        error("boom");
    };
} catch(e) display(e);
// r is closed despite the error
```

### Full Example

```javascript
define s = Schema("""
  @0xaabb112233445570;
  struct Employee {
    name   @0 :Text;
    age    @1 :UInt32;
    salary @2 :Float64;
    active @3 :Bool;
  }
""");

// Build and save
define msg = s->Employee->build(
    "name", "Bob", "age", 42,
    "salary", 75000.50, "active", true
);
s->Employee->save(msg, "employee.bin");

// Load via mmap with RAII — zero-copy, auto-cleanup
define result = with(r = s->Employee->mmap("employee.bin")) {
    display(r->name);      // "Bob"
    display(r->salary);    // 75000.5
    r->age
};
// File is unmapped here, result is 42
```

## How It Works

The implementation uses Cap'n Proto's **dynamic API** — the same C++ library that powers Cap'n Proto's own tools:

1. **Schema parsing**: `Schema(text)` passes the schema string to `capnp::SchemaParser`, which parses it into runtime type metadata. No code generation happens — the schema is interpreted at runtime.

2. **Building messages**: `->build(...)` creates a `capnp::MallocMessageBuilder`, initializes a `DynamicStruct::Builder` from the runtime schema, sets fields by name, and serializes to a flat byte array returned as an Eval bytevector.

3. **Reading messages**: `->read(bytes)` creates a `capnp::FlatArrayMessageReader` over the byte data and returns a `DynamicStruct::Reader` wrapper. Field access is pointer arithmetic on the wire format — no deserialization or copying.

The binary format is Cap'n Proto's standard wire format. Messages produced by Eval can be read by any Cap'n Proto implementation (C++, Rust, Go, Java, Python, etc.) and vice versa.

## Wire Compatibility

Because Eval uses Cap'n Proto's own C++ library for both reading and writing, the output is guaranteed wire-compatible. You can:

- Build a message in Eval, send it over TCP (see [NETWORKING.md](NETWORKING.md)), and read it in a Go server
- Read messages produced by a Rust service into Eval for analysis
- Exchange data between Eval thread pool workers via channels using binary format for efficiency
- Store serialized messages to files and read them back later

## Comparison with JSON

| | Cap'n Proto | JSON |
|---|---|---|
| **Format** | Binary, schema-typed | Text, schema-less |
| **Size** | Compact (no field names in wire format) | Verbose (field names repeated) |
| **Speed** | Zero-copy reads, no parsing | Must parse entire document |
| **Schema** | Required, enforced at build time | Optional, no enforcement |
| **Compatibility** | Cross-language via schema | Universal text format |
| **Use case** | High-throughput IPC, storage, networking | Config files, REST APIs, debugging |

## Optional Feature

Cap'n Proto support is optional — it requires the `capnproto` vcpkg package. The build system detects it automatically:

```
Found Cap'n Proto — binary serialization enabled    (capnproto installed)
Cap'n Proto not found — serialization disabled      (capnproto not installed)
```

All Cap'n Proto code is conditionally compiled behind `EVAL_HAVE_CAPNP`. The rest of Eval works identically with or without it.

## See also

- [ARROW.md](ARROW.md) — Apache Arrow columnar data: tables, filter/sort/group-by, CSV/Parquet/IPC I/O
- [GRAMMARS.md](GRAMMARS.md) — Grammar JIT: another compile-time feature for runtime parser generation
- [NETWORKING.md](NETWORKING.md) — TCP sockets and HTTP for sending serialized data over the wire
- [ASYNC.md](ASYNC.md) — Thread pools and channels for parallel processing of binary messages
- [THREADS.md](THREADS.md) — Green threads and concurrency
