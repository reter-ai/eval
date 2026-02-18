# Filesystem

Eval provides comprehensive filesystem access through chibi-scheme's filesystem and I/O libraries. Functions are available with underscore aliases (e.g., `file_size`) or via backtick identifiers for hyphenated Scheme names (e.g., `` `file-size` ``).

## Reading files

### Read an entire file into a string

```
define content = file_to_string("data.txt");
display(content);
```

### Read an entire file into a bytevector

```
define bytes = file_to_bytevector("image.png");
`bytevector-length`(bytes);    // file size in bytes
```

### Read with ports (streaming)

Open a file, read from it, close it:

```
define port = `open-input-file`("data.txt");
define line = read_line(port);          // read one line
define chunk = read_string(1024, port); // read up to 1024 chars
`close-input-port`(port);
```

The safer pattern uses `call-with-input-file` which auto-closes on exit or error:

```
`call-with-input-file`("data.txt", function(port) {
    port_to_string(port);
});
```

### Read line by line

```
define port = `open-input-file`("log.txt");
define line = read_line(port);
while(line != `eof-object`(line)) {
    display(line); newline();
    line = read_line(port);
};
`close-input-port`(port);
```

Or with the port-fold pattern:

```
`call-with-input-file`("log.txt", function(port) {
    `port-fold`(function(line, count) {
        display(`number->string`(count) ++ ": " ++ line);
        newline();
        count + 1;
    }, 1, function() read_line(port));
});
```

## Writing files

### Write a string to a file

```
define port = `open-output-file`("output.txt");
write_string("Hello, world!\n", port);
write_line("Second line", port);     // appends newline
`close-output-port`(port);
```

### Safe writing with auto-close

```
`call-with-output-file`("output.txt", function(port) {
    write_string("Hello, world!\n", port);
    write_line("Done", port);
});
```

### Append to a file

```
define port = `open-output-file/append`("log.txt");
write_line("new entry", port);
`close-output-port`(port);
```

## Directory operations

### List directory contents

```
define files = directory_files("src");
// => [".", "..", "main.c", "utils.c", "lib"]
```

Note: `directory_files` includes `.` and `..`. Filter them out:

```
define files = filter(
    function(f) f != "." && f != "..",
    directory_files("src")
);
```

### Filter by file type

```
define dir = "src";
define entries = directory_files(dir);

// Only regular files (not directories)
define files = filter(
    function(f) f != "." && f != ".." && file_regular(dir ++ "/" ++ f),
    entries
);

// Only subdirectories
define dirs = filter(
    function(f) f != "." && f != ".." && file_directory(dir ++ "/" ++ f),
    entries
);
```

### Create directories

```
`create-directory`("output");                // single directory
create_directory_star("path/to/nested");     // create parents as needed
```

### Delete files and directories

```
delete_file("temp.txt");                       // delete a file
`delete-directory`("empty_dir");               // delete empty directory
delete_file_hierarchy("build");                // recursive delete (rm -rf)
```

### Current directory

```
define cwd = current_directory();
display(cwd);

change_directory("/tmp");
// ... work in /tmp ...
change_directory(cwd);        // go back
```

Or use `with_directory` for automatic restore:

```
with_directory("/tmp", function() {
    // current directory is /tmp here
    define files = directory_files(".");
    display(files);
});
// automatically restored to previous directory
```

### Rename files

```
`rename-file`("old_name.txt", "new_name.txt");
```

## File metadata

### File size

```
file_size("data.txt");    // size in bytes (integer)
```

### File existence and type

```
file_exists("data.txt");      // #t or #f
file_regular("data.txt");     // true if regular file
file_directory("src");         // true if directory
```

Additional type tests via backtick:

```
`file-link?`("symlink");        // symbolic link
`file-fifo?`("pipe");           // named pipe
`file-socket?`("unix.sock");    // Unix domain socket
`file-character?`("/dev/tty");  // character device
`file-block?`("/dev/sda");      // block device
```

### Timestamps

```
file_modification_time("data.txt");   // last modified (epoch seconds)
file_access_time("data.txt");         // last accessed (epoch seconds)
```

### Full stat information

For detailed metadata, use the stat functions:

```
define st = `file-status`("data.txt");

`file-size`(st);               // size in bytes
`file-mode`(st);               // permission bits
`file-owner`(st);              // owner UID
`file-group`(st);              // group GID
`file-num-links`(st);          // hard link count
`file-inode`(st);              // inode number
`file-device`(st);             // device ID
`file-modification-time`(st);  // mtime
`file-access-time`(st);        // atime
`file-change-time`(st);        // ctime
```

All the file type and size functions accept either a path string or a stat object, so you can reuse the stat result:

```
define st = `file-status`("data.txt");
file_size(st);           // no extra syscall
file_regular(st);        // no extra syscall
```

### Permission checks

```
`file-is-readable?`("data.txt");     // can current user read?
`file-is-writable?`("data.txt");     // can current user write?
`file-is-executable?`("script.sh");  // can current user execute?
```

### File permissions

```
`chmod`("script.sh", 493);    // 493 = #o755 (rwxr-xr-x)
```

## Symbolic links

```
`symbolic-link-file`("target.txt", "link.txt");  // create symlink
read_link("link.txt");                            // => "target.txt"
`file-link?`("link.txt");                         // => true
```

Note: symbolic links are not supported on Windows (`read_link` returns `#f`, `` `file-link?` `` returns `#f`).

## Recursive directory traversal

### Using `directory-fold-tree`

Walk an entire directory tree with callbacks:

```
`directory-fold-tree`("src",
    function(dir, acc) acc,                        // down: entering directory
    function(dir, acc) acc,                        // up: leaving directory
    function(file, acc) { display(file); newline(); acc + 1; },  // here: each file
    0                                              // initial accumulator
);
```

### Manual recursion

```
define walk = function(dir) {
    define entries = filter(
        function(f) f != "." && f != "..",
        directory_files(dir)
    );
    `for-each`(function(f) {
        define path = dir ++ "/" ++ f;
        if(file_directory(path)) {
            display("[dir]  " ++ path); newline();
            walk(path);
        } else {
            display("[file] " ++ path); newline();
        };
    }, entries);
};

walk("src");
```

## Path utilities

The `(chibi pathname)` library provides pure-string path manipulation (no filesystem access):

```
`path-directory`("src/main.c");          // => "src"
`path-strip-directory`("src/main.c");    // => "main.c"
`path-extension`("main.c");             // => "c"
`path-strip-extension`("main.c");       // => "main"
`path-replace-extension`("main.c", "o");// => "main.o"
`path-absolute?`("/usr/bin");            // => true
`path-relative?`("src/main.c");          // => true
`make-path`("src", "main.c");           // => "src/main.c"
`path-normalize`("src//./main.c");       // => "src/main.c"
```

## Environment variables

```
`get-environment-variable`("HOME");      // => "/home/user" or #f
`get-environment-variables`();           // => (("HOME" . "/home/user") ...)
```

Or use the underscore alias:

```
get_env("PATH");    // => path string or false
```

## String ports

Create ports that read from or write to strings in memory:

```
// Read from a string as if it were a file
define port = `open-input-string`("line 1\nline 2\nline 3");
read_line(port);    // => "line 1"
read_line(port);    // => "line 2"

// Write to a string buffer
define out = `open-output-string`();
write_string("hello ", out);
write_string("world", out);
`get-output-string`(out);    // => "hello world"
```

## Bytevector ports

```
define port = `open-input-bytevector`(#u8(72 101 108 108 111));
read_string(5, port);    // => "Hello"

define out = `open-output-bytevector`();
write_string("test", out);
`get-output-bytevector`(out);    // => bytevector
```

## File position (seeking)

```
define port = `open-input-file`("data.bin");
`file-position`(port);                    // => 0
read_string(10, port);
`file-position`(port);                    // => 10
`set-file-position!`(port, 0, `seek/set`);  // rewind to start
`set-file-position!`(port, -5, `seek/end`); // 5 bytes before end
`close-input-port`(port);
```

## Practical example: file scanner

This example scans a directory, categorizes files by extension, and reports statistics. It uses indexing for clean data access:

```
define scan_dir = function(dir) {
    define files = filter(
        function(f) f != "." && f != ".." && file_regular(dir ++ "/" ++ f),
        directory_files(dir)
    );

    // Build [name, size, extension] tuples
    define details = map(function(f) {
        define path = dir ++ "/" ++ f;
        define ext = `path-extension`(f);
        [f, file_size(path), if(ext) ext else ""];
    }, files);

    // Find largest file
    define largest = if(length(details) == 0) false
        else fold(function(d, best)
            if(d[1] > best[1]) d else best,
            details[0], details[1:]);

    // Total size
    define total = fold(function(d, acc) acc + d[1], 0, details);

    display("Directory: " ++ dir); newline();
    display("Files: " ++ `number->string`(length(details))); newline();
    display("Total size: " ++ `number->string`(total) ++ " bytes"); newline();
    when(largest) {
        display("Largest: " ++ largest[0]
            ++ " (" ++ `number->string`(largest[1]) ++ " bytes)");
        newline();
    };
};

scan_dir("src");
```

See [`examples/file_processor.eval`](examples/file_processor.eval) for a full reactive file processing pipeline that uses signals and computed values to reactively update when the target directory or filter changes.

## Function reference

### File reading

| Function | Description |
|----------|-------------|
| `file_to_string(path)` | Read entire file into string |
| `file_to_bytevector(path)` | Read entire file into bytevector |
| `` `open-input-file`(path) `` | Open file for reading, returns input port |
| `read_string(n, port)` | Read up to n characters from port |
| `read_line(port)` | Read one line (strips newline) |
| `port_to_string(port)` | Read entire port into string |
| `` `call-with-input-file`(path, proc) `` | Open, call proc(port), auto-close |

### File writing

| Function | Description |
|----------|-------------|
| `` `open-output-file`(path) `` | Open file for writing, returns output port |
| `` `open-output-file/append`(path) `` | Open file for appending |
| `write_string(str, port)` | Write string to port |
| `write_line(str, port)` | Write string + newline to port |
| `` `call-with-output-file`(path, proc) `` | Open, call proc(port), auto-close |

### Port management

| Function | Description |
|----------|-------------|
| `` `close-input-port`(port) `` | Close input port |
| `` `close-output-port`(port) `` | Close output port |
| `` `file-position`(port) `` | Get current position |
| `` `set-file-position!`(port, pos, whence) `` | Seek to position |

### Directory operations

| Function | Description |
|----------|-------------|
| `directory_files(dir)` | List directory contents (includes `.` and `..`) |
| `directory_fold(dir, kons, knil)` | Fold over directory entries |
| `` `directory-fold-tree`(dir, down, up, here, knil) `` | Recursive tree traversal |
| `current_directory()` | Get working directory |
| `change_directory(dir)` | Change working directory |
| `with_directory(dir, thunk)` | Temporarily change directory |
| `` `create-directory`(dir) `` | Create directory |
| `create_directory_star(dir)` | Create directory and parents |
| `` `delete-directory`(dir) `` | Remove empty directory |
| `delete_file(path)` | Delete file |
| `delete_file_hierarchy(dir)` | Recursive delete |
| `` `rename-file`(old, new) `` | Rename/move file |

### File metadata

| Function | Description |
|----------|-------------|
| `file_exists(path)` | Check if file exists |
| `file_regular(path)` | Check if regular file |
| `file_directory(path)` | Check if directory |
| `file_size(path)` | File size in bytes |
| `file_modification_time(path)` | Last modified (epoch seconds) |
| `file_access_time(path)` | Last accessed (epoch seconds) |
| `` `file-status`(path) `` | Full stat structure |
| `` `file-is-readable?`(path) `` | Check read permission |
| `` `file-is-writable?`(path) `` | Check write permission |
| `` `file-is-executable?`(path) `` | Check execute permission |

### Symbolic links

| Function | Description |
|----------|-------------|
| `` `symbolic-link-file`(target, link) `` | Create symbolic link |
| `read_link(path)` | Read symlink target |
| `` `file-link?`(path) `` | Check if symbolic link |

### String/Bytevector conversion

| Function | Description |
|----------|-------------|
| `string_to_utf8(str)` | String to UTF-8 bytevector |
| `utf8_to_string(bv)` | UTF-8 bytevector to string |

### Path utilities (`(chibi pathname)`)

| Function | Description |
|----------|-------------|
| `` `path-directory`(path) `` | Directory component |
| `` `path-strip-directory`(path) `` | Filename component |
| `` `path-extension`(path) `` | File extension |
| `` `path-strip-extension`(path) `` | Path without extension |
| `` `path-replace-extension`(path, ext) `` | Replace extension |
| `` `path-absolute?`(path) `` | Is absolute path? |
| `` `make-path`(parts...) `` | Join path components |
| `` `path-normalize`(path) `` | Normalize path |

## See also

- [NETWORKING.md](NETWORKING.md) — TCP sockets and HTTP with OO wrappers (`TcpSocket`, `TcpServer`, `HttpClient`), RAII, and reactive signals
