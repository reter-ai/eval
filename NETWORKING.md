# Networking

Eval provides a complete networking stack built on chibi-scheme's socket library. All socket operations integrate with green threads — `accept`, `connect`, `send`, and `recv` are non-blocking and automatically yield to the scheduler when they would block, so you can handle thousands of connections in a single thread.

## Quick start: TCP client

```
// OO wrapper: connect, send, receive, auto-close
with(sock = TcpClient("example.com", 80)) {
    sock->send("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
    define response = sock->recv(4096);
    display(response);
};
// sock->close() called automatically by `with`
```

## Quick start: TCP server

```
// OO server: listen, accept, respond — each connection in a green thread
define server = TcpServer(8080, function(sock, addr, port) {
    sock->send("Hello!\n");
});
server->run();   // blocking accept loop (sock auto-closed per connection)
```

## Quick start: HTTP client

```
// OO HTTP client with response parsing
with(client = HttpClient("example.com", 80)) {
    define result = client->get("/");
    define status = car(result);     // 200
    define body = car(cdr(result));  // HTML content
    display(body);
};
```

## OO wrappers

Eval provides built-in OO wrappers for common networking patterns. These handle string/bytevector conversion, RAII cleanup, and integrate with reactive signals.

### TcpSocket

Wraps a raw socket file descriptor with string-based send/recv and RAII:

```
define sock = TcpSocket(fd);    // wrap an existing fd
sock->send("hello");            // sends string (auto-converts to bytevector)
sock->send(bytevector);         // or send raw bytes
sock->recv(4096);               // returns string or false (EOF)
sock->recv_bytes(4096);         // returns raw bytevector or false
sock->fd;                       // underlying file descriptor
sock->open?;                    // true if not yet closed
sock->close();                  // close socket (idempotent)
sock->dispose();                // alias for close (RAII)
```

Works with `with` for automatic cleanup:

```
with(sock = TcpSocket(fd)) {
    sock->send("data");
};
// sock->close() called automatically
```

### TcpClient

Connects to a host:port and returns a `TcpSocket`:

```
define sock = TcpClient("example.com", 80);
sock->send("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
define response = sock->recv(4096);
sock->close();

// Or with RAII:
with(sock = TcpClient("localhost", 8080)) {
    sock->send("hello");
    sock->recv(4096);
};
```

### TcpServer

Listener with green-thread-per-connection dispatch, RAII, and reactive signals:

```
define server = TcpServer(8080, function(sock, addr, port) {
    // sock is a TcpSocket, auto-closed when handler returns
    define data = sock->recv(4096);
    sock->send("HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK");
});

server->run();      // blocking accept loop
// or
server->start();    // non-blocking (runs in a green thread)
```

The handler receives three arguments: the `TcpSocket`, the client IP address string, and the client port number.

| Method | Description |
|--------|-------------|
| `server->run()` | Blocking accept loop |
| `server->start()` | Start in a green thread (non-blocking) |
| `server->stop()` | Stop accepting and close listener |
| `server->close()` | Alias for stop (RAII) |
| `server->dispose()` | Alias for stop (RAII) |
| `server->connections` | Reactive `Signal` — current active connection count |
| `server->requests` | Reactive `Signal` — total requests served |
| `server->port` | Port number |
| `server->running?` | Whether the server is running |

Reactive signals update automatically as connections arrive and complete:

```
define server = TcpServer(8080, handler);

Effect(function() {
    display("[server] connections=" ++ `number->string`(server->connections())
        ++ " requests=" ++ `number->string`(server->requests()));
    newline();
});

server->run();
```

### HttpClient

OO HTTP client with `get` and `post` methods. Each request returns `[status, body]`:

```
with(client = HttpClient("example.com", 80)) {
    define result = client->get("/");
    define status = car(result);     // 200
    define body = car(cdr(result));  // response body
};
```

| Method | Description |
|--------|-------------|
| `client->get(path)` | GET request, returns `[status, body]` |
| `client->post(path, body)` | POST request, returns `[status, body]` |
| `client->host` | Host string |
| `client->port` | Port number |
| `client->close()` | Close underlying connection (RAII) |

```
define client = HttpClient("api.example.com", 80);
define result = client->get("/api/data");
define status = car(result);
when(status == 200)
    display(car(cdr(result)));
client->close();

// POST with body
define result = client->post("/api/echo", "hello world");
```

## Low-level API

The OO wrappers are built on these low-level functions, which you can use directly for full control.

### Helper functions

Working with raw sockets involves converting between strings and bytevectors. These helpers make it more convenient:

```
define tcp_connect = function(host, port) {
    define ai = get_address_info(host, `number->string`(port));
    define sock = socket(address_info_family(ai),
                         address_info_socket_type(ai),
                         address_info_protocol(ai));
    connect(sock, address_info_address(ai), address_info_address_length(ai));
    sock;
};

define tcp_send = function(sock, str) {
    send(sock, string_to_utf8(str));
};

define tcp_recv = function(sock, n) {
    define bv = recv(sock, n);
    if(bv == false) "" else utf8_to_string(bv);
};
```

These are defined in [`examples/http_client.eval`](examples/http_client.eval) and can be included:

```
include("examples/http_client.eval");
```

## DNS resolution

`get_address_info` resolves hostnames to addresses. It wraps POSIX `getaddrinfo`:

```
define ai = get_address_info("example.com", "443");

// Walk the result chain
address_info_family(ai);          // AF_INET or AF_INET6
address_info_socket_type(ai);     // SOCK_STREAM
address_info_protocol(ai);        // IPPROTO_TCP
address_info_address(ai);         // sockaddr pointer (for connect/bind)
address_info_address_length(ai);  // sockaddr length

// Multiple results — iterate
define next = address_info_next(ai);
```

You can also construct address info manually with hints:

```
define hints = make_address_info(
    address_family_inet,       // AF_INET only
    socket_type_stream,        // TCP
    ip_proto_tcp               // TCP protocol
);
define ai = get_address_info("example.com", "80", hints);
```

## Socket creation

```
define sock = socket(family, socktype, protocol);
```

Typically you get these values from `get_address_info`:

```
define ai = get_address_info(host, port);
define sock = socket(address_info_family(ai),
                     address_info_socket_type(ai),
                     address_info_protocol(ai));
```

## Connecting

```
connect(sock, address_info_address(ai), address_info_address_length(ai));
```

When green threads are enabled, `connect` is non-blocking. If the connection is in progress, the green thread yields and resumes when the connection completes.

## Sending and receiving

```
send(sock, bytevector);          // returns bytes sent
define bv = recv(sock, max_bytes);  // returns bytevector or #f
```

Both `send` and `recv` work with bytevectors. Convert to/from strings with:

```
string_to_utf8("hello")         // string -> bytevector
utf8_to_string(bv)              // bytevector -> string
```

`recv` returns `#f` when the connection is closed or would block without data.

## Listening and accepting

For servers, the high-level `make_listener_socket` handles socket creation, `SO_REUSEADDR`, bind, and listen in one call:

```
define ai = get_address_info("0.0.0.0", "8080");
define listener = make_listener_socket(ai);
```

This is equivalent to:

```
define ai = get_address_info("0.0.0.0", "8080");
define sock = socket(address_info_family(ai),
                     address_info_socket_type(ai),
                     address_info_protocol(ai));
set_socket_option(sock, `level/socket`, socket_opt_reuseaddr, 1);
bind(sock, address_info_address(ai), address_info_address_length(ai));
listen(sock, 128);
```

Accept incoming connections:

```
define sa = make_sockaddr();
define conn = accept(listener, sa, 16);
```

`accept` returns a new socket for the connection. When green threads are enabled, it yields if no connection is pending.

After accepting, you can inspect the client address:

```
sockaddr_name(sa);    // IP address string, e.g. "192.168.1.100"
sockaddr_port(sa);    // port number
```

## Socket options

```
// Enable address reuse (for server restart)
set_socket_option(sock, `level/socket`, socket_opt_reuseaddr, 1);

// Read current option value
get_socket_option(sock, `level/socket`, socket_opt_reuseaddr);
```

## Non-blocking I/O with green threads

All socket operations automatically work with green threads. When a socket operation would block (e.g., no data available on `recv`, connection queue full on `accept`), the green thread yields to the scheduler and resumes when the socket is ready.

This means you can write blocking-style code that handles concurrency:

```
// Each connection runs in its own green thread
define handle = function(conn) {
    define data = recv(conn, 4096);            // yields if no data
    send(conn, string_to_utf8("response"));    // yields if buffer full
    close_file_descriptor(conn);
};

while(running) {
    define conn = accept(listener, sa, 16);    // yields if no connection
    thread_start(make_thread(function() handle(conn)));
};
```

No explicit async/await or callbacks needed — the VM scheduler handles everything.

## HTTP client

Using the OO `HttpClient` wrapper:

```
with(client = HttpClient("example.com", 80)) {
    define result = client->get("/");
    define status = car(result);
    define body = car(cdr(result));
    display(body);
};
```

Or build an HTTP client manually on top of the raw socket API:

```
define http_get = function(host, port, path) {
    define sock = tcp_connect(host, port);
    tcp_send(sock, "GET " ++ path ++ " HTTP/1.0\r\n"
        ++ "Host: " ++ host ++ "\r\n"
        ++ "Connection: close\r\n"
        ++ "\r\n");

    // Read response
    define buf = "";
    define chunk = tcp_recv(sock, 4096);
    while(chunk != "") {
        buf = buf ++ chunk;
        chunk = tcp_recv(sock, 4096);
    };
    close_file_descriptor(sock);
    buf;
};

define response = http_get("example.com", 80, "/");
display(response);
```

A more complete implementation with status code parsing and Content-Length handling is in [`examples/http_client.eval`](examples/http_client.eval).

## HTTP server

Using the OO `TcpServer` wrapper:

```
define server = TcpServer(8080, function(sock, addr, port) {
    define data = sock->recv(8192);
    define body = "Hello from Eval!";
    sock->send("HTTP/1.0 200 OK\r\n"
        ++ "Content-Type: text/plain\r\n"
        ++ "Content-Length: " ++ `number->string`(`string-length`(body)) ++ "\r\n"
        ++ "\r\n"
        ++ body);
});
display("Listening on port 8080\n");
server->run();
```

Or build a server manually with the raw socket API:

```
define ai = get_address_info("0.0.0.0", "8080");
define listener = make_listener_socket(ai);
display("Listening on port 8080\n");

while(true) {
    define sa = make_sockaddr();
    define conn = accept(listener, sa, 16);

    thread_start(make_thread(function() {
        define bv = recv(conn, 4096);
        define data = if(bv) utf8_to_string(bv) else "";

        define body = "Hello from Eval!";
        define response = "HTTP/1.0 200 OK\r\n"
            ++ "Content-Type: text/plain\r\n"
            ++ "Content-Length: " ++ `number->string`(`string-length`(body)) ++ "\r\n"
            ++ "\r\n"
            ++ body;

        send(conn, string_to_utf8(response));
        close_file_descriptor(conn);
    }));
};
```

A full-featured HTTP server with routing, reactive state, JSON responses, POST handling, and graceful shutdown is in [`examples/http_server.eval`](examples/http_server.eval).

### Route dispatch pattern

The HTTP server example uses a route table with indexing:

```
// Route table: [method-or-false, path, handler]
define routes = [
    ["GET",  "/",          handle_index],
    ["GET",  "/api/count", handle_count],
    [false,  "/api/echo",  handle_echo]    // false = any method
];

define find_route = function(method, path) {
    define result = false;
    define r = routes;
    while(r != []) {
        define route = r[0];
        when((route[0] == false || route[0] == method) && route[1] == path)
            result = route[2];
        r = cdr(r);
    };
    result;
};
```

### Request parsing

Parse an HTTP request into `[method, path, body]`:

```
define parse_request = function(data) {
    define header_end = string_index(data, "\r\n\r\n");
    define headers_str = if(header_end) data[:header_end] else data;
    define body = if(header_end) data[header_end + 4:] else "";

    define first_line_end = string_index(headers_str, "\r\n");
    define request_line = if(first_line_end) headers_str[:first_line_end]
                          else headers_str;

    define sp1 = string_index(request_line, " ");
    define method = if(sp1) request_line[:sp1] else "GET";
    define rest = if(sp1) request_line[sp1 + 1:] else "/";
    define sp2 = string_index(rest, " ");
    define path = if(sp2) rest[:sp2] else rest;

    [method, path, body];
};
```

## Server in a thread pool

For testing or when the server needs to run alongside client code, put it in a thread pool worker. Use a channel to synchronize startup:

```
define pool = make_pool(1);
define ready_ch = pool_channel(pool, "ready");

define server_future = pool_submit(pool, "
    define ai = get_address_info(\"0.0.0.0\", \"9090\");
    define listener = make_listener_socket(ai);
    channel_send(ready, true);

    define sa = make_sockaddr();
    define conn = accept(listener, sa, 16);
    define bv = recv(conn, 4096);
    send(conn, string_to_utf8(\"HTTP/1.0 200 OK\\r\\nContent-Length: 2\\r\\n\\r\\nOK\"));
    close_file_descriptor(conn);
    close_file_descriptor(listener);
    \"done\";
");

// Wait for server to be ready, then make requests
channel_recv(ready_ch);

define sock = tcp_connect("127.0.0.1", 9090);
tcp_send(sock, "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n");
define response = tcp_recv(sock, 4096);
close_file_descriptor(sock);

define server_result = future_result(server_future);
pool_shutdown(pool);
```

## Reactive server state

`TcpServer` has built-in reactive signals for connection and request tracking:

```
define server = TcpServer(8080, function(sock, addr, port) {
    define data = sock->recv(4096);
    sock->send("OK");
});

// React to server state changes
Effect(function() {
    display("[server] connections=" ++ `number->string`(server->connections())
        ++ " requests=" ++ `number->string`(server->requests()));
    newline();
});

server->run();
```

For more granular control, combine raw sockets with custom signals:

```
define request_count = Signal(0);
define active_conns = Signal(0);
define last_path = Signal("");

Effect(function() {
    when(request_count() > 0)
        display("[server] Requests: " ++ `number->string`(request_count())
            ++ "  Active: " ++ `number->string`(active_conns()));
    newline();
});

define handle_connection = function(conn) {
    active_conns->update(function(n) n + 1);
    try {
        // ... handle request ...
        batch(function() {
            request_count->update(function(n) n + 1);
            last_path->set(path);
        });
    } catch(err) {
        // error handling
    };
    active_conns->update(function(n) n - 1);
};
```

## Synchronization with networking

When multiple green threads access shared state (e.g., server request handlers modifying a shared cache), use OO synchronization wrappers for safe coordination:

```
define cache = dict();
define rwl = ReadWriteLock();

define server = TcpServer(8080, function(sock, addr, port) {
    define data = sock->recv(4096);
    define key = parse_key(data);

    // Multiple readers can look up the cache concurrently
    define value = with(guard = rwl->read_lock()) {
        cache->get(key);
    };

    when(!value) {
        // Exclusive write access to update the cache
        with(guard = rwl->write_lock()) {
            cache->set(key, compute_value(key));
        };
    };

    sock->send("OK");
});
```

See [MULTITHREADING.md](MULTITHREADING.md) for the complete guide to `Mutex`, `Monitor`, `ReadWriteLock`, and `Semaphore`.

## Constants reference

### Address families

| Eval name | Scheme name | Value |
|-----------|-------------|-------|
| `address_family_unspecified` | `address-family/unspecified` | AF_UNSPEC |
| `address_family_inet` | `address-family/inet` | AF_INET |
| `address_family_inet6` | `address-family/inet6` | AF_INET6 |
| `address_family_unix` | `address-family/unix` | AF_UNIX |

### Socket types

| Eval name | Scheme name | Value |
|-----------|-------------|-------|
| `socket_type_stream` | `socket-type/stream` | SOCK_STREAM (TCP) |
| `socket_type_datagram` | `socket-type/datagram` | SOCK_DGRAM (UDP) |
| `socket_type_raw` | `socket-type/raw` | SOCK_RAW |

### Protocols

| Eval name | Scheme name | Value |
|-----------|-------------|-------|
| `ip_proto_ip` | `ip-proto/ip` | IPPROTO_IP |
| `ip_proto_tcp` | `ip-proto/tcp` | IPPROTO_TCP |
| `ip_proto_udp` | `ip-proto/udp` | IPPROTO_UDP |

### Socket options

| Eval name | Scheme name |
|-----------|-------------|
| `socket_opt_reuseaddr` | `socket-opt/reuseaddr` |

Other options are available via backtick: `` `socket-opt/keepalive` ``, `` `socket-opt/broadcast` ``, `` `socket-opt/sndbuf` ``, `` `socket-opt/rcvbuf` ``.

## Function reference

### Connection

| Function | Description |
|----------|-------------|
| `get_address_info(host, port)` | DNS/address resolution. Returns address info chain. |
| `make_address_info(family, socktype, proto)` | Create address info with hints. |
| `socket(family, socktype, proto)` | Create a socket. Returns file descriptor. |
| `connect(sock, addr, addrlen)` | Connect to remote host. Non-blocking with green threads. |
| `make_listener_socket(addrinfo)` | Create, bind, and listen in one call. Sets `SO_REUSEADDR`. |
| `bind(sock, addr, addrlen)` | Bind socket to address. |
| `listen(sock, backlog)` | Mark socket as listening. Default backlog: 128. |
| `accept(listener, sockaddr, addrlen)` | Accept incoming connection. Non-blocking with green threads. |

### Data transfer

| Function | Description |
|----------|-------------|
| `send(sock, bytevector)` | Send data. Returns bytes sent. Non-blocking with green threads. |
| `recv(sock, max_bytes)` | Receive data. Returns bytevector or `#f`. Non-blocking with green threads. |
| `string_to_utf8(str)` | Convert string to bytevector for sending. |
| `utf8_to_string(bv)` | Convert received bytevector to string. |

### Address inspection

| Function | Description |
|----------|-------------|
| `address_info_family(ai)` | Address family (AF_INET, etc.) |
| `address_info_socket_type(ai)` | Socket type (SOCK_STREAM, etc.) |
| `address_info_protocol(ai)` | Protocol (IPPROTO_TCP, etc.) |
| `address_info_address(ai)` | Sockaddr pointer for connect/bind |
| `address_info_address_length(ai)` | Sockaddr length |
| `address_info_next(ai)` | Next address in chain |
| `make_sockaddr()` | Allocate empty sockaddr (for accept) |
| `sockaddr_name(sa)` | IP address string |
| `sockaddr_port(sa)` | Port number |

### Socket management

| Function | Description |
|----------|-------------|
| `close_file_descriptor(sock)` | Close socket |
| `set_socket_option(sock, level, opt, val)` | Set socket option |
| `get_socket_option(sock, level, opt)` | Get socket option |
| `get_peer_name(sock, sockaddr)` | Get remote address of connected socket |

## OO wrapper reference

### TcpSocket

| Method | Description |
|--------|-------------|
| `TcpSocket(fd)` | Wrap an existing socket file descriptor |
| `sock->send(data)` | Send string or bytevector |
| `sock->recv(n)` | Receive up to n bytes as string, or `false` on EOF |
| `sock->recv_bytes(n)` | Receive up to n bytes as bytevector, or `false` on EOF |
| `sock->close()` | Close socket (idempotent) |
| `sock->dispose()` | Alias for close (RAII) |
| `sock->fd` | Underlying file descriptor |
| `sock->open?` | Whether socket is open |

### TcpClient

| Constructor | Description |
|------------|-------------|
| `TcpClient(host, port)` | Connect to host:port, returns a `TcpSocket` |

### TcpServer

| Method | Description |
|--------|-------------|
| `TcpServer(port, handler)` | Create server. Handler receives `(sock, addr, port)`. |
| `server->run()` | Blocking accept loop |
| `server->start()` | Start in a green thread (non-blocking) |
| `server->stop()` | Stop accepting and close listener |
| `server->close()` | Alias for stop (RAII) |
| `server->connections` | Reactive Signal — current active connections |
| `server->requests` | Reactive Signal — total requests served |
| `server->port` | Port number |
| `server->running?` | Whether the server is running |

### HttpClient

| Method | Description |
|--------|-------------|
| `HttpClient(host, port)` | Create client for host:port |
| `client->get(path)` | GET request, returns `[status, body]` |
| `client->post(path, body)` | POST request, returns `[status, body]` |
| `client->host` | Host string |
| `client->port` | Port number |
| `client->close()` | Close connection (RAII) |

## Examples

| File | Description |
|------|-------------|
| [`examples/http_server.eval`](examples/http_server.eval) | Full HTTP server with routing, reactive state, green threads, graceful shutdown |
| [`examples/http_client.eval`](examples/http_client.eval) | HTTP client helpers: `tcp_connect`, `tcp_send`, `tcp_recv`, `http_get`, `http_post`, plus `HttpClient` OO wrapper |
| [`tests/eval/test_http.eval`](tests/eval/test_http.eval) | Server/client integration test: GET, POST, 404, JSON, pool worker |
| [`tests/eval/test_nb.eval`](tests/eval/test_nb.eval) | Non-blocking socket test |
