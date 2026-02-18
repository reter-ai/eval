# Green Threads in Chibi-Scheme: A Deep Dive

This document explains how green threads work at the lowest level in chibi-scheme, the Scheme implementation embedded in Eval. Green threads provide cooperative multitasking within a single OS thread — multiple logical threads of execution share one native thread, with the VM scheduler switching between them.

## 1. The Big Picture

```
┌─────────────────────────────────────────────────────┐
│                  One OS Thread                      │
│                                                     │
│  ┌─────────────────────────────────────────────┐    │
│  │            Bytecode VM (vm.c)               │    │
│  │                                             │    │
│  │  fuel counter: 500 instructions per slice   │    │
│  │                                             │    │
│  │  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐   │    │
│  │  │ ctx0 │  │ ctx1 │  │ ctx2 │  │ ctx3 │   │    │
│  │  │(root)│  │(thrd)│  │(thrd)│  │(thrd)│   │    │
│  │  │stack │  │stack │  │stack │  │stack │   │    │
│  │  │ip    │  │ip    │  │ip    │  │ip    │   │    │
│  │  │dk    │  │dk    │  │dk    │  │dk    │   │    │
│  │  │params│  │params│  │params│  │params│   │    │
│  │  └──────┘  └──────┘  └──────┘  └──────┘   │    │
│  │       ▲         │          │         │     │    │
│  │       └─────────┴──────────┴─────────┘     │    │
│  │              round-robin queue              │    │
│  │                                             │    │
│  │  Scheduler: sexp_scheduler() in threads.c   │    │
│  └─────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────┘
```

Every green thread **is** a `sexp` context object. The VM runs one context at a time. After every 500 bytecode instructions, the scheduler picks the next runnable context, the VM restores that context's registers, and execution continues seamlessly.

## 2. Thread = Context

A green thread is literally a chibi-scheme context (`SEXP_CONTEXT` type). Every context carries the full machine state needed to resume execution:

```
sexp_context fields (sexp.h:1403-1443):
├── stack       — own value stack (array of sexp)
├── ip          — instruction pointer into bytecode
├── proc        — current procedure being executed
├── last_fp     — frame pointer (index into stack)
├── env         — environment (shared with parent)
├── globals     — global vector (shared — same heap)
├── parent      — parent context
├── child       — child context
├── dk          — dynamic-wind point (thread-local)
├── params      — parameter alist (thread-local storage)
├── event       — what this thread is blocked on (mutex/condvar/port/thread)
├── waitp       — 1 if blocked, 0 if runnable
├── refuel      — instructions per timeslice (default 500)
├── tval        — timeout (struct timeval) for timed waits
├── result      — final result when thread terminates
├── errorp      — 1 if thread ended with exception
├── timeoutp    — 1 if a timed wait expired
├── name        — thread name (for debugging)
├── specific    — thread-specific storage (SRFI-18)
└── saves       — GC save chain
```

The critical insight: `globals` is **shared** across all contexts in the same heap. This means all green threads see the same global variables, the same type table, the same scheduler. But each thread has its own `stack`, `ip`, `dk`, and `params`.

## 3. Thread Creation: `make-thread`

`sexp_make_thread` in `threads.c:85-112`:

```c
sexp sexp_make_thread (sexp ctx, sexp self, sexp_sint_t n,
                       sexp thunk, sexp name) {
  sexp *stack;
  sexp_gc_var1(res);
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, thunk);
  sexp_gc_preserve1(ctx, res);

  // Create a new context as child of current
  res = sexp_make_eval_context(ctx, SEXP_FALSE, sexp_context_env(ctx), 0, 0);

  sexp_context_name(res) = name;
  sexp_context_proc(res) = thunk;
  sexp_context_ip(res) = sexp_bytecode_data(sexp_procedure_code(thunk));

  // Set up initial stack frame
  stack = sexp_stack_data(sexp_context_stack(res));
  stack[0] = stack[1] = stack[3] = SEXP_ZERO;
  stack[2] = sexp_global(ctx, SEXP_G_FINAL_RESUMER);  // return address
  sexp_context_top(res) = 4;
  sexp_context_last_fp(res) = 0;

  // Share parent's dynamic-wind point
  sexp_context_dk(res) = sexp_context_dk(ctx);

  // Fresh parameter list (thread-local storage starts empty)
  sexp_context_params(res) = SEXP_NULL;

  sexp_gc_release1(ctx);
  return res;
}
```

Key points:
- The new context shares the parent's **heap** and **globals vector** (same GC arena).
- It gets its own **stack** (allocated by `sexp_make_context`), initially 8192 slots.
- The **ip** points to the first instruction of the thunk's bytecode.
- The stack is set up with a minimal frame: slot 2 holds `FINAL_RESUMER`, which is the procedure that catches the thread's return value and terminates it.
- `dk` is copied from the parent so `travel-to-point!` can find a common ancestor.
- `params` starts empty — thread-local parameter bindings are created on demand.

## 4. Thread Start: Enqueueing

`sexp_thread_start` in `threads.c:114-126` adds a thread to the **run queue**:

```c
sexp sexp_thread_start (sexp ctx, sexp self, sexp_sint_t n, sexp thread) {
  sexp cell;
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  sexp_context_errorp(thread) = 0;
  cell = sexp_cons(ctx, thread, SEXP_NULL);

  if (sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_BACK))) {
    // Append to back of queue
    sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK)) = cell;
    sexp_global(ctx, SEXP_G_THREADS_BACK) = cell;
  } else {
    // Empty queue — this cell is both front and back
    sexp_global(ctx, SEXP_G_THREADS_BACK) = cell;
    sexp_global(ctx, SEXP_G_THREADS_FRONT) = cell;
  }
  return thread;
}
```

The run queue is a **singly-linked list** stored in two globals:
- `SEXP_G_THREADS_FRONT` — head (dequeue from here)
- `SEXP_G_THREADS_BACK` — tail (enqueue here)

There's also a **paused list** for blocked threads:
- `SEXP_G_THREADS_PAUSED` — sorted by timeout (earliest first)

## 5. The VM Loop and Fuel-Based Preemption

The heart of green thread scheduling lives in `vm.c:1103-1152`, at the top of the bytecode dispatch loop:

```c
sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  sexp root_thread = ctx;                    // remember who called sexp_apply
  sexp_sint_t fuel = sexp_context_refuel(ctx); // 500 instructions

  // ... setup ...

 loop:
  if (--fuel <= 0) {
    // Check for interrupt
    if (sexp_context_interruptp(ctx)) {
      fuel = sexp_context_refuel(ctx);
      sexp_context_interruptp(ctx) = 0;
      _ARG1 = sexp_global(ctx, SEXP_G_INTERRUPT_ERROR);
      goto call_error_handler;
    }

    tmp1 = sexp_global(ctx, SEXP_G_THREADS_SCHEDULER);
    if (sexp_applicablep(tmp1) && sexp_not(sexp_global(ctx, SEXP_G_ATOMIC_P))) {
      // ---- CONTEXT SWITCH ----

      // 1. Save current thread's registers
      sexp_context_top(ctx) = top;
      sexp_context_ip(ctx) = ip;
      sexp_context_last_fp(ctx) = fp;
      sexp_context_proc(ctx) = self;

      // 2. Run scheduler — it returns the next context to execute
      ctx = sexp_apply1(ctx, tmp1, root_thread);

      // 3. Restore new thread's registers
      stack = sexp_stack_data(sexp_context_stack(ctx));
      top = sexp_context_top(ctx);
      fp = sexp_context_last_fp(ctx);
      ip = sexp_context_ip(ctx);
      self = sexp_context_proc(ctx);
      bc = sexp_procedure_code(self);
      cp = sexp_procedure_vars(self);
    }

    fuel = sexp_context_refuel(ctx);
    if (fuel <= 0) goto end_loop;        // thread terminated
    if (sexp_context_waitp(ctx)) {
      fuel = 1;
      goto loop;  // still blocked, try scheduler again immediately
    }
  }

  // ... dispatch bytecode instruction ...
  goto loop;
```

**How it works:**

1. Every bytecode instruction decrements `fuel`.
2. When fuel reaches 0 (every 500 instructions), the scheduler runs.
3. The scheduler returns which context to execute next (may be the same one).
4. The VM saves registers to the old context, loads registers from the new context.
5. Execution continues at the new context's `ip` — the thread resumes where it left off.

**`SEXP_DEFAULT_QUANTUM`** (features.h:341) defines the timeslice:

```c
#define SEXP_DEFAULT_QUANTUM 500
```

So each thread gets **500 bytecode instructions** before the scheduler is invoked.

**Atomic sections**: When `SEXP_G_ATOMIC_P` is true, the scheduler is skipped even when fuel runs out. This allows critical sections that must not be interrupted.

## 6. The Scheduler

`sexp_scheduler` in `threads.c:422-636` is a C function that decides which thread runs next. It's called as a Scheme foreign function. Here's what it does on each invocation:

### Step 1: Check Unix Signals

```c
if (sexp_global(ctx, SEXP_G_THREADS_SIGNALS) != SEXP_ZERO) {
    // Wake up (or create) the signal-runner thread
}
```

### Step 2: Check Blocked I/O (poll)

```c
pollfds = sexp_global(ctx, SEXP_G_THREADS_POLL_FDS);
if (sexp_pollfdsp(ctx, pollfds) && sexp_pollfds_num_fds(pollfds) > 0) {
    k = poll(sexp_pollfds_fds(pollfds), sexp_pollfds_num_fds(pollfds), 0);
    // For each ready fd, unblock all threads waiting on that fd:
    //   move from PAUSED list → FRONT of run queue
    //   clear waitp and event
}
```

Uses non-blocking `poll(timeout=0)` to check which file descriptors are ready without blocking the whole VM.

### Step 3: Check Terminated Threads

```c
if (sexp_context_refuel(ctx) <= 0) {
    // Current thread is dead — wake up any threads doing thread-join! on it
    // Move them from PAUSED → run queue
}
```

A thread with `refuel <= 0` is terminated. The scheduler unblocks any thread that was waiting to join it.

### Step 4: Check Timeouts

```c
gettimeofday(&tval, NULL);
while (sexp_pairp(ls2) && sexp_context_before(sexp_car(ls2), tval)) {
    sexp_context_timeoutp(sexp_car(ls2)) = 1;
    sexp_context_waitp(sexp_car(ls2)) = 0;
    // Move to run queue
}
```

The paused list is sorted by timeout. Walk from the front, moving all expired threads back to the run queue.

### Step 5: Pick Next Thread (Round-Robin)

```c
if (sexp_pairp(front)) {
    res = sexp_car(front);  // next thread

    if (sexp_context_refuel(ctx) <= 0 || sexp_context_waitp(ctx)) {
        // Current is dead or blocked — just dequeue next
        sexp_global(ctx, SEXP_G_THREADS_FRONT) = sexp_cdr(front);
    } else {
        // Current is still runnable — swap: put current at back, take front
        sexp_car(sexp_global(ctx, SEXP_G_THREADS_FRONT)) = ctx;
        // Rotate: front → back
    }
} else {
    res = ctx;  // no other threads, keep running current
}
```

This is **round-robin scheduling**: the current thread goes to the back of the queue, the thread at the front gets to run next.

### Step 6: Handle All-Blocked Case

```c
if (sexp_context_waitp(res)) {
    // Only blocked threads available — take a nap to avoid busy-looping
    usleep(usecs);  // up to 10ms
}
```

If every thread is blocked (waiting on I/O, mutex, sleep, or join), the scheduler calls `usleep` to avoid spinning the CPU. It sleeps for the minimum of 10ms or until the next timeout.

## 7. Thread States

A thread's state is determined by several fields:

```
                              ┌──────────┐
                              │   NEW    │  (created, not started)
                              │ refuel>0 │
                              │ not in   │
                              │ any queue│
                              └────┬─────┘
                                   │ thread-start!
                                   ▼
                    ┌──────────────────────────────┐
                    │          RUNNABLE             │
                    │  refuel > 0, waitp = 0       │
                    │  in THREADS_FRONT/BACK queue  │
                    └──────┬──────────────┬────────┘
                           │              │
              mutex-lock!  │              │  fuel→0 (preempted)
              thread-join! │              │  or I/O would block
              thread-sleep!│              │
              condvar-wait │              │
              I/O EAGAIN   │              │
                           ▼              ▼
                    ┌──────────────┐  (back to RUNNABLE
                    │   BLOCKED    │   via scheduler)
                    │  waitp = 1   │
                    │  event = X   │──── what we're waiting on:
                    │  in PAUSED   │     mutex, condvar, port,
                    │  list        │     thread (join), or
                    └──────┬───────┘     SEXP_FALSE (sleep)
                           │
              event ready  │
              or timeout   │
                           ▼
                    ┌──────────────┐
                    │  TERMINATED  │
                    │  refuel = 0  │
                    │  result set  │
                    │  errorp if   │
                    │  exception   │
                    └──────────────┘
```

State checks:
- **New**: context exists but not in any queue
- **Runnable**: `refuel > 0`, `waitp == 0`, in the front/back queue
- **Blocked**: `waitp == 1`, in `SEXP_G_THREADS_PAUSED` list, `event` says why
- **Terminated**: `refuel == 0`, `result` holds return value

## 8. Stack Management

Each context gets its own stack, allocated in `sexp_make_context` (`sexp.c`):

```c
// features.h:872-880
#define SEXP_INIT_STACK_SIZE 8192          // initial slots (debug: 1024)
#define SEXP_MAX_STACK_SIZE SEXP_INIT_STACK_SIZE*1000  // 8 million slots
```

The stack is a GC-managed `SEXP_STACK` object:

```
sexp_stack:
├── length   — allocated size (number of sexp slots)
├── top      — current stack pointer (index)
└── data[]   — flexible array of sexp values
```

The VM uses `sexp_ensure_stack` to grow the stack if needed:

```c
// vm.c:966-977
#define sexp_ensure_stack(n)                                          \
  if (top + (n) >= sexp_stack_length(sexp_context_stack(ctx))) {      \
    if (top + (n) >= SEXP_MAX_STACK_SIZE)                             \
      { _ARG1 = sexp_global(ctx, SEXP_G_OOS_ERROR);                  \
        fuel = 0; ip--; goto loop; }                                  \
    sexp_grow_stack(ctx, n);                                          \
    stack = sexp_stack_data(sexp_context_stack(ctx));                  \
    fuel = 0; ip--; goto loop;                                        \
  }
```

Stack overflow forces a scheduler invocation (`fuel = 0`) after growing, because `sexp_grow_stack` may trigger GC which can move objects.

## 9. Dynamic Wind and `%dk`

Dynamic wind creates a chain of "points" that tracks which `dynamic-wind` scopes are active. Each point is a vector:

```scheme
;; init-7.scm:776-786
(define %make-point vector)
(define (%point-depth point) (vector-ref point 0))   ; nesting depth
(define (%point-in point)    (vector-ref point 1))    ; entry thunk
(define (%point-out point)   (vector-ref point 2))    ; exit thunk
(define (%point-parent point)(vector-ref point 3))    ; enclosing point

(define root-point
  (%make-point 0
    (lambda () (error "winding in to root!"))
    (lambda () (error "winding out of root!"))
    #f))   ; no parent
```

When `dynamic-wind` is entered:

```scheme
(define (dynamic-wind in body out)
  (in)
  (let ((here (%dk)))
    (%dk (%make-point (+ (%point-depth here) 1) in out here))
    (let ((res (body)))
      (%dk here)
      (out)
      res)))
```

### Thread-Local `%dk`

Without green threads, `%dk` is a closure over a mutable variable. With green threads, `%dk` is a C function that reads/writes `sexp_context_dk(ctx)` — the current context's dk field:

```c
// eval.c:2472-2479
sexp sexp_dk (sexp ctx, sexp self, sexp_sint_t n, sexp val) {
  if (sexp_not(val)) {
    return sexp_context_dk(ctx) ? sexp_context_dk(ctx) : SEXP_FALSE;
  } else {
    sexp_context_dk(ctx) = val;
    return SEXP_VOID;
  }
}
```

This makes `%dk` **thread-local**: each green thread has its own dynamic-wind position. When thread A enters a `dynamic-wind`, only thread A's `dk` changes. Thread B's `dk` is unaffected.

### `call-with-current-continuation` and `travel-to-point!`

When a continuation is invoked, the VM must wind in/out of dynamic-wind scopes to get from the current point to the captured point:

```scheme
;; init-7.scm:809-829
(define (travel-to-point! here target)
  (cond
   ((eq? here target) 'done)                              ; same point
   ((< (%point-depth here) (%point-depth target))
    (travel-to-point! here (%point-parent target))        ; wind in
    ((%point-in target)))
   (else
    ((%point-out here))                                   ; wind out
    (travel-to-point! (%point-parent here) target))))

(define (continuation->procedure cont point)
  (lambda res
    (travel-to-point! (%dk) point)   ; wind from current dk to captured dk
    (%dk point)                       ; set dk to captured point
    (cont (%values res))))            ; invoke raw continuation

(define (call-with-current-continuation proc)
  (%call/cc
   (lambda (cont)
     (proc (continuation->procedure cont (%dk))))))
```

**Why thread creation copies the parent's dk**: `sexp_context_dk(res) = sexp_context_dk(ctx)`. Without this, `travel-to-point!` would fail to find a common ancestor between threads (it compares points with `eq?`). By starting from the same root, continuations captured in child threads can safely travel back.

## 10. Parameters (Thread-Local Storage)

Scheme parameters (`make-parameter`) provide thread-local values. Each context has a `params` field — an **alist** (list of `(parameter . value)` pairs):

```c
// vm.c:1450-1460 — SEXP_OP_PARAMETER_REF opcode
case SEXP_OP_PARAMETER_REF:
  _ALIGN_IP();
  sexp_context_top(ctx) = top;
  tmp2 = _WORD0;           // the parameter opcode object
  ip += sizeof(sexp);

  // Search thread-local bindings first
  for (tmp1=sexp_context_params(ctx); sexp_pairp(tmp1); tmp1=sexp_cdr(tmp1))
    if (sexp_caar(tmp1) == tmp2) {
      _PUSH(sexp_car(tmp1));    // found thread-local binding
      goto loop;
    }

  // Fall back to global default
  _PUSH(sexp_opcode_data(tmp2));
```

When a parameter is read:
1. Walk the current context's `params` alist looking for a matching parameter.
2. If found, use the thread-local value.
3. If not found, use the parameter's global default (stored in `sexp_opcode_data`).

`parameterize` pushes new entries onto `params`. Since each green thread has its own `params`, parameterized values are automatically thread-local.

This is how `current-input-port`, `current-output-port`, and `current-error-port` work — they're parameters, so each green thread can have its own I/O ports.

## 11. I/O and Blocking

Green threads require that I/O never blocks the OS thread. Chibi handles this by:

### Non-blocking file descriptors

When opening files with green threads enabled (`eval.c:1310-1312`):

```c
#if SEXP_USE_GREEN_THREADS
  fcntl(fileno(in), F_SETFL, O_NONBLOCK);
#endif
```

All file descriptors are set to non-blocking mode.

### EAGAIN handling in the VM

When a read or write returns `EAGAIN` (would block), the VM doesn't block. Instead, it calls the **blocker** to pause the current thread on that fd:

```c
// vm.c:2194-2204 — inside SEXP_OP_READ_CHAR
} else if (errno == EAGAIN) {
    clearerr(sexp_port_stream(_ARG1));
    // Register this thread as blocked on this port
    sexp_apply2(ctx, sexp_global(ctx, SEXP_G_THREADS_BLOCKER), _ARG1, SEXP_FALSE);
    fuel = 0;     // force scheduler to run
    ip--;         // retry the read-char instruction later
}
```

### The Blocker

`sexp_blocker` in `threads.c:402-419`:

```c
sexp sexp_blocker (sexp ctx, sexp self, sexp_sint_t n,
                   sexp portorfd, sexp timeout) {
  int fd = /* extract fd from port/fileno/fixnum */;
  if (fd >= 0)
    sexp_insert_pollfd(ctx, fd, sexp_oportp(portorfd) ? POLLOUT : POLLIN);

  sexp_context_waitp(ctx) = 1;         // mark as blocked
  sexp_context_event(ctx) = portorfd;   // record what we're waiting on
  sexp_insert_timed(ctx, ctx, timeout); // add to paused list
  return SEXP_VOID;
}
```

This registers the fd in a **pollfds** array and moves the thread to the paused list. Next time the scheduler runs, it calls `poll()` on all registered fds. When the fd becomes ready, the thread is moved back to the run queue and the read/write instruction is retried.

### The flow:

```
Thread A: (read-char port)
  → I/O would block (EAGAIN)
    → blocker adds fd to pollfds, marks thread blocked
      → fuel=0, scheduler runs
        → Thread B gets to run
          → ... eventually scheduler polls fds ...
            → fd ready! Thread A unblocked
              → Thread A re-executes read-char
                → data available, succeeds
```

## 12. Mutex and Condition Variable

### Mutex Lock (`threads.c:250-262`)

```c
sexp sexp_mutex_lock (sexp ctx, sexp self, sexp_sint_t n,
                      sexp mutex, sexp timeout, sexp thread) {
  if (sexp_not(sexp_mutex_lockp(mutex))) {
    // Unlocked — acquire immediately
    sexp_mutex_lockp(mutex) = SEXP_TRUE;
    sexp_mutex_thread(mutex) = thread;
    return SEXP_TRUE;
  } else {
    // Already locked — block current thread
    sexp_context_waitp(ctx) = 1;
    sexp_context_event(ctx) = mutex;     // blocked on this mutex
    sexp_insert_timed(ctx, ctx, timeout);
    return SEXP_FALSE;
  }
}
```

### Mutex Unlock (`threads.c:265-296`)

```c
sexp sexp_mutex_unlock (sexp ctx, ..., sexp mutex, sexp condvar, sexp timeout) {
  if (sexp_truep(sexp_mutex_lockp(mutex))) {
    sexp_mutex_lockp(mutex) = SEXP_FALSE;

    // Search paused list for threads blocked on this mutex
    for (ls2 = sexp_global(ctx, SEXP_G_THREADS_PAUSED); ...) {
      if (sexp_context_event(sexp_car(ls2)) == mutex) {
        // Move from PAUSED → FRONT of run queue
        sexp_context_waitp(sexp_car(ls2)) = 0;
        break;
      }
    }
  }

  if (sexp_truep(condvar)) {
    // Optional: wait on condition variable after unlock
    sexp_context_waitp(ctx) = 1;
    sexp_context_event(ctx) = condvar;
    sexp_insert_timed(ctx, ctx, timeout);
  }
  return SEXP_TRUE;
}
```

### Condition Variable Signal (`threads.c:300-316`)

```c
sexp sexp_condition_variable_signal (sexp ctx, ..., sexp condvar) {
  // Find first thread blocked on this condvar in paused list
  for (ls2 = sexp_global(ctx, SEXP_G_THREADS_PAUSED); ...) {
    if (sexp_context_event(sexp_car(ls2)) == condvar) {
      // Move from PAUSED → FRONT of run queue
      sexp_context_waitp(sexp_car(ls2)) = 0;
      sexp_context_timeoutp(sexp_car(ls2)) = 0;
      return SEXP_TRUE;   // one thread woken
    }
  }
  return SEXP_FALSE;  // no thread was waiting
}
```

All synchronization is cooperative — there are no OS-level locks. A mutex is just a Scheme record with a boolean flag. Blocking means "put this context on the paused list." The scheduler later moves it back when the mutex is unlocked.

## 13. Thread Termination

When a thread's thunk returns, execution falls through to `end_loop` in `vm.c:2303-2327`:

```c
end_loop:
  sexp_context_result(ctx) = _ARG1;   // save return value

  if (ctx != root_thread) {
    if (sexp_context_refuel(root_thread) <= 0) {
      // Root already terminated — return root's result
      _ARG1 = sexp_context_result(root_thread);
    } else {
      // Child thread finishing
      if (sexp_exceptionp(_ARG1)) {
        // Print uncaught exception to stderr
        sexp_print_exception(ctx, _ARG1, tmp1);
      }
      sexp_context_refuel(ctx) = fuel = 0;  // mark as terminated
      goto loop;  // back to scheduler
    }
  }
```

Setting `refuel = 0` marks the thread as terminated. The scheduler will:
1. Never schedule it again (checks `refuel > 0`).
2. Wake up any threads doing `thread-join!` on it.
3. The joining thread can read `sexp_context_result` and `sexp_context_errorp`.

## 14. Summary: Cooperative, Not Preemptive

Despite having a fuel counter that forces periodic scheduling, chibi's green threads are fundamentally **cooperative with automatic yield points**. The fuel mechanism ensures that a long-running computation doesn't starve other threads, but:

- Context switches only happen at **bytecode instruction boundaries** (between any two instructions).
- A C function call (bridge function) runs to completion — no preemption mid-call.
- The `SEXP_G_ATOMIC_P` flag can suppress scheduling entirely.
- Mutex/condvar operations are non-blocking at the OS level — they just manipulate linked lists.
- All I/O is non-blocking; blocking I/O pauses only the green thread, not the OS thread.

This is fundamentally different from OS threads or the Eval thread pool:

| | Green Threads | Thread Pool |
|---|---|---|
| **Parallelism** | None (1 OS thread) | True (N OS threads) |
| **Scheduling** | Cooperative + fuel | OS preemptive |
| **Shared state** | Same heap, same globals | Separate heaps |
| **Communication** | Direct variable access | Channels (serialized) |
| **Synchronization** | Scheme mutexes (cooperative) | OS mutexes (blocking) |
| **I/O** | Non-blocking + poll (`TcpSocket`, `TcpServer`) | Regular blocking |
| **Stack** | Own stack per context | Own heap per worker |
| **GC** | Single GC for all threads | Independent GC per worker |
| **Cost** | ~200 bytes per thread | ~MB per worker |
