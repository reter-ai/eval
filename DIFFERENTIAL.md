# Differentiable Programming in Eval

Eval has a built-in automatic differentiation engine with two modes:

- **Forward-mode** (dual numbers) for scalar functions, composable for higher-order derivatives
- **Reverse-mode** (tape-based) for scalars and tensors, with optional TensorFlow graph-mode acceleration

When TensorFlow's C API is available, the entire backward pass is translated into a TF computation graph, enabling op fusion, memory planning, and GPU acceleration — all transparent to user code.

## Quick Start

### Scalar Reverse-Mode

```
param x = 3.0;
define y = x * x + 2.0 * x;
backward(y);
x->grad          // => 8.0  (dy/dx = 2x + 2 at x=3)
```

`param` declares a differentiable variable. `backward` runs reverse-mode AD from the output back to all parameters. Gradients accumulate in `->grad`.

### Scalar Forward-Mode

```
define f = function(x) x * x + 2.0 * x;
define df = grad(f);
df(3.0)           // => 8.0
```

`grad(f)` returns a new function computing df/dx via dual numbers. Composable: `grad(grad(f))` gives the second derivative.

### Tensor Operations

```
param W = [[0.1, 0.2], [0.3, 0.4]];
param x = [[1.0], [2.0]];
define y = W @ x;                   // matrix multiply
define loss = sum(y);
backward(loss);
W->grad           // => [[1.0, 2.0], [1.0, 2.0]]
x->grad           // => [[0.3], [0.6]]
```

## Operations

### Arithmetic (scalar and tensor, with broadcasting)
| Syntax | Operation |
|--------|-----------|
| `a + b` | Addition |
| `a - b` | Subtraction |
| `a * b` | Element-wise multiplication |
| `a / b` | Element-wise division |
| `a ** b` | Power |

### Unary Math
| Function | Description |
|----------|-------------|
| `sin(x)` | Sine |
| `cos(x)` | Cosine |
| `tan(x)` | Tangent |
| `exp(x)` | Exponential |
| `log(x)` | Natural logarithm |
| `sqrt(x)` | Square root |
| `tanh(x)` | Hyperbolic tangent |
| `sigmoid(x)` | Logistic sigmoid |
| `relu(x)` | Rectified linear unit |
| `abs(x)` | Absolute value |

### Tensor-Specific
| Function | Description |
|----------|-------------|
| `a @ b` | Matrix multiplication |
| `transpose(x)` | Matrix transpose (2D) |
| `sum(x)` | Sum all elements to scalar |
| `mean(x)` | Mean of all elements |

### Tape Control
| Function | Description |
|----------|-------------|
| `param x = val` | Create a differentiable parameter |
| `backward(y)` | Run reverse-mode from `y` |
| `x->grad` | Read accumulated gradient |
| `tape_reset()` | Clear the tape for a new computation |
| `zero_grad()` | Zero all gradients without clearing tape |
| `no_grad(function() ...)` | Run code without recording to tape |

### Optimizer
```
define opt = SGD([x, y, z], 0.01);
opt->step();         // update params: p -= lr * p.grad
opt->zero_grad();    // reset gradients
```

## TensorFlow Graph-Mode Backend

When built with TensorFlow (`EVAL_HAVE_TF=1`), the backward pass operates in graph mode:

```
Forward pass:
  Native C ops compute values immediately (always)

Backward pass (with TF):
  tape entries  -->  TF_Graph nodes  -->  TF_AddGradients()  -->  TF_SessionRun()
  Single execution replaces the manual C reverse sweep.
  TF handles gradient math, op fusion, and GPU placement.

Backward pass (without TF):
  Native C reverse sweep (unchanged behavior)
```

Each tape entry maps to a TensorFlow graph node:

| AD Op | TF Node | Notes |
|-------|---------|-------|
| Leaf (param) | `Placeholder` | Fed via SessionRun |
| Leaf (const) | `Const` | Value baked into graph |
| `+` | `AddV2` | |
| `-` | `Sub` | |
| `*` | `Mul` | Element-wise |
| `/` | `RealDiv` | |
| `**` | `Pow` | |
| `-x` | `Neg` | |
| `sin` | `Sin` | |
| `cos` | `Cos` | |
| `exp` | `Exp` | |
| `log` | `Log` | |
| `sqrt` | `Sqrt` | |
| `tanh` | `Tanh` | |
| `sigmoid` | `Sigmoid` | |
| `relu` | `Relu` | |
| `abs` | `Abs` | |
| `@` | `MatMul` | |
| `transpose` | `Transpose` | perm=[1,0] |
| `sum` | `Sum` | Reduce all axes |
| `mean` | `Mean` | Reduce all axes |

Forward tensor ops (matmul, element-wise, transpose) also use TensorFlow Eager execution for GPU acceleration on tensors with 64+ elements. Smaller tensors use native C to avoid dispatch overhead.

If graph construction or gradient computation fails for any reason, the system falls back to the native C backward pass automatically.

### Building with TensorFlow

TensorFlow is detected automatically by CMake:

```
cmake .. -DCMAKE_PREFIX_PATH=/path/to/libtensorflow
```

Or place the TF C library headers/libs where CMake can find them (e.g., in `vcpkg_installed/x64-windows/`). The build prints:

```
-- Found TensorFlow - GPU-accelerated tensor ops enabled
```

Without TensorFlow, everything works identically using native C:

```
-- TensorFlow not found - using native tensor ops
```

## Examples

### Linear Regression

```
// Data: y = 2x + 1
define xs = [1.0, 2.0, 3.0, 4.0, 5.0];
define ys = [3.0, 5.0, 7.0, 9.0, 11.0];

param w = 0.0;
param b = 0.0;

for(let i = 0, i < 100, i++) {
    zero_grad();

    // Forward: MSE loss
    define loss_sum = 0.0;
    for(let j = 0, j < 5, j++) {
        define pred = w * xs[j] + b;
        define diff = pred - ys[j];
        loss_sum = loss_sum + diff * diff;
    };

    backward(loss_sum);

    // Gradient descent
    define lr = 0.001;
    w = w->value - lr * w->grad;
    b = b->value - lr * b->grad;

    tape_reset();
    param w = w;
    param b = b;
};

display(w->value);   // => ~2.0
display(b->value);   // => ~1.0
```

### Multi-Layer Perceptron

A 2-layer neural network with ReLU activation:

```
// Network: input[1,2] -> W1[2,4] -> ReLU -> W2[4,1] -> output
param W1 = [[0.1, 0.2, 0.3, 0.4],
            [0.5, 0.6, 0.7, 0.8]];
param W2 = [[0.1], [0.2], [0.3], [0.4]];
param bias1 = [[0.0, 0.0, 0.0, 0.0]];
param bias2 = [[0.0]];

define input = tensor([[1.0, 2.0]]);
define target = tensor([[1.0]]);

// Forward
define h = relu(input @ W1 + bias1);
define out = h @ W2 + bias2;
define loss = mean((out - target) * (out - target));

// Backward
backward(loss);

display("Loss: "); display(loss->data); newline();
display("W1 grad: "); display(W1->grad); newline();
display("W2 grad: "); display(W2->grad); newline()
```

### Softmax and Cross-Entropy

Softmax is composed from `exp`, `sum`, and division with broadcasting:

```
define softmax = function(logits) {
    define ex = exp(logits);
    define s = sum(ex);
    ex / s
};

param logits = [[2.0, 1.0, 0.1]];
define probs = softmax(logits);
display(probs->data); newline();
// => ((0.6590 0.2424 0.0986))

// Cross-entropy loss against target class 0
define target = tensor([[1.0, 0.0, 0.0]]);
define picked = probs * target;
define loss = sum(picked);
backward(loss);
display(logits->grad); newline();
// Gradient shows push toward target class
```

### Transformer Self-Attention

A single-head self-attention layer. Softmax, matmul, transpose, and scaling
are all differentiable:

```
// Self-attention: Attention(Q,K,V) = softmax(Q @ K^T / sqrt(d_k)) @ V
//
// input: [seq_len, d_model] = [3, 4]
// W_q, W_k, W_v: [d_model, d_k] = [4, 2]

define softmax = function(logits) {
    define ex = exp(logits);
    define s = sum(ex);
    ex / s
};

param W_q = [[0.1, 0.2], [0.3, 0.4], [0.5, 0.6], [0.7, 0.8]];
param W_k = [[0.2, 0.1], [0.4, 0.3], [0.6, 0.5], [0.8, 0.7]];
param W_v = [[0.1, 0.3], [0.2, 0.4], [0.3, 0.5], [0.4, 0.6]];

define input = tensor([
    [1.0, 0.0, 1.0, 0.0],
    [0.0, 1.0, 0.0, 1.0],
    [1.0, 1.0, 0.0, 0.0]
]);

define d_k = 2.0;

// Project to Q, K, V
define Q = input @ W_q;           // [3, 2]
define K = input @ W_k;           // [3, 2]
define V = input @ W_v;           // [3, 2]

// Attention scores: Q @ K^T / sqrt(d_k)
define scores = (Q @ transpose(K)) / sqrt(d_k);   // [3, 3]

// Softmax over each row (note: this applies softmax globally,
// a row-wise softmax would need per-row ops — see Limitations)
define attn = softmax(scores);     // [3, 3]

// Weighted values
define out = attn @ V;             // [3, 2]

// Loss: sum of output
define loss = sum(out);
backward(loss);

display("Attention output: "); display(out->data); newline();
display("W_q grad: "); display(W_q->grad); newline();
display("W_k grad: "); display(W_k->grad); newline();
display("W_v grad: "); display(W_v->grad); newline()
```

All weight gradients (`W_q->grad`, `W_k->grad`, `W_v->grad`) are computed in a single `backward(loss)` call. With TensorFlow, this entire gradient computation executes as one optimized TF graph.

### YOLO-Style Detection Head

A simplified YOLO detection head computing bounding box regression loss
and objectness loss. Uses sigmoid for objectness, MSE for box coordinates:

```
// YOLO detection head
// Grid output: [grid_h * grid_w, 5] = [4, 5]
//   Each row: [tx, ty, tw, th, objectness]
// For simplicity: 2x2 grid, 1 anchor

param W_detect = [
    [0.1, 0.2, 0.3, 0.4, 0.5],
    [0.2, 0.3, 0.4, 0.5, 0.6],
    [0.3, 0.4, 0.5, 0.6, 0.7],
    [0.4, 0.5, 0.6, 0.7, 0.8]
];

// Feature map input [4, 8] (flattened grid cells, feature dim)
define features = tensor([
    [1.0, 0.5, 0.3, 0.2, 0.8, 0.1, 0.4, 0.6],
    [0.2, 0.8, 0.1, 0.7, 0.3, 0.9, 0.5, 0.2],
    [0.6, 0.3, 0.7, 0.1, 0.5, 0.4, 0.2, 0.8],
    [0.4, 0.6, 0.2, 0.9, 0.1, 0.7, 0.8, 0.3]
]);

// Linear projection to detection outputs
param W_proj = [
    [0.1, 0.2, 0.1, 0.2, 0.3],
    [0.2, 0.1, 0.3, 0.1, 0.2],
    [0.3, 0.3, 0.2, 0.3, 0.1],
    [0.1, 0.2, 0.3, 0.2, 0.2],
    [0.2, 0.1, 0.1, 0.3, 0.3],
    [0.3, 0.3, 0.2, 0.1, 0.1],
    [0.1, 0.2, 0.3, 0.1, 0.2],
    [0.2, 0.1, 0.1, 0.2, 0.3]
];

define raw_preds = features @ W_proj;   // [4, 5]

// Ground truth for one object in cell 0
// [cx, cy, w, h, obj] — obj=1 for cell with object, 0 otherwise
define gt_boxes = tensor([
    [0.5, 0.5, 0.3, 0.4, 1.0],
    [0.0, 0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0, 0.0]
]);

// Apply sigmoid to objectness (column 4) and box coords
define preds = sigmoid(raw_preds);

// MSE loss over all predictions vs ground truth
define diff = preds - gt_boxes;
define loss = mean(diff * diff);

backward(loss);

display("Detection loss: "); display(loss->data); newline();
display("W_proj grad shape: "); display(W_proj->shape); newline();
display("W_proj grad: "); display(W_proj->grad); newline()
```

### Recurrent Computation (Unrolled RNN)

Manual unrolling of a simple RNN cell:

```
// Simple RNN: h_t = tanh(W_h @ h_{t-1} + W_x @ x_t)
// Sequence length = 3, hidden_dim = 2, input_dim = 2

param W_h = [[0.1, 0.2], [0.3, 0.4]];
param W_x = [[0.5, 0.6], [0.7, 0.8]];

// Input sequence: 3 time steps
define x0 = tensor([[1.0, 0.0]]);
define x1 = tensor([[0.0, 1.0]]);
define x2 = tensor([[1.0, 1.0]]);

// Initial hidden state
define h = tensor([[0.0, 0.0]]);

// Unroll 3 steps
define h = tanh(h @ W_h + x0 @ W_x);
define h = tanh(h @ W_h + x1 @ W_x);
define h = tanh(h @ W_h + x2 @ W_x);

// Loss: sum of final hidden state
define loss = sum(h);
backward(loss);

display("Final hidden: "); display(h->data); newline();
display("W_h grad: "); display(W_h->grad); newline();
display("W_x grad: "); display(W_x->grad); newline()
```

Gradients flow through all 3 time steps via backpropagation through time (BPTT), computed in one `backward` call.

## Architecture

```
User code (Eval language)
       |
       v
  Eval Parser  -->  Scheme AST  -->  Chibi-Scheme VM
       |                                    |
       v                                    v
  param/backward keywords         C bridge functions
       |                           (__ad_make_var__, etc.)
       v                                    |
  AD Tape (thread-local)  <-----------------+
       |
       +--- Forward: native C ops (or TFE eager for tensors)
       |
       +--- Backward: dispatch table
                |
                +-- Native C:  manual reverse sweep
                |
                +-- TensorFlow: tape -> TF_Graph -> TF_AddGradients
                                    -> TF_SessionRun -> read gradients
```

### Dispatch Table

All tensor operations go through a swappable dispatch table:

```c
typedef struct {
    void (*binary_ew)(...);   // element-wise add/sub/mul/div/pow
    void (*matmul)(...);      // matrix multiplication
    void (*transpose)(...);   // 2D transpose
    void (*unary_ew)(...);    // sin/cos/exp/relu/...
    void (*backward)(...);    // full reverse sweep
} ADTensorOps;
```

At startup, all slots point to native C implementations. When TensorFlow is available, `eval_tf_init()` replaces them with TF-backed versions. Forward ops use TFE (eager) for GPU. The backward slot uses TF graph mode for whole-graph optimization.

### Thread Safety

The AD tape is thread-local (`__declspec(thread)` on Windows, `__thread` on POSIX). Each thread pool worker has its own tape. TFE contexts are also thread-local and lazily initialized.

## Limitations

Current limitations relative to full ML frameworks:

- **No row-wise softmax**: `softmax` applies globally across all elements, not per-row. A true transformer attention layer needs per-row softmax over the sequence dimension. This can be worked around by manually looping over rows, but is not yet a single differentiable op.
- **No reshape/view**: Tensors cannot be reshaped without creating new allocations. Operations like "split attention heads" from `[batch, seq, d_model]` to `[batch, heads, seq, d_k]` are not supported.
- **No slicing/indexing**: Cannot extract or assign to sub-tensors (e.g., `x[0:3, :]`).
- **No concatenation**: Cannot join tensors along an axis.
- **2D tensors only for matmul/transpose**: Batched matmul and higher-dimensional transpose are not supported.
- **No convolutions**: Conv2D, pooling, and other spatial ops are not available.
- **CPU-only forward by default**: TFE eager ops provide GPU forward, but data round-trips through CPU memory (no persistent GPU tensors).
- **Single-precision not supported**: All computation is float64 (double). No float32/float16 mode.

These are sufficient for fully-connected networks, attention mechanisms (with global softmax), and educational/research use. Production transformer or CNN training requires additional ops.

## Files

| File | Role |
|------|------|
| `src/_eval_ad.h` | Type declarations, dispatch table, op enum |
| `src/_eval_ad.c` | Tape, native C ops, type registration, bridge functions |
| `src/_eval_tf.c` | TF graph-mode backward + TFE eager forward (conditional) |
| `chibi-scheme/lib/eval/ad.scm` | Scheme layer: operator overloading, grad, math, SGD |
| `src/eval_grammar.y` | `param` keyword, `@` matmul operator |
