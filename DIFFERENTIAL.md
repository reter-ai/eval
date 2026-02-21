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
| `gelu(x)` | Gaussian error linear unit |
| `silu(x)` | SiLU / Swish activation |

### Tensor-Specific
| Function | Description |
|----------|-------------|
| `a @ b` | Matrix multiplication |
| `transpose(x)` | Matrix transpose (2D) |
| `sum(x)` | Sum all elements to scalar |
| `sum(x, axis)` | Sum along specific axis |
| `mean(x)` | Mean of all elements |
| `mean(x, axis)` | Mean along specific axis |
| `softmax(x)` / `softmax(x, axis)` | Axis-aware softmax (default: last dim) |
| `reshape(x, shape)` | Reshape tensor (same data, new shape) |
| `tensor_slice(x, begin, size)` | Extract subtensor by begin+size lists |
| `concat(tensors)` / `concat(tensors, axis)` | Concatenate tensors along axis (default: 0) |
| `gather(x, indices)` / `gather(x, indices, axis)` | Index-select along axis (embedding lookup) |
| `layer_norm(x, gamma, beta)` | Layer normalization (optional epsilon) |
| `where(cond, a, b)` | Conditional element-wise select |
| `batch_matmul(a, b)` | Batched matrix multiply (3D+) |
| `conv2d(input, kernel)` | 2D convolution, NCHW layout (im2col+GEMM) |
| `conv2d(input, kernel, stride, pad, dilation)` | Conv2D with explicit parameters |
| `max_pool2d(input, ksize)` | 2D max pooling, NCHW layout |
| `max_pool2d(input, ksize, stride, pad)` | MaxPool2D with explicit parameters |
| `avg_pool2d(input, ksize)` | 2D average pooling (count_include_pad=false) |
| `avg_pool2d(input, ksize, stride, pad)` | AvgPool2D with explicit parameters |

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
| `gelu` | `Tanh` + `Mul` + `Add` + `Pow` | Composed (no atomic TF GELU) |
| `silu` | `Mul(x, Sigmoid(x))` | Composed |
| `softmax` | `Softmax` | Axis-aware |
| `sum(x, axis)` | `Sum` | With reduction_indices |
| `mean(x, axis)` | `Mean` | With reduction_indices |
| `reshape` | `Reshape` | With int32 shape const |
| `tensor_slice` | `Slice` | With begin + size consts |
| `concat` | `ConcatV2` | N inputs + axis const |
| `gather` | `GatherV2` | With indices + axis consts |
| `layer_norm` | Native C fallback | Composed normalization |
| `where` | `SelectV2` | With Cast(cond → bool) |
| `batch_matmul` | `BatchMatMulV2` | 3D+ batched multiply |
| `conv2d` | Native C fallback | NCHW im2col+GEMM |
| `max_pool2d` | Native C fallback | NCHW max pooling |
| `avg_pool2d` | Native C fallback | NCHW average pooling |

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

### Delay-Load (Optional TF at Runtime)

On Windows (MSVC), TensorFlow is linked with `/DELAYLOAD`. The binary runs without `tensorflow.dll` — all ops use native C. If `tensorflow.dll` is found next to the executable at runtime, TF acceleration activates automatically:

```
With tensorflow.dll:    TF graph-mode backward + TFE eager forward
Without tensorflow.dll: Native C backward + native C forward (same results)
```

This means a single binary works everywhere. Drop `tensorflow.dll` alongside it for GPU acceleration; remove it for a dependency-free deployment.

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

Built-in axis-aware softmax with numerically stable max-subtraction:

```
param logits = [[2.0, 1.0, 0.1]];
define probs = softmax(logits);          // row-wise softmax (default: last axis)
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

A single-head self-attention layer using built-in row-wise softmax:

```
// Self-attention: Attention(Q,K,V) = softmax(Q @ K^T / sqrt(d_k)) @ V
//
// input: [seq_len, d_model] = [3, 4]
// W_q, W_k, W_v: [d_model, d_k] = [4, 2]

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

// Row-wise softmax (axis -1 = last dimension, the default)
define attn = softmax(scores);     // [3, 3] — proper per-row softmax

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

### Multi-Head Attention with Reshape

Using reshape, gather, layer normalization, and GELU — the full transformer building blocks:

```
// Multi-head attention: split d_model into heads, attend, recombine
// input: [seq_len, d_model] = [4, 8], heads = 2, d_k = 4

param W_qkv = [[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8],
               [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.1],
               [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.1, 0.2],
               [0.4, 0.5, 0.6, 0.7, 0.8, 0.1, 0.2, 0.3],
               [0.5, 0.6, 0.7, 0.8, 0.1, 0.2, 0.3, 0.4],
               [0.6, 0.7, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5],
               [0.7, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6],
               [0.8, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]];

param gamma = [[1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]];
param beta  = [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]];

define input = tensor([[1.0, 0.5, 0.3, 0.2, 0.8, 0.1, 0.4, 0.6],
                       [0.2, 0.8, 0.1, 0.7, 0.3, 0.9, 0.5, 0.2],
                       [0.6, 0.3, 0.7, 0.1, 0.5, 0.4, 0.2, 0.8],
                       [0.4, 0.6, 0.2, 0.9, 0.1, 0.7, 0.8, 0.3]]);

// Layer norm → project → attention → GELU FFN
define normed = layer_norm(input, gamma, beta);
define projected = normed @ W_qkv;

// Reshape for head splitting: [4, 8] → [4, 2, 4] (seq, heads, d_k)
define heads = reshape(projected, [4, 2, 4]);

// GELU activation in feed-forward network
define ffn_out = gelu(projected);
define loss = sum(ffn_out);
backward(loss);

display("FFN output shape: "); display(ffn_out->shape); newline();
display("W_qkv grad: "); display(W_qkv->grad); newline()
```

### Embedding Lookup with Gather

Differentiable embedding lookup using `gather` — gradients scatter back to the selected rows:

```
// Vocabulary embedding: 5 tokens, embedding dim = 3
param embeddings = [[0.1, 0.2, 0.3],
                    [0.4, 0.5, 0.6],
                    [0.7, 0.8, 0.9],
                    [1.0, 1.1, 1.2],
                    [1.3, 1.4, 1.5]];

// Token indices for sequence "the cat sat" → [1, 3, 2]
define token_ids = [1, 3, 2];
define embedded = gather(embeddings, token_ids);
// => [[0.4, 0.5, 0.6], [1.0, 1.1, 1.2], [0.7, 0.8, 0.9]]

define loss = sum(embedded);
backward(loss);

display("Embedded: "); display(embedded->data); newline();
display("Embedding grad: "); display(embeddings->grad); newline();
// Only rows 1, 2, 3 receive gradient; rows 0, 4 stay zero
```

### Conditional Masking with Where

Apply attention masks using `where` — gradients flow only through the selected branch:

```
param scores = [[1.0, 2.0, 3.0],
                [4.0, 5.0, 6.0]];

// Mask: 1 = attend, 0 = mask out
define mask = tensor([[1.0, 1.0, 0.0],
                      [1.0, 0.0, 0.0]]);

// Replace masked positions with -1e9 (effectively -inf for softmax)
define masked = where(mask, scores, tensor([[-1e9, -1e9, -1e9],
                                             [-1e9, -1e9, -1e9]]));

define attn = softmax(masked);
define loss = sum(attn);
backward(loss);

display("Masked attention: "); display(attn->data); newline();
display("Scores grad: "); display(scores->grad); newline();
// Masked positions get zero gradient
```

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

### Conv2D, MaxPool2D, AvgPool2D

Convolutional and pooling operations use **NCHW layout**: `[batch, channels, height, width]`. Convolution uses im2col + GEMM internally.

**Signatures:**

```
conv2d(input, kernel)                         // stride=1, pad=0, dilation=1
conv2d(input, kernel, stride)                 // square stride
conv2d(input, kernel, stride, pad)            // square stride and pad
conv2d(input, kernel, stride, pad, dilation)  // full control

max_pool2d(input, ksize)                      // stride=ksize, pad=0
max_pool2d(input, ksize, stride)              // custom stride
max_pool2d(input, ksize, stride, pad)         // full control

avg_pool2d(input, ksize)                      // same signature as max_pool2d
avg_pool2d(input, ksize, stride, pad)
```

Each of `stride`, `pad`, `dilation`, and `ksize` can be an integer (square) or a `(h w)` list for asymmetric settings.

**Input shapes:**
- **input**: `[N, C_in, H, W]`
- **kernel** (conv2d only): `[C_out, C_in, kH, kW]`

**Output dimension formulas:**
- Conv2D: `H_out = (H + 2*pad_h - dil_h*(kH-1) - 1) / stride_h + 1`
- Pool2D: `H_out = (H + 2*pad_h - kH) / stride_h + 1`

```
// Conv2D: 1-batch, 1-channel 4x4 input, 1-filter 3x3 kernel
tape_reset();
param input = [[[[1.0, 2.0, 3.0, 4.0],
                  [5.0, 6.0, 7.0, 8.0],
                  [9.0, 10.0, 11.0, 12.0],
                  [13.0, 14.0, 15.0, 16.0]]]];
param kernel = [[[[1.0, 0.0, -1.0],
                   [1.0, 0.0, -1.0],
                   [1.0, 0.0, -1.0]]]];

define out = conv2d(input, kernel);       // stride=1, pad=0, dilation=1
display(out->shape);                      // (1 1 2 2)
define loss = sum(out);
backward(loss);
display(kernel->grad);                    // gradient w.r.t. kernel

// Padded convolution (same-size output)
tape_reset();
param I = [[[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]]];
param K = [[[[1.0, 1.0, 1.0], [1.0, 1.0, 1.0], [1.0, 1.0, 1.0]]]];
define out = conv2d(I, K, 1, 1);          // stride=1, pad=1 => 3x3 output
display(out->shape);                      // (1 1 3 3) — same spatial size
```

```
// MaxPool2D: 2x2 pool on 4x4
tape_reset();
param X = [[[[1.0, 2.0, 3.0, 4.0],
              [5.0, 6.0, 7.0, 8.0],
              [9.0, 10.0, 11.0, 12.0],
              [13.0, 14.0, 15.0, 16.0]]]];
define pooled = max_pool2d(X, 2);         // 2x2 window, stride=2
display(pooled->data);                    // ((((6.0 8.0) (14.0 16.0))))
define loss = sum(pooled);
backward(loss);
display(X->grad);                         // 1.0 at max positions, 0.0 elsewhere
```

```
// AvgPool2D: 2x2 pool on 4x4
tape_reset();
param Y = [[[[1.0, 2.0, 3.0, 4.0],
              [5.0, 6.0, 7.0, 8.0],
              [9.0, 10.0, 11.0, 12.0],
              [13.0, 14.0, 15.0, 16.0]]]];
define avg = avg_pool2d(Y, 2);
display(avg->data);                       // ((((3.5 5.5) (11.5 13.5))))
define loss = sum(avg);
backward(loss);
display(Y->grad);                         // 0.25 everywhere (1/4 per window)
```

### CNN Pipeline: Conv, ReLU, Pool

A conv-relu-pool pipeline with end-to-end gradient flow:

```
tape_reset();
param inp = [[[[1.0, 2.0, 3.0, 4.0],
                [5.0, 6.0, 7.0, 8.0],
                [9.0, 10.0, 11.0, 12.0],
                [13.0, 14.0, 15.0, 16.0]]]];
param ker = [[[[1.0, 1.0, 1.0],
                [1.0, 1.0, 1.0],
                [1.0, 1.0, 1.0]]]];

// conv -> relu -> max_pool
define conv_out = conv2d(inp, ker);       // [1,1,2,2]
define activated = relu(conv_out);        // ReLU activation
define pooled = max_pool2d(activated, 2); // [1,1,1,1]

display(pooled->data);                    // ((((99.0))))
define loss = sum(pooled);
backward(loss);
display(ker->grad);                       // gradients flow through pool -> relu -> conv
```

Gradients propagate backward through the entire pipeline: max_pool selects the max position, relu gates the gradient, and conv2d distributes it to both the kernel and input via im2col/col2im.

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

- **CPU-only forward by default**: TFE eager ops provide GPU forward, but data round-trips through CPU memory (no persistent GPU tensors).
- **Single-precision not supported**: All computation is float64 (double). No float32/float16 mode.
- **No sparse ops**: Sparse matrix multiply, sparse embeddings, and sparse gradients are not supported.

The 35 differentiable ops (arithmetic, activations, softmax, axis reductions, reshape, slice, concat, gather, layer norm, where, batch matmul, conv2d, max_pool2d, avg_pool2d) are sufficient for full transformer and CNN training including multi-head attention, embedding lookup, masked attention, feed-forward networks with GELU/SiLU, and convolutional architectures (LeNet, ResNet, etc.).

## Files

| File | Role |
|------|------|
| `src/_eval_ad.h` | Type declarations, dispatch table, op enum |
| `src/_eval_ad.c` | Tape, native C ops, type registration, bridge functions |
| `src/_eval_tf.c` | TF graph-mode backward + TFE eager forward (conditional) |
| `chibi-scheme/lib/eval/ad.scm` | Scheme layer: operator overloading, grad, math, SGD |
| `src/eval_grammar.y` | `param` keyword, `@` matmul operator |
