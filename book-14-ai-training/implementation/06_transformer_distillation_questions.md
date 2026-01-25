<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Transformer Distillation - Questions

**Q&A companion to [06_transformer_distillation_impl.md](./06_transformer_distillation_impl.md)**

This document contains questions about rotation-based transformer distillation. Each question links to the relevant section in the implementation documentation.

---

<a id="b14c06-q-what-is-givens-rotation"></a>
## Q: What is a Givens rotation?

A Givens rotation is a rotation in n-dimensional space that only affects two coordinates (i, j) while leaving all others unchanged.

**Properties**:
- Orthogonal: G^T G = I (preserves norms)
- Determinant 1 (proper rotation)
- Composable: Multiple rotations compose
- Sparse: Only 4 non-trivial entries

**Matrix form**:
```
G[i,i] = cos(θ)
G[j,j] = cos(θ)
G[i,j] = -sin(θ)
G[j,i] = sin(θ)
```

**Reference**: [Givens Rotation Mathematics](./06_transformer_distillation_impl.md#givens-rotation-mathematics)

---

<a id="b14c06-q-why-givens-rotations"></a>
## Q: Why use Givens rotations for distillation?

Any orthogonal matrix can be decomposed into Givens rotations. By selecting k < n(n-1)/2 rotation planes, we create a constrained rotation manifold that:

1. Has fewer parameters (k angles vs n² matrix entries)
2. Is always valid (no need to enforce orthogonality)
3. Provides interpretable transformations (angles are meaningful)

**Reference**: [Givens Rotation Mathematics](./06_transformer_distillation_impl.md#givens-rotation-mathematics)

---

<a id="b14c06-q-optimal-angle-formula"></a>
## Q: How do you compute the optimal Givens rotation angle?

For source (x_i, x_j) and target (y_i, y_j):

```
θ = atan2(y_j, y_i) - atan2(x_j, x_i)
```

The angle is normalized to [-π, π] to avoid discontinuities.

**Reference**: [compute_optimal_givens_angle()](./06_transformer_distillation_impl.md#compute_optimal_givens_angle)

---

<a id="b14c06-q-angle-normalization"></a>
## Q: Why is angle normalization important?

Angles wrap around at ±π. Without normalization:
- θ = 3π and θ = -π are the same rotation
- Gradient descent can oscillate around the wrap point

**Normalization**:
```python
while theta > math.pi:
    theta -= 2 * math.pi
while theta < -math.pi:
    theta += 2 * math.pi
```

**Reference**: [Angle Normalization](./06_transformer_distillation_impl.md#angle-normalization)

---

<a id="b14c06-q-optimal-rotation-params"></a>
## Q: How does `compute_optimal_rotation_params()` work?

Uses a **greedy sequential** approach:

1. Start with input vector
2. For each plane (i, j) in order:
   - Compute optimal angle to rotate toward target
   - Apply rotation
   - Record angle
3. Compute scale factor: `||target|| / ||rotated||`

**Why sequential?** Joint optimization is non-convex and expensive. Sequential is fast (O(k × d)) and always converges.

**Reference**: [compute_optimal_rotation_params()](./06_transformer_distillation_impl.md#compute_optimal_rotation_params)

---

<a id="b14c06-q-plane-selection"></a>
## Q: How are rotation planes selected?

The GivensRotationLayer uses an **interleaved** strategy:

```python
for k in range(num_planes):
    i = (2 * k) % embed_dim
    j = (2 * k + 1) % embed_dim
```

Creates pairs: (0,1), (2,3), (4,5), ... then wraps around to cover the embedding space.

**Reference**: [GivensRotationLayer](./06_transformer_distillation_impl.md#givensrotationlayer)

---

<a id="b14c06-q-non-inplace-rotation"></a>
## Q: Why does GivensRotationLayer use non-in-place operations?

PyTorch autograd cannot track gradients through in-place operations on tensors needed for backward pass.

**Solution**: Use `unbind` and `stack`:
```python
cols = list(x.unbind(dim=1))  # Split into columns
# ... modify cols[i], cols[j] with new tensors ...
return torch.stack(cols, dim=1)  # Reassemble
```

**Reference**: [GivensRotationLayer](./06_transformer_distillation_impl.md#givensrotationlayer)

---

<a id="b14c06-q-rotation-transformer-architecture"></a>
## Q: What is the RotationTransformer architecture?

A transformer that predicts rotation parameters instead of output vectors:

```
Input → Input Projection → Transformer Encoder → Rotation Head
                                                      ↓
                                            angles [k] + scale [1]
                                                      ↓
                                            Apply GivensRotationLayer
                                                      ↓
                                                   Output
```

**Output dimension**: 65 parameters (64 angles + 1 scale) vs 384 for direct prediction.

**Reference**: [RotationTransformer](./06_transformer_distillation_impl.md#rotationtransformer)

---

<a id="b14c06-q-scaling-modes"></a>
## Q: What scaling modes does RotationTransformer support?

Three modes:

| Mode | Description | Output params |
|------|-------------|---------------|
| `uniform` | Single scale for all dims | k + 1 |
| `per_dim` | One scale per dimension | k + d |
| `none` | No scaling (scale = 1) | k |

**Recommended**: `uniform` - maintains geometric constraint.

**Reference**: [RotationTransformer](./06_transformer_distillation_impl.md#rotationtransformer)

---

<a id="b14c06-q-standard-vs-minimal"></a>
## Q: What's the difference between standard and minimal transform projection?

**Standard** blends output vectors:
```
output = Σ wᵢ × answer_embᵢ
```
Can produce outputs **anywhere** in embedding space.

**Minimal** blends rotation parameters:
```
angles = Σ wᵢ × anglesᵢ
scale = Σ wᵢ × scaleᵢ
output = scale × R(angles) @ input
```
Output is **always** a valid rotation of input.

**Reference**: [MinimalTransformProjection](./06_transformer_distillation_impl.md#minimaltransformprojection)

---

<a id="b14c06-q-minimal-projection-example"></a>
## Q: Can you show an example of why minimal projection matters?

**Standard interpolation**:
```
head_1_output = [1, 0, 0]
head_2_output = [0, 1, 0]
interpolated = 0.5 * [1,0,0] + 0.5 * [0,1,0] = [0.5, 0.5, 0]
```
Result is NOT a rotation of input.

**Minimal interpolation**:
```
head_1_angles = [θ₁, θ₂, ...]
head_2_angles = [φ₁, φ₂, ...]
blended_angles = 0.5 * [θ₁, ...] + 0.5 * [φ₁, ...]
output = R(blended_angles) @ input  # Always valid rotation
```

**Reference**: [MinimalTransformProjection](./06_transformer_distillation_impl.md#minimaltransformprojection)

---

<a id="b14c06-q-add-head"></a>
## Q: How do I add heads to a MinimalTransformProjection?

Use `add_head()` with centroid and transformation parameters:

```python
proj.add_head(
    centroid=centroid_vector,     # [embed_dim]
    angles=rotation_angles,        # [num_planes]
    scale=scale_factor             # float
)
```

Or create from LDA data with `from_lda_heads()`.

**Reference**: [MinimalTransformProjection](./06_transformer_distillation_impl.md#minimaltransformprojection)

---

<a id="b14c06-q-angle-supervised-training"></a>
## Q: What is angle-supervised training?

Instead of only supervising on output vectors, directly supervise on rotation angles:

```
L = w_output × L_output + w_angle × L_angle + w_scale × L_scale
```

Where:
- `L_output` = MSE + cosine loss on output vectors
- `L_angle` = circular distance² on angles
- `L_scale` = MSE on scale factor

**Reference**: [train_rotation_distillation_angle_supervised()](./06_transformer_distillation_impl.md#train_rotation_distillation_angle_supervised)

---

<a id="b14c06-q-circular-loss-problem"></a>
## Q: Why does standard MSE fail for angle loss?

Angles wrap at ±π. Standard MSE gives wrong distances:

```
pred = -π + 0.1
target = π - 0.1
MSE = (pred - target)² ≈ 37  # Wrong!
Actual angular distance: 0.2 rad
```

**Reference**: [Circular Angle Loss](./06_transformer_distillation_impl.md#circular-angle-loss)

---

<a id="b14c06-q-circular-loss-solution"></a>
## Q: How does circular angle loss work?

Use `atan2` to compute shortest path around the circle:

```python
def circular_mse_loss(pred_angles, target_angles):
    diff = pred_angles - target_angles
    circular_diff = torch.atan2(torch.sin(diff), torch.cos(diff))
    return (circular_diff ** 2).mean()
```

`atan2(sin(θ), cos(θ)) = θ` for θ ∈ [-π, π], wraps correctly otherwise.

**Reference**: [Circular Angle Loss](./06_transformer_distillation_impl.md#circular-angle-loss)

---

<a id="b14c06-q-recommended-hyperparams"></a>
## Q: What are the recommended hyperparameters for angle-supervised training?

| Parameter | Recommended | Notes |
|-----------|-------------|-------|
| output_weight | 0.5 | Balance with angle supervision |
| angle_weight | 0.5 | Direct geometric supervision |
| scale_weight | 0.1 | Scale is less critical |
| cosine_weight | 0.7 | Cosine ensures direction |
| learning_rate | 1e-4 | Standard for transformers |
| num_epochs | 100-200 | Until convergence |

**Reference**: [train_rotation_distillation_angle_supervised()](./06_transformer_distillation_impl.md#train_rotation_distillation_angle_supervised)

---

<a id="b14c06-q-test-results"></a>
## Q: What accuracy does rotation distillation achieve?

Synthetic data test (8 heads, 64 rotation planes, 100 epochs):

| Metric | Result |
|--------|--------|
| Mean Cosine Similarity | **0.9997 ± 0.0001** |
| Mean MSE | 0.000001 |
| Min Cosine | 0.9991 |
| Max Cosine | 1.0000 |
| Mean angle | 28.91° |
| Max angle | 113.66° |

**Reference**: [06_transformer_distillation.md](../06_transformer_distillation.md#validation-results-synthetic-data)

---

<a id="b14c06-q-when-use-rotation"></a>
## Q: When should I use rotation-based distillation?

**Use rotation approach when**:
- Interpretability matters (angles are meaningful)
- Want constrained geometric transformations
- Need output to stay "close" to input

**Stay with standard when**:
- Transformations aren't naturally rotational
- Need maximum expressivity
- Simpler training preferred

**Reference**: [06_transformer_distillation.md](../06_transformer_distillation.md#when-to-use-rotation-based-distillation)

---

<a id="b14c06-q-parameter-efficiency"></a>
## Q: How does rotation distillation reduce parameters?

**Standard output**: 384 parameters (full embedding dimension)

**Rotation output**: 65 parameters
- 64 rotation angles
- 1 scale factor

**Reduction**: 384 → 65 = **83% fewer output parameters**

The geometric constraint acts as a strong prior.

**Reference**: [RotationTransformer](./06_transformer_distillation_impl.md#rotationtransformer)

---

<a id="b14c06-q-sequential-limitations"></a>
## Q: What are the limitations of sequential angle computation?

- Order of planes matters (different orders give different angles)
- May not find globally optimal solution
- Assumes planes are pre-selected (not optimized)

However, in practice:
- Works well when planes cover embedding space
- Fast: O(k × d) complexity
- Always converges (each step improves alignment)

**Reference**: [compute_optimal_rotation_params()](./06_transformer_distillation_impl.md#compute_optimal_rotation_params)

---

<a id="b14c06-q-gradient-flow"></a>
## Q: Does circular loss have well-defined gradients?

Yes, everywhere except exactly at ±π (measure zero).

The gradient correctly points toward the shorter path around the circle, enabling smooth optimization.

**Reference**: [Circular Angle Loss](./06_transformer_distillation_impl.md#circular-angle-loss)

---

## Question Index

| ID | Topic |
|----|-------|
| [b14c06-q-what-is-givens-rotation](#b14c06-q-what-is-givens-rotation) | Givens rotation definition |
| [b14c06-q-why-givens-rotations](#b14c06-q-why-givens-rotations) | Why use Givens rotations |
| [b14c06-q-optimal-angle-formula](#b14c06-q-optimal-angle-formula) | Optimal angle formula |
| [b14c06-q-angle-normalization](#b14c06-q-angle-normalization) | Angle normalization |
| [b14c06-q-optimal-rotation-params](#b14c06-q-optimal-rotation-params) | compute_optimal_rotation_params |
| [b14c06-q-plane-selection](#b14c06-q-plane-selection) | Plane selection strategy |
| [b14c06-q-non-inplace-rotation](#b14c06-q-non-inplace-rotation) | Non-in-place operations |
| [b14c06-q-rotation-transformer-architecture](#b14c06-q-rotation-transformer-architecture) | RotationTransformer architecture |
| [b14c06-q-scaling-modes](#b14c06-q-scaling-modes) | Scaling modes |
| [b14c06-q-standard-vs-minimal](#b14c06-q-standard-vs-minimal) | Standard vs minimal projection |
| [b14c06-q-minimal-projection-example](#b14c06-q-minimal-projection-example) | Minimal projection example |
| [b14c06-q-add-head](#b14c06-q-add-head) | Adding projection heads |
| [b14c06-q-angle-supervised-training](#b14c06-q-angle-supervised-training) | Angle-supervised training |
| [b14c06-q-circular-loss-problem](#b14c06-q-circular-loss-problem) | Why MSE fails for angles |
| [b14c06-q-circular-loss-solution](#b14c06-q-circular-loss-solution) | Circular loss solution |
| [b14c06-q-recommended-hyperparams](#b14c06-q-recommended-hyperparams) | Recommended hyperparameters |
| [b14c06-q-test-results](#b14c06-q-test-results) | Test results |
| [b14c06-q-when-use-rotation](#b14c06-q-when-use-rotation) | When to use rotation distillation |
| [b14c06-q-parameter-efficiency](#b14c06-q-parameter-efficiency) | Parameter efficiency |
| [b14c06-q-sequential-limitations](#b14c06-q-sequential-limitations) | Sequential algorithm limitations |
| [b14c06-q-gradient-flow](#b14c06-q-gradient-flow) | Gradient flow |
