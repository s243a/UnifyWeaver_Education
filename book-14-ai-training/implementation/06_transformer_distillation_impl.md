<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6 Implementation: Transformer Distillation

**Detailed function documentation for RAG systems**

This document provides implementation details for all key functions and classes in the transformer distillation system. Each section includes mathematical derivations, algorithm descriptions, and code explanations.

---

## Table of Contents

1. [Givens Rotation Mathematics](#givens-rotation-mathematics)
2. [compute_optimal_givens_angle()](#compute_optimal_givens_angle)
3. [compute_optimal_rotation_params()](#compute_optimal_rotation_params)
4. [GivensRotationLayer](#givensrotationlayer)
5. [RotationTransformer](#rotationtransformer)
6. [MinimalTransformProjection](#minimaltransformprojection)
7. [train_rotation_distillation_angle_supervised()](#train_rotation_distillation_angle_supervised)
8. [Circular Angle Loss](#circular-angle-loss)

---

## Givens Rotation Mathematics

### Definition

A **Givens rotation** is a rotation in n-dimensional space that only affects two coordinates (i, j) while leaving all others unchanged. The rotation matrix G(i, j, θ) is the identity matrix with four modified entries:

```
G[i,i] = cos(θ)
G[j,j] = cos(θ)
G[i,j] = -sin(θ)
G[j,i] = sin(θ)
```

### Properties

1. **Orthogonal**: G^T G = I (preserves vector norms)
2. **Determinant 1**: det(G) = 1 (proper rotation, no reflection)
3. **Composable**: Multiple Givens rotations compose: G₁ G₂ ... Gₖ
4. **Sparse**: Only 4 non-trivial entries, efficient to apply

### Why Givens Rotations?

Any orthogonal matrix can be decomposed into a product of Givens rotations. For an n×n matrix, at most n(n-1)/2 rotations are needed. By selecting k < n(n-1)/2 rotation planes, we create a **constrained** rotation manifold that:

1. Has fewer parameters (k angles vs n² matrix entries)
2. Is always valid (no need to enforce orthogonality)
3. Provides interpretable transformations (angles are meaningful)

---

## compute_optimal_givens_angle()

### Purpose

Computes the optimal rotation angle θ for a single 2D plane (i, j) that rotates vector component (x_i, x_j) to align with target component (y_i, y_j).

### Mathematical Derivation

Given source point (x_i, x_j) and target point (y_i, y_j), we want θ such that:

```
[cos(θ)  -sin(θ)] [x_i]   [y_i]
[sin(θ)   cos(θ)] [x_j] ≈ [y_j]
```

The angle of a 2D vector (a, b) is atan2(b, a). The optimal rotation is:

```
θ = angle(target) - angle(source)
θ = atan2(y_j, y_i) - atan2(x_j, x_i)
```

### Angle Normalization

Angles must be normalized to [-π, π] to avoid discontinuities:

```python
while theta > math.pi:
    theta -= 2 * math.pi
while theta < -math.pi:
    theta += 2 * math.pi
```

### Implementation

```python
def compute_optimal_givens_angle(x_i: float, x_j: float,
                                  y_i: float, y_j: float) -> float:
    """
    Compute optimal Givens rotation angle for a single 2D plane.

    Args:
        x_i: Source vector component i
        x_j: Source vector component j
        y_i: Target vector component i
        y_j: Target vector component j

    Returns:
        Optimal rotation angle in radians, normalized to [-π, π]
    """
    x_angle = math.atan2(x_j, x_i)
    y_angle = math.atan2(y_j, y_i)
    theta = y_angle - x_angle

    # Normalize to [-π, π]
    while theta > math.pi:
        theta -= 2 * math.pi
    while theta < -math.pi:
        theta += 2 * math.pi

    return theta
```

### Edge Cases

- **Zero source vector**: atan2(0, 0) returns 0; any angle is equally valid
- **Zero target vector**: The rotation doesn't matter since we're targeting zero
- **Large angles**: Normalized to avoid θ = 3π when θ = -π is equivalent

---

## compute_optimal_rotation_params()

### Purpose

Computes optimal rotation angles for all k planes plus a scale factor, given input and target vectors.

### Algorithm

The algorithm uses a **greedy sequential** approach:

1. Start with the input vector
2. For each plane (i, j) in order:
   a. Compute optimal angle to rotate current vector toward target
   b. Apply the rotation
   c. Record the angle
3. After all rotations, compute scale factor

### Why Sequential (Not Joint)?

Joint optimization of all angles simultaneously is non-convex and computationally expensive. Sequential optimization:

1. Is fast: O(k × d) where k is planes, d is dimension
2. Always converges (each step improves alignment)
3. Works well in practice when planes are well-distributed

### Scale Factor Computation

After rotation, compute uniform scale:

```
scale = ||target|| / ||rotated||
```

This ensures the final output matches the target norm.

### Implementation

```python
def compute_optimal_rotation_params(
    input_vec: torch.Tensor,
    target_vec: torch.Tensor,
    planes: List[Tuple[int, int]]
) -> Tuple[torch.Tensor, torch.Tensor]:
    """
    Compute optimal rotation angles and scale for input→target transformation.

    Args:
        input_vec: Input vector [embed_dim]
        target_vec: Target vector [embed_dim]
        planes: List of (i, j) index pairs for rotation planes

    Returns:
        angles: Tensor of optimal angles [num_planes]
        scale: Tensor with single scale factor [1]
    """
    current = input_vec.clone().detach().cpu().numpy()
    target = target_vec.clone().detach().cpu().numpy()
    angles = []

    for (i, j) in planes:
        # Compute optimal angle for this plane
        theta = compute_optimal_givens_angle(
            current[i], current[j],
            target[i], target[j]
        )
        angles.append(theta)

        # Apply rotation to current vector
        cos_t, sin_t = math.cos(theta), math.sin(theta)
        new_i = cos_t * current[i] - sin_t * current[j]
        new_j = sin_t * current[i] + cos_t * current[j]
        current[i], current[j] = new_i, new_j

    # Compute scale factor
    rotated_norm = np.linalg.norm(current)
    target_norm = np.linalg.norm(target)
    scale = target_norm / (rotated_norm + 1e-8)

    return (
        torch.tensor(angles, dtype=torch.float32),
        torch.tensor([scale], dtype=torch.float32)
    )
```

### Limitations

- Order of planes matters (different orders give different angles)
- May not find globally optimal solution
- Assumes planes are pre-selected (not optimized)

---

## GivensRotationLayer

### Purpose

Applies a sequence of Givens rotations to input vectors, parameterized by k angles.

### Plane Selection Strategy

The layer pre-computes which (i, j) pairs to rotate. The selection uses an **interleaved** strategy to cover the embedding space:

```python
def _generate_planes(self) -> List[Tuple[int, int]]:
    planes = []
    for k in range(self.num_planes):
        i = (2 * k) % self.embed_dim
        j = (2 * k + 1) % self.embed_dim
        if i != j:
            planes.append((i, j))
    return planes
```

This creates pairs: (0,1), (2,3), (4,5), ... then wraps around.

### Forward Pass

The forward pass applies rotations sequentially using **non-in-place** operations to preserve autograd:

```python
def forward(self, x: torch.Tensor, angles: torch.Tensor) -> torch.Tensor:
    """
    Apply Givens rotations to input vectors.

    Args:
        x: Input vectors [batch, embed_dim]
        angles: Rotation angles [batch, num_planes]

    Returns:
        Rotated vectors [batch, embed_dim]
    """
    # Split into columns for non-in-place modification
    cols = list(x.unbind(dim=1))  # List of [batch] tensors

    for k, (i, j) in enumerate(self.planes):
        theta = angles[:, k]  # [batch]
        cos_t = torch.cos(theta)
        sin_t = torch.sin(theta)

        # Compute rotated values
        new_i = cos_t * cols[i] - sin_t * cols[j]
        new_j = sin_t * cols[i] + cos_t * cols[j]

        # Update columns (creates new tensors, no in-place)
        cols[i] = new_i
        cols[j] = new_j

    # Reassemble
    return torch.stack(cols, dim=1)
```

### Why Non-In-Place?

PyTorch autograd cannot track gradients through in-place operations on tensors that are needed for backward pass. Using `unbind` and `stack` creates new tensors at each step, preserving the computation graph.

### Memory Considerations

- Stores k (i, j) pairs: O(k) memory
- Forward pass: O(k × batch) operations
- No learnable parameters (angles come from transformer)

---

## RotationTransformer

### Purpose

A transformer that predicts rotation parameters (k angles + scale) instead of output vectors directly.

### Architecture

```
Input [batch, embed_dim]
    ↓
Input Projection (Linear: embed_dim → embed_dim)
    ↓
Add sequence dimension [batch, 1, embed_dim]
    ↓
Transformer Encoder (L layers, H heads each)
    ↓
Remove sequence dimension [batch, embed_dim]
    ↓
Rotation Head (Linear: embed_dim → num_planes + 1)
    ↓
Split into angles [batch, num_planes] and scale [batch, 1]
    ↓
Apply GivensRotationLayer with predicted angles
    ↓
Multiply by scale
    ↓
Output [batch, embed_dim]
```

### Output Dimension

The rotation head outputs `num_planes + 1` values:
- `num_planes` rotation angles (in radians)
- 1 scale factor

For embed_dim=384 and num_planes=64: **65 output parameters** vs 384 for direct prediction.

### Scaling Modes

Three modes for scale prediction:

1. **uniform**: Single scale factor for all dimensions
2. **per_dim**: One scale per dimension (not recommended, loses geometric constraint)
3. **none**: No scaling (scale = 1.0)

### Implementation

```python
class RotationTransformer:
    def __init__(
        self,
        embed_dim: int = 384,
        num_rotation_planes: int = 64,
        num_heads: int = 4,
        num_layers: int = 2,
        ff_dim: int = 512,
        scaling_mode: str = "uniform"
    ):
        self.embed_dim = embed_dim
        self.num_rotation_planes = num_rotation_planes
        self.scaling_mode = scaling_mode

        # Rotation layer (applies predicted angles)
        self.rotation_layer = GivensRotationLayer(embed_dim, num_rotation_planes)

        # Input projection
        self.input_proj = nn.Linear(embed_dim, embed_dim)

        # Transformer encoder
        encoder_layer = nn.TransformerEncoderLayer(
            d_model=embed_dim,
            nhead=num_heads,
            dim_feedforward=ff_dim,
            activation='gelu',
            batch_first=True
        )
        self.encoder = nn.TransformerEncoder(encoder_layer, num_layers)

        # Rotation parameter head
        if scaling_mode == "uniform":
            output_dim = num_rotation_planes + 1
        elif scaling_mode == "per_dim":
            output_dim = num_rotation_planes + embed_dim
        else:
            output_dim = num_rotation_planes

        self.rotation_head = nn.Linear(embed_dim, output_dim)

    def forward(self, query: torch.Tensor) -> torch.Tensor:
        # Project input
        x = self.input_proj(query)

        # Add sequence dimension
        x = x.unsqueeze(1)  # [batch, 1, embed_dim]

        # Transformer encoding
        x = self.encoder(x)

        # Remove sequence dimension
        x = x.squeeze(1)  # [batch, embed_dim]

        # Predict rotation parameters
        params = self.rotation_head(x)

        # Split angles and scale
        angles = params[:, :self.num_rotation_planes]

        if self.scaling_mode == "uniform":
            scale = params[:, self.num_rotation_planes:].abs() + 0.1
        elif self.scaling_mode == "per_dim":
            scale = params[:, self.num_rotation_planes:].abs() + 0.1
        else:
            scale = torch.ones(query.shape[0], 1, device=query.device)

        # Apply rotation
        rotated = self.rotation_layer.forward(query, angles)

        # Apply scale
        if self.scaling_mode == "per_dim":
            output = rotated * scale
        else:
            output = rotated * scale

        return output
```

---

## MinimalTransformProjection

### Purpose

A multi-head projection that blends **rotation parameters** instead of output vectors, keeping transformations in the rotation manifold during interpolation.

### Standard vs Minimal

**Standard Multi-Head Projection**:
```
weights = softmax(query @ centroids.T / τ)
output = weights @ answer_embeddings  # Blends output vectors
```

**Minimal Transform Projection**:
```
weights = softmax(query @ centroids.T / τ)
angles = weights @ head_angles        # Blends rotation parameters
scale = weights @ head_scales
output = scale * R(angles) @ query    # Applies blended rotation
```

### Why This Matters

Standard interpolation can produce outputs **anywhere** in embedding space:

```
head_1_output = [1, 0, 0]
head_2_output = [0, 1, 0]
interpolated = 0.5 * [1,0,0] + 0.5 * [0,1,0] = [0.5, 0.5, 0]
```

The interpolated output is not necessarily a rotation of the input.

Minimal interpolation stays geometric:

```
head_1_angles = [θ₁, θ₂, ...]
head_2_angles = [φ₁, φ₂, ...]
interpolated_angles = 0.5 * [θ₁, θ₂, ...] + 0.5 * [φ₁, φ₂, ...]
output = R(interpolated_angles) @ input  # Always a valid rotation
```

### Implementation

```python
class MinimalTransformProjection:
    def __init__(self, embed_dim: int, num_rotation_planes: int = 64):
        self.embed_dim = embed_dim
        self.num_rotation_planes = num_rotation_planes
        self.rotation_layer = GivensRotationLayer(embed_dim, num_rotation_planes)

        self.centroids = []      # [num_heads, embed_dim]
        self.head_angles = []    # [num_heads, num_planes]
        self.head_scales = []    # [num_heads]
        self.temperature = 1.0

    def add_head(
        self,
        centroid: np.ndarray,
        angles: np.ndarray,
        scale: float
    ):
        """Add a head with its centroid and transformation parameters."""
        self.centroids.append(centroid)
        self.head_angles.append(angles)
        self.head_scales.append(scale)

    def project(self, query: torch.Tensor) -> torch.Tensor:
        """
        Project query using rotation-interpolated multi-head attention.

        Args:
            query: Input vectors [batch, embed_dim]

        Returns:
            Transformed vectors [batch, embed_dim]
        """
        # Compute attention weights
        centroids = torch.tensor(
            np.array(self.centroids),
            dtype=torch.float32,
            device=query.device
        )
        logits = query @ centroids.T / self.temperature
        weights = F.softmax(logits, dim=-1)  # [batch, num_heads]

        # Interpolate rotation parameters
        angles_tensor = torch.tensor(
            np.array(self.head_angles),
            dtype=torch.float32,
            device=query.device
        )  # [num_heads, num_planes]

        scales_tensor = torch.tensor(
            np.array(self.head_scales),
            dtype=torch.float32,
            device=query.device
        )  # [num_heads]

        # Weighted combination of angles and scales
        blended_angles = weights @ angles_tensor  # [batch, num_planes]
        blended_scale = weights @ scales_tensor   # [batch]

        # Apply rotation
        rotated = self.rotation_layer.forward(query, blended_angles)

        # Apply scale
        output = rotated * blended_scale.unsqueeze(-1)

        return output
```

### Head Initialization

Heads can be initialized from LDA cluster data:

```python
@classmethod
def from_lda_heads(
    cls,
    centroids: np.ndarray,      # [num_heads, embed_dim]
    query_examples: np.ndarray, # [num_heads, embed_dim]
    answer_examples: np.ndarray # [num_heads, embed_dim]
) -> 'MinimalTransformProjection':
    """
    Create projection from LDA cluster data.

    For each head, computes optimal rotation from query→answer example.
    """
    embed_dim = centroids.shape[1]
    num_planes = embed_dim // 6  # Heuristic

    proj = cls(embed_dim, num_planes)

    for i in range(len(centroids)):
        angles, scale = compute_optimal_rotation_params(
            torch.tensor(query_examples[i]),
            torch.tensor(answer_examples[i]),
            proj.rotation_layer.planes
        )
        proj.add_head(
            centroid=centroids[i],
            angles=angles.numpy(),
            scale=scale.item()
        )

    return proj
```

---

## train_rotation_distillation_angle_supervised()

### Purpose

Trains the RotationTransformer using **direct supervision on rotation angles**, not just output vectors.

### Loss Function

The loss combines three components:

```
L = w_output × L_output + w_angle × L_angle + w_scale × L_scale

L_output = (1-λ) × MSE(pred, target) + λ × (1 - cosine_sim(pred, target))
L_angle = mean(circular_distance(pred_angles, target_angles)²)
L_scale = MSE(pred_scale, target_scale)
```

### Circular Angle Loss

Angles wrap around at ±π. Standard MSE fails:

```
pred_angle = -3.1 rad
target_angle = 3.1 rad
MSE = (-3.1 - 3.1)² = 38.44  # Wrong! Actual distance is ~0.08 rad
```

Circular distance uses atan2:

```python
def circular_distance(pred, target):
    diff = pred - target
    return torch.atan2(torch.sin(diff), torch.cos(diff))
```

This returns the shortest angular distance in [-π, π].

### Implementation

```python
def train_rotation_distillation_angle_supervised(
    transformer: RotationTransformer,
    teacher_projection,  # MinimalTransformProjection or similar
    query_embeddings: np.ndarray,
    num_epochs: int = 200,
    batch_size: int = 32,
    learning_rate: float = 1e-4,
    output_weight: float = 0.5,
    angle_weight: float = 0.5,
    scale_weight: float = 0.1,
    cosine_weight: float = 0.7
) -> List[float]:
    """
    Train transformer with angle supervision.

    Args:
        transformer: RotationTransformer to train
        teacher_projection: Teacher model (must have rotation parameters)
        query_embeddings: Training queries [N, embed_dim]
        num_epochs: Training epochs
        batch_size: Batch size
        learning_rate: Learning rate
        output_weight: Weight for output vector loss
        angle_weight: Weight for angle prediction loss
        scale_weight: Weight for scale prediction loss
        cosine_weight: Cosine vs MSE balance in output loss

    Returns:
        List of epoch losses
    """
    optimizer = torch.optim.Adam(transformer.parameters(), lr=learning_rate)
    losses = []

    for epoch in range(num_epochs):
        epoch_loss = 0.0
        num_batches = 0

        # Shuffle data
        indices = np.random.permutation(len(query_embeddings))

        for start in range(0, len(indices), batch_size):
            batch_idx = indices[start:start+batch_size]
            queries = torch.tensor(
                query_embeddings[batch_idx],
                dtype=torch.float32,
                device=transformer.device
            )

            # Get teacher outputs and parameters
            with torch.no_grad():
                teacher_output, teacher_info = teacher_projection.project_with_info(queries)
                target_angles = teacher_info['blended_angles']
                target_scale = teacher_info['blended_scale']

            # Forward pass
            optimizer.zero_grad()

            pred_output = transformer(queries)
            pred_params = transformer.get_rotation_params(queries)
            pred_angles = pred_params[:, :transformer.num_rotation_planes]
            pred_scale = pred_params[:, transformer.num_rotation_planes:]

            # Output loss (MSE + cosine)
            mse_loss = F.mse_loss(pred_output, teacher_output)
            cosine_loss = 1 - F.cosine_similarity(
                pred_output, teacher_output, dim=-1
            ).mean()
            output_loss = (1 - cosine_weight) * mse_loss + cosine_weight * cosine_loss

            # Angle loss (circular)
            angle_diff = pred_angles - target_angles
            circular_diff = torch.atan2(
                torch.sin(angle_diff),
                torch.cos(angle_diff)
            )
            angle_loss = (circular_diff ** 2).mean()

            # Scale loss
            scale_loss = F.mse_loss(pred_scale, target_scale)

            # Total loss
            loss = (
                output_weight * output_loss +
                angle_weight * angle_loss +
                scale_weight * scale_loss
            )

            loss.backward()
            optimizer.step()

            epoch_loss += loss.item()
            num_batches += 1

        avg_loss = epoch_loss / num_batches
        losses.append(avg_loss)

        if epoch % 20 == 0:
            print(f"Epoch {epoch}: loss = {avg_loss:.6f}")

    return losses
```

### Hyperparameter Guidelines

| Parameter | Recommended | Notes |
|-----------|-------------|-------|
| output_weight | 0.5 | Balance with angle supervision |
| angle_weight | 0.5 | Direct geometric supervision |
| scale_weight | 0.1 | Scale is less critical |
| cosine_weight | 0.7 | Cosine ensures directional alignment |
| learning_rate | 1e-4 | Standard for transformer training |
| num_epochs | 100-200 | Until convergence |

---

## Circular Angle Loss

### The Problem

Angles are periodic: θ and θ + 2π represent the same rotation. Standard losses fail:

```
# Standard MSE
pred = -π + 0.1
target = π - 0.1
MSE = (pred - target)² = (−π+0.1 − π+0.1)² = (−2π+0.2)² ≈ 37

# Actual angular distance: 0.2 rad
```

### The Solution

Use atan2 to compute the shortest path around the circle:

```python
def circular_mse_loss(pred_angles, target_angles):
    """
    Compute MSE loss for angles with proper wraparound handling.

    Args:
        pred_angles: Predicted angles [batch, num_planes]
        target_angles: Target angles [batch, num_planes]

    Returns:
        Scalar loss
    """
    diff = pred_angles - target_angles

    # Map to [-π, π] via atan2
    circular_diff = torch.atan2(torch.sin(diff), torch.cos(diff))

    # Square and mean
    return (circular_diff ** 2).mean()
```

### Why atan2?

The function atan2(sin(θ), cos(θ)) = θ for θ ∈ [-π, π]. For θ outside this range, it wraps correctly:

```
atan2(sin(3π), cos(3π)) = atan2(sin(π), cos(π)) = π
atan2(sin(-3π), cos(-3π)) = atan2(sin(-π), cos(-π)) = -π
```

### Gradient Flow

The circular loss has well-defined gradients everywhere except at exactly ±π (measure zero). The gradient correctly points toward the shorter path around the circle.

---

## Summary

| Function | Purpose | Key Insight |
|----------|---------|-------------|
| `compute_optimal_givens_angle` | Optimal 2D rotation | θ = atan2(y_j,y_i) - atan2(x_j,x_i) |
| `compute_optimal_rotation_params` | All angles + scale | Greedy sequential, not joint |
| `GivensRotationLayer` | Apply rotations | Non-in-place for autograd |
| `RotationTransformer` | Predict rotation params | 65 params vs 384 direct |
| `MinimalTransformProjection` | Blend rotation params | Stay in rotation manifold |
| `train_rotation_distillation_angle_supervised` | Direct angle supervision | Circular loss essential |

## Source Files

- `scripts/test_rotation_distillation.py`
- `src/unifyweaver/targets/python_runtime/rotation_transformer.py`

## References

- Givens, W. (1958). "Computation of Plane Unitary Rotations Transforming a General Matrix to Triangular Form"
- Golub & Van Loan (2013). "Matrix Computations", Chapter 5.1
