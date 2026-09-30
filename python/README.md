# Datasaurus Python

Python port of the datasaurus project for generating point clouds that match statistics.

This project demonstrates that datasets can have identical statistical properties (mean, variance, correlation) but visually different distributions.

## Installation

```bash
uv pip install -e ".[dev]"
```

## Usage

```python
from datasaurus import datasaurus, means, variances, co_var_matrix

# Get the famous datasaurus dataset
print(datasaurus.shape)  # (142, 2)

# Compute statistics
print(means(datasaurus))
print(variances(datasaurus))
print(co_var_matrix(datasaurus))
```

## Development

```bash
# Install development dependencies
uv pip install -e ".[dev]"

# Run tests
pytest tests/
```
