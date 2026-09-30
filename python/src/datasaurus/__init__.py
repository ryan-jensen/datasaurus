"""
Datasaurus - Python port for generating point clouds that match statistics.

This package provides tools for creating point clouds (datasets) that have
identical statistical properties (mean, variance, correlation, etc.) but
visually different distributions.
"""

from .point import Point, PointCloud, distance, norm, quadrance
from .simplex import Simplex, dim, as_matrix, as_vectors, translate_to_origin
from .simplicial_complex import SimplicialComplex, qd_to_sc, sc_from_lists
from .datasaurus_core import (
    datasaurus_raw,
    datasaurus,
    PointCloudState,
    mk_point_cloud,
    iters,
    cooling,
)

# Re-export for convenience
from .datasaurus_core import (
    means, variances, co_var_matrix,
    total_qd,
    swap_row, take_rows, drop_rows, as_row,
    v_lines2, v_lines4, h_lines2, h_lines4,
    sqr, x_shape, wedge, wedge4, grid_shape, s1, sqr4,
)

__version__ = "0.2.0.0"
__all__ = [
    # Point module
    "Point",
    "PointCloud",
    "distance",
    "norm",
    "quadrance",
    # Simplex module
    "Simplex",
    "dim",
    "as_matrix",
    "as_vectors",
    "translate_to_origin",
    # SimplicialComplex module
    "SimplicialComplex",
    "qd_to_sc",
    "sc_from_lists",
    # Datasaurus module
    "datasaurus_raw",
    "datasaurus",
    "PointCloudState",
    "mk_point_cloud",
    "iters",
    "cooling",
]
