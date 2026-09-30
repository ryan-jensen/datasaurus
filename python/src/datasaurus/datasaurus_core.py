"""
Main datasaurus module for generating point clouds with matching statistics.

This module implements the core algorithm for generating point clouds
(the "datasaurus" dataset) that have identical statistical properties
but visually different distributions.
"""

from typing import List, Tuple, Optional, NamedTuple
import random
import math
import numpy as np
import numpy.typing as npt
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from point import Point, PointCloud, point_from_pair, mpc_from_point_cloud, num_points, dim_pc
from simplex import Simplex, from_list, simplex_from_list, qd_to_simplex, dim as simplex_dim
from simplicial_complex import SimplicialComplex, mk_simplicial_complex, qd_to_sc


# Type aliases
R = float
Bound = Tuple[R, R]
Bounds = List[Bound]


class PointCloudState(NamedTuple):
    """State for the point cloud generation algorithm.
    
    Attributes:
        point_cloud: The current point cloud as a matrix.
        generator: Random number generator state (seed).
        target_complex: The target simplicial complex.
        mean_bounds: Bounds for the mean of each dimension.
        stddev_bounds: Bounds for the standard deviation of each dimension.
        movement_bound: Bounds for how much a point can move.
        index: Current iteration index.
    """
    point_cloud: PointCloud
    generator: random.Random
    target_complex: SimplicialComplex
    mean_bounds: Bounds
    stddev_bounds: Bounds
    movement_bound: Bound
    index: int


# The famous datasaurus dataset points
datasaurus_raw: List[Tuple[R, R]] = [
    (55.3846, 97.1795), (51.5385, 96.0256), (46.1538, 94.4872), (42.8205, 91.4103),
    (40.7692, 88.3333), (38.7179, 84.8718), (35.641, 79.8718), (33.0769, 77.5641),
    (28.9744, 74.4872), (26.1538, 71.4103), (23.0769, 66.4103), (22.3077, 61.7949),
    (22.3077, 57.1795), (23.3333, 52.9487), (25.8974, 51.0256), (29.4872, 51.0256),
    (32.8205, 51.0256), (35.3846, 51.4103), (40.2564, 51.4103), (44.1026, 52.9487),
    (46.6667, 54.1026), (50, 55.2564), (53.0769, 55.641), (56.6667, 56.0256),
    (59.2308, 57.9487), (61.2821, 62.1795), (61.5385, 66.4103), (61.7949, 69.1026),
    (57.4359, 55.2564), (54.8718, 49.8718), (52.5641, 46.0256), (48.2051, 38.3333),
    (49.4872, 42.1795), (51.0256, 44.1026), (45.3846, 36.4103), (42.8205, 32.5641),
    (38.7179, 31.4103), (35.1282, 30.2564), (32.5641, 32.1795), (30, 36.7949),
    (33.5897, 41.4103), (36.6667, 45.641), (38.2051, 49.1026), (29.7436, 36.0256),
    (29.7436, 32.1795), (30, 29.1026), (32.0513, 26.7949), (35.8974, 25.2564),
    (41.0256, 25.2564), (44.1026, 25.641), (47.1795, 28.718), (49.4872, 31.4103),
    (51.5385, 34.8718), (53.5897, 37.5641), (55.1282, 40.641), (56.6667, 42.1795),
    (59.2308, 44.4872), (62.3077, 46.0256), (64.8718, 46.7949), (67.9487, 47.9487),
    (70.5128, 53.718), (71.5385, 60.641), (71.5385, 64.4872), (69.4872, 69.4872),
    (46.9231, 79.8718), (48.2051, 84.1026), (50, 85.2564), (53.0769, 85.2564),
    (55.3846, 86.0256), (56.6667, 86.0256), (56.1538, 82.9487), (53.8462, 80.641),
    (51.2821, 78.718), (50, 78.718), (47.9487, 77.5641), (29.7436, 59.8718),
    (29.7436, 62.1795), (31.2821, 62.5641), (57.9487, 99.4872), (61.7949, 99.1026),
    (64.8718, 97.5641), (68.4615, 94.1026), (70.7692, 91.0256), (72.0513, 86.4103),
    (73.8462, 83.3333), (75.1282, 79.1026), (76.6667, 75.2564), (77.6923, 71.4103),
    (79.7436, 66.7949), (81.7949, 60.2564), (83.3333, 55.2564), (85.1282, 51.4103),
    (86.4103, 47.5641), (87.9487, 46.0256), (89.4872, 42.5641), (93.3333, 39.8718),
    (95.3846, 36.7949), (98.2051, 33.718), (56.6667, 40.641), (59.2308, 38.3333),
    (60.7692, 33.718), (63.0769, 29.1026), (64.1026, 25.2564), (64.359, 24.1026),
    (74.359, 22.9487), (71.2821, 22.9487), (67.9487, 22.1795), (65.8974, 20.2564),
    (63.0769, 19.1026), (61.2821, 19.1026), (58.7179, 18.3333), (55.1282, 18.3333),
    (52.3077, 18.3333), (49.7436, 17.5641), (47.4359, 16.0256), (44.8718, 13.718),
    (48.7179, 14.8718), (51.2821, 14.8718), (54.1026, 14.8718), (56.1538, 14.1026),
    (52.0513, 12.5641), (48.7179, 11.0256), (47.1795, 9.8718), (46.1538, 6.0256),
    (50.5128, 9.4872), (53.8462, 10.2564), (57.4359, 10.2564), (60, 10.641),
    (64.1026, 10.641), (66.9231, 10.641), (71.2821, 10.641), (74.359, 10.641),
    (78.2051, 10.641), (67.9487, 8.718), (68.4615, 5.2564), (68.2051, 2.9487),
    (37.6923, 25.7692), (39.4872, 25.3846), (91.2821, 41.5385), (50, 95.7692),
    (47.9487, 95), (44.1026, 92.6923)
]


datasaurus: PointCloud = mpc_from_point_cloud(
    [point_from_pair(p) for p in datasaurus_raw]
)


# Test simplicial complexes
def v_line0() -> Simplex:
    return simplex_from_list([[30, 0], [30, 100]])


def v_line1() -> Simplex:
    return simplex_from_list([[70, 0], [70, 100]])


def v_line2() -> Simplex:
    return simplex_from_list([[50, 0], [50, 100]])


def v_line3() -> Simplex:
    return simplex_from_list([[90, 0], [90, 100]])


v_lines2: SimplicialComplex = mk_simplicial_complex(
    1, [v_line0(), v_line1()]
)

v_lines4: SimplicialComplex = mk_simplicial_complex(
    1, [v_line0(), v_line1(), v_line2(), v_line3()]
)


def h_line0() -> Simplex:
    return simplex_from_list([[0, 10], [100, 10]])


def h_line1() -> Simplex:
    return simplex_from_list([[0, 90], [100, 90]])


def h_line2() -> Simplex:
    return simplex_from_list([[0, 37], [100, 37]])


def h_line3() -> Simplex:
    return simplex_from_list([[0, 64], [100, 64]])


h_lines2: SimplicialComplex = mk_simplicial_complex(
    1, [h_line0(), h_line1()]
)

h_lines4: SimplicialComplex = mk_simplicial_complex(
    1, [h_line0(), h_line1(), h_line2(), h_line3()]
)


def circ(rx: R, ry: R, center: Tuple[R, R], npts: int) -> List[Simplex]:
    """Generate a circle as a list of line segments (1-simplices).
    
    Args:
        rx: Radius in x-direction.
        ry: Radius in y-direction.
        center: Center point (h, k).
        npts: Number of points on the circle.
        
    Returns:
        List of Simplex objects (line segments).
    """
    h, k = center
    stp = 2 * math.pi / npts
    points = [(h + rx * math.cos(t), k + ry * math.sin(t)) 
              for t in [0, stp, 2*stp, ..., 2*math.pi]]
    return from_pairs(points)


def from_pairs(points: List[Tuple[R, R]]) -> List[Simplex]:
    """Convert a list of 2D points to a list of line segment simplices.
    
    Args:
        points: List of (x, y) tuples.
        
    Returns:
        List of Simplex objects, each representing a line segment.
    """
    if len(points) < 2:
        return []
    segments = []
    for i in range(len(points) - 1):
        seg = simplex_from_list([list(points[i]), list(points[i+1])])
        segments.append(seg)
    return segments


def circ_prime(r: R, center: Tuple[R, R]) -> List[Simplex]:
    """Generate a circle with equal radius in both directions.
    
    Args:
        r: Radius.
        center: Center point (h, k).
        
    Returns:
        List of Simplex objects.
    """
    return circ(r, r, center, 40)


def circ_double_prime(rx: R, ry: R, center: Tuple[R, R], npts: int) -> List[Tuple[R, R]]:
    """Generate points on an ellipse.
    
    Args:
        rx: Radius in x-direction.
        ry: Radius in y-direction.
        center: Center point (h, k).
        npts: Number of points.
        
    Returns:
        List of (x, y) points on the ellipse.
    """
    h, k = center
    stp = 2 * math.pi / npts
    return [(h + rx * math.cos(t), k + ry * math.sin(t)) 
            for t in [i * stp for i in range(npts + 1)]]


# Helper functions for point cloud operations
def swap_row(pc: PointCloud, row_num: int, new_point: Point) -> PointCloud:
    """Replace a row in the point cloud with a new point.
    
    Args:
        pc: The point cloud matrix.
        row_num: The row index to replace (0-based).
        new_point: The new point to insert.
        
    Returns:
        A new point cloud with the row replaced.
    """
    new_pc = pc.copy()
    new_pc[row_num] = new_point
    return new_pc


def take_rows(pc: PointCloud, n: int) -> PointCloud:
    """Take the first n rows of a point cloud.
    
    Args:
        pc: The point cloud matrix.
        n: Number of rows to take.
        
    Returns:
        A new point cloud with the first n rows.
    """
    return pc[:n]


def drop_rows(pc: PointCloud, n: int) -> PointCloud:
    """Drop the first n rows of a point cloud.
    
    Args:
        pc: The point cloud matrix.
        n: Number of rows to drop.
        
    Returns:
        A new point cloud without the first n rows.
    """
    return pc[n:]


def as_row(point: Point) -> PointCloud:
    """Convert a point to a 1-row point cloud.
    
    Args:
        point: A point as a numpy array.
        
    Returns:
        A 2D array with one row.
    """
    return point.reshape(1, -1)


def total_qd(pc: PointCloud, sc: SimplicialComplex) -> R:
    """Compute the total squared distance from all points to the simplicial complex.
    
    Args:
        pc: The point cloud.
        sc: The simplicial complex.
        
    Returns:
        Sum of squared distances from each point to the complex.
    """
    return sum(qd_to_sc(pc[i], sc) for i in range(len(pc)))


def means(pc: PointCloud) -> Point:
    """Compute the mean of each dimension across all points.
    
    Args:
        pc: The point cloud.
        
    Returns:
        A point containing the mean of each dimension.
    """
    return np.mean(pc, axis=0)


def variances(pc: PointCloud) -> Point:
    """Compute the variance of each dimension across all points.
    
    Args:
        pc: The point cloud.
        
    Returns:
        A point containing the variance of each dimension.
    """
    return np.var(pc, axis=0, ddof=0)


def co_var_matrix(pc: PointCloud) -> npt.NDArray[np.float64]:
    """Compute the covariance matrix of the point cloud.
    
    Args:
        pc: The point cloud.
        
    Returns:
        The covariance matrix.
    """
    return np.cov(pc, rowvar=False)


def uniform_sample(seed: int, n: int, bounds: List[Bound]) -> PointCloud:
    """Generate random points uniformly sampled from bounds.
    
    Args:
        seed: Random seed.
        n: Number of points to generate.
        bounds: List of (min, max) tuples for each dimension.
        
    Returns:
        A point cloud with n points.
    """
    rng = random.Random(seed)
    d = len(bounds)
    points = []
    for _ in range(n):
        point = [rng.uniform(lo, hi) for lo, hi in bounds]
        points.append(point)
    return np.array(points, dtype=np.float64)


def cooling(x: R) -> R:
    """Cooling schedule for the simulated annealing algorithm.
    
    Args:
        x: The current iteration index.
        
    Returns:
        The cooling factor (probability of accepting worse solutions).
    """
    max_iters = 100000
    a_prime = 0.01
    b = 0.4
    a = (a_prime - b) / (max_iters ** 2)
    return a * (x ** 2) + b


# Simplicial complex shapes for target distributions
def sqr() -> SimplicialComplex:
    """Create a square simplicial complex."""
    s0 = simplex_from_list([[85, 18], [85, 78]])
    s1 = simplex_from_list([[25, 18], [25, 78]])
    s2 = simplex_from_list([[25, 18], [85, 18]])
    s3 = simplex_from_list([[25, 78], [85, 78]])
    return mk_simplicial_complex(1, [s0, s1, s2, s3])


def x_shape() -> SimplicialComplex:
    """Create an X-shaped simplicial complex."""
    d0 = simplex_from_list([[20, 0], [100, 100]])
    d1 = simplex_from_list([[20, 100], [100, 0]])
    return mk_simplicial_complex(1, [d0, d1])


def wedge() -> SimplicialComplex:
    """Create a wedge-shaped simplicial complex."""
    r = 22
    x_center = 54.26
    y_center = 47.83
    c0 = circ_prime(r, (x_center, y_center + r))
    c1 = circ_prime(r, (x_center, y_center - r))
    return mk_simplicial_complex(1, c0 + c1)


def wedge4() -> SimplicialComplex:
    """Create a more complex wedge shape."""
    r = 16
    x_center = 54.26
    y_center = 47.83
    c0 = circ_prime(r, (x_center - r, y_center - r))
    c1 = circ_prime(r, (x_center + r, y_center - r))
    c2 = circ_prime(r, (x_center - r, y_center + r))
    c3 = circ_prime(r, (x_center + r, y_center + r))
    return mk_simplicial_complex(1, c0 + c1 + c2 + c3)


def grid_shape() -> SimplicialComplex:
    """Create a grid-shaped simplicial complex."""
    ps = [simplex_from_list([[x, y]]) 
          for x in [20, 50, 80] 
          for y in [15, 50, 85]]
    return mk_simplicial_complex(0, ps)


def s1() -> SimplicialComplex:
    """Create a circle simplicial complex."""
    return mk_simplicial_complex(1, circ(32, 32, (54.26, 47.83), 40))


def sqr4() -> SimplicialComplex:
    """Create a square with cross simplicial complex."""
    s0 = simplex_from_list([[55, 18], [55, 78]])
    s1 = simplex_from_list([[25, 48], [85, 48]])
    base = sqr()
    _, ss = base.un_simplicial_complex()
    return mk_simplicial_complex(1, [s0, s1] + ss)


# Test data
test0: PointCloud = mpc_from_point_cloud([
    point_from_pair((1, 2)),
    point_from_pair((1.5, 1.5)),
    point_from_pair((3, 1.75)),
    point_from_pair((1, 0.5)),
])


# State management
def my_pcs() -> PointCloudState:
    """Create a test point cloud state."""
    return PointCloudState(
        point_cloud=test0,
        generator=random.Random(0),
        target_complex=mk_simplicial_complex(1, [v_line0(), v_line1()]),
        mean_bounds=[(-1, 2), (-1, 2)],
        stddev_bounds=[(-1, 2), (-1, 2)],
        movement_bound=(-0.1, 0.1),
        index=0,
    )


def datas_pcs() -> PointCloudState:
    """Create a point cloud state for the datasaurus dataset."""
    return PointCloudState(
        point_cloud=datasaurus,
        generator=random.Random(0),
        target_complex=h_lines2,
        mean_bounds=[(54.0, 55.0), (47.0, 48.0)],
        stddev_bounds=[(16.0, 17.0), (26.0, 27.0)],
        movement_bound=(-0.5, 0.5),
        index=0,
    )


def mk_point_cloud(tc: SimplicialComplex) -> PointCloudState:
    """Create a point cloud state with the datasaurus data and a target complex.
    
    Args:
        tc: The target simplicial complex.
        
    Returns:
        A PointCloudState object.
    """
    return PointCloudState(
        point_cloud=datasaurus,
        generator=random.Random(0),
        target_complex=tc,
        mean_bounds=[(54.0, 55.0), (47.0, 48.0)],
        stddev_bounds=[(16.0, 17.0), (26.0, 27.0)],
        movement_bound=(-0.5, 0.5),
        index=0,
    )


def mk_point_cloud_prime(pc: PointCloud, tc: SimplicialComplex, seed: int) -> PointCloudState:
    """Create a point cloud state with custom parameters.
    
    Args:
        pc: The initial point cloud.
        tc: The target simplicial complex.
        seed: Random seed.
        
    Returns:
        A PointCloudState object.
    """
    return PointCloudState(
        point_cloud=pc,
        generator=random.Random(seed),
        target_complex=tc,
        mean_bounds=[(54.0, 55.0), (47.0, 48.0)],
        stddev_bounds=[(16.0, 17.0), (26.0, 27.0)],
        movement_bound=(-0.5, 0.5),
        index=0,
    )


# Main iteration function
def new_point(state: PointCloudState) -> Tuple[int, Point]:
    """Generate a new point by perturbing an existing point.
    
    This implements one step of the simulated annealing algorithm.
    
    Args:
        state: The current state.
        
    Returns:
        A tuple of (row_number, new_point).
    """
    pc = state.point_cloud
    gen = state.generator
    target = state.target_complex
    mv_bnd = state.movement_bound
    ix = state.index
    
    # Select a random row to perturb
    row_num = gen.randint(1, num_points(pc))
    
    # Generate new random seed and movement
    new_seed = gen.randint(0, 2**32)
    temp = gen.random()
    
    # Generate random movement within bounds
    new_row_prime = uniform_sample(new_seed, 1, [mv_bnd] * dim_pc(pc))
    
    # Get old row and compute new row
    old_row = pc[row_num - 1]
    new_row = old_row + new_row_prime[0]
    
    # Check if we accept this new point
    if qd_to_sc(new_row, target) <= qd_to_sc(old_row, target) or temp < cooling(float(ix)):
        return (row_num, new_row)
    else:
        # Try again
        return new_point(state._replace(index=ix + 1))


def with_in_bounds(state: PointCloudState) -> bool:
    """Check if the point cloud is within the specified bounds.
    
    Args:
        state: The current state.
        
    Returns:
        True if all statistics are within bounds.
    """
    pc = state.point_cloud
    pc_means = means(pc)
    pc_sd = np.sqrt(variances(pc))
    mean_bnds = state.mean_bounds
    stddev_bnds = state.stddev_bounds
    
    tmp = list(zip(pc_means, mean_bnds))
    tmp2 = list(zip(pc_sd, stddev_bnds))
    
    return all(with_in_bound(t) for t in tmp) and all(with_in_bound(t) for t in tmp2)


def with_in_bound(pair: Tuple[R, Bound]) -> bool:
    """Check if a value is within its bounds.
    
    Args:
        pair: A tuple of (value, (lower, upper)).
        
    Returns:
        True if lower < value < upper.
    """
    r, (l, h) = pair
    return l < r and r < h


def next_iter(state: PointCloudState) -> PointCloud:
    """Perform one iteration of the point cloud generation algorithm.
    
    Args:
        state: The current state.
        
    Returns:
        The new point cloud.
    """
    (row_num, new_row) = new_point(state)
    current_pc = state.point_cloud
    new_pc = swap_row(current_pc, row_num - 1, new_row)
    new_state = state._replace(point_cloud=new_pc, index=state.index + 1)
    
    if with_in_bounds(new_state):
        return new_pc
    else:
        return next_iter(new_state)


def iters(state: PointCloudState, max_iters: int = 1000) -> List[PointCloud]:
    """Generate a sequence of point clouds through iterations.
    
    Args:
        state: The initial state.
        max_iters: Maximum number of iterations.
        
    Returns:
        List of point clouds generated during the iterations.
    """
    pcs = [state.point_cloud]
    current_state = state
    
    for _ in range(max_iters):
        new_pc = next_iter(current_state)
        pcs.append(new_pc)
        current_state = current_state._replace(
            point_cloud=new_pc,
            index=current_state.index + 1
        )
    
    return pcs


if __name__ == "__main__":
    print("Datasaurus module loaded successfully!")
    print(f"Datasaurus shape: {datasaurus.shape}")
    print(f"Datasaurus mean: {means(datasaurus)}")
    print(f"Datasaurus variance: {variances(datasaurus)}")
    print(f"Datasaurus covariance:\n{co_var_matrix(datasaurus)}")
