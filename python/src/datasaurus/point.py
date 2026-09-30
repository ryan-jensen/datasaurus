"""
Point and PointCloud classes for representing points and collections of points.

This module provides the basic data structures for working with points in
n-dimensional space and point clouds (matrices of points).
"""

from typing import List, Tuple, Union
import numpy as np
import numpy.typing as npt

# Type aliases
R = float
C = complex
I = int
Z = int

Point = npt.NDArray[np.float64]
"""A point in n-dimensional space, represented as a numpy array."""

LPointCloud = List[Point]
"""A point cloud represented as a list of Point arrays."""

PointCloud = npt.NDArray[np.float64]
"""A point cloud represented as a 2D numpy array (matrix)."""


def quadrance(v: Point) -> R:
    """Compute the squared norm (quadrance) of a vector.
    
    Args:
        v: A point/vector as a numpy array.
        
    Returns:
        The squared Euclidean norm of the vector.
    """
    return np.dot(v, v)


def qd(u: Point, v: Point) -> R:
    """Compute the squared distance between two points.
    
    Args:
        u: First point as a numpy array.
        v: Second point as a numpy array.
        
    Returns:
        The squared Euclidean distance between u and v.
    """
    return quadrance(u - v)


def norm(v: Point) -> R:
    """Compute the Euclidean norm of a vector.
    
    Args:
        v: A point/vector as a numpy array.
        
    Returns:
        The Euclidean norm of the vector.
    """
    return np.sqrt(quadrance(v))


def distance(u: Point, v: Point) -> R:
    """Compute the Euclidean distance between two points.
    
    Args:
        u: First point as a numpy array.
        v: Second point as a numpy array.
        
    Returns:
        The Euclidean distance between u and v.
    """
    return norm(u - v)


def point_from_list(coords: List[R]) -> Point:
    """Create a point from a list of coordinates.
    
    Args:
        coords: List of coordinates.
        
    Returns:
        A numpy array representing the point.
    """
    return np.array(coords, dtype=np.float64)


def point_from_pair(coords: Tuple[R, R]) -> Point:
    """Create a 2D point from a pair of coordinates.
    
    Args:
        coords: A tuple of (x, y) coordinates.
        
    Returns:
        A numpy array representing the 2D point.
    """
    return point_from_list([coords[0], coords[1]])


def point_from_triple(coords: Tuple[R, R, R]) -> Point:
    """Create a 3D point from a triple of coordinates.
    
    Args:
        coords: A tuple of (x, y, z) coordinates.
        
    Returns:
        A numpy array representing the 3D point.
    """
    return point_from_list([coords[0], coords[1], coords[2]])


def get_point_comp(p: Point, idx: int) -> R:
    """Get a component of a point.
    
    Args:
        p: A point as a numpy array.
        idx: The index of the component to retrieve.
        
    Returns:
        The component at the given index.
    """
    return p[idx]


def get_all_comps(pc: LPointCloud) -> List[List[R]]:
    """Get all components of all points in a point cloud.
    
    Args:
        pc: A list of points.
        
    Returns:
        A list of lists, where each inner list contains all values for
        a particular dimension across all points.
    """
    if not pc:
        return []
    d = len(pc[0])
    return [[p[i] for p in pc] for i in range(d)]


def mpc_from_point_cloud(pc: LPointCloud) -> PointCloud:
    """Convert a list of points to a point cloud matrix.
    
    Args:
        pc: A list of points (each as numpy arrays).
        
    Returns:
        A 2D numpy array where each row is a point.
    """
    return np.array(pc, dtype=np.float64)


def num_points(pc: PointCloud) -> int:
    """Get the number of points in a point cloud.
    
    Args:
        pc: A point cloud as a 2D numpy array.
        
    Returns:
        The number of points (rows).
    """
    return pc.shape[0]


def dim_pc(pc: PointCloud) -> int:
    """Get the dimension of points in a point cloud.
    
    Args:
        pc: A point cloud as a 2D numpy array.
        
    Returns:
        The dimension of each point (columns).
    """
    return pc.shape[1]


# Test data
if __name__ == "__main__":
    p0 = point_from_list([0, 1, 2, 3, 4])
    p1 = point_from_list([1, 2, 3, 4, 5])
    
    my_pc = mpc_from_point_cloud([p0, p1])
    
    print(f"Point p0: {p0}")
    print(f"Point p1: {p1}")
    print(f"Distance between p0 and p1: {distance(p0, p1)}")
    print(f"Point cloud shape: {my_pc.shape}")
    print(f"Number of points: {num_points(my_pc)}")
    print(f"Dimension: {dim_pc(my_pc)}")
