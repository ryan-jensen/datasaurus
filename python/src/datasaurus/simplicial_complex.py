"""
SimplicialComplex class for representing collections of simplices.

A simplicial complex is a topological space that can be triangulated,
built from simplices (points, lines, triangles, tetrahedra, etc.)
glued together along their faces.
"""

from typing import List, Tuple, Iterator
import numpy as np
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from point import Point, point_from_list
from simplex import Simplex, from_list, qd_to_simplex, dim


class SimplicialComplex:
    """Represents a simplicial complex.
    
    A simplicial complex is defined by its maximal simplices.
    
    Attributes:
        dim: The dimension of the complex (dimension of maximal simplices).
        simplices: List of maximal simplices.
    """
    
    def __init__(self, simplices: List[Simplex]):
        """Create a simplicial complex from a list of simplices.
        
        Args:
            simplices: List of maximal simplices.
        """
        if not simplices:
            self.dim = -1
        else:
            self.dim = max(dim(s) for s in simplices)
        self.simplices = simplices
    
    @classmethod
    def mk(cls, dim: int, simplices: List[Simplex]) -> 'SimplicialComplex':
        """Create a simplicial complex with a specified dimension.
        
        Args:
            dim: The dimension of the complex.
            simplices: List of maximal simplices.
            
        Returns:
            A SimplicialComplex object.
        """
        sc = cls(simplices)
        sc.dim = dim
        return sc
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SimplicialComplex):
            return False
        return self.dim == other.dim and self.simplices == other.simplices
    
    def __repr__(self) -> str:
        return f"SimplicialComplex(dim={self.dim}, num_simplices={len(self.simplices)})"
    
    def is_valid(self) -> bool:
        """Check if the simplicial complex is valid.
        
        A simplicial complex is valid if all its simplices are valid.
        
        Returns:
            True if all simplices are valid.
        """
        if self.dim < 0:
            return True
        return all(s.is_valid() for s in self.simplices)
    
    def un_simplicial_complex(self) -> Tuple[int, List[Simplex]]:
        """Unpack the simplicial complex into its components.
        
        Returns:
            A tuple of (dimension, list of simplices).
        """
        return (self.dim, self.simplices)


def mk_simplicial_complex(dim: int, simplices: List[Simplex]) -> SimplicialComplex:
    """Create a simplicial complex.
    
    Args:
        dim: The dimension of the complex.
        simplices: List of maximal simplices.
        
    Returns:
        A SimplicialComplex object.
    """
    return SimplicialComplex.mk(dim, simplices)


def qd_to_sc(p: Point, sc: SimplicialComplex) -> float:
    """Compute the minimum squared distance from a point to any simplex in the complex.
    
    Args:
        p: The query point.
        sc: A SimplicialComplex object.
        
    Returns:
        The minimum squared distance from p to any simplex in sc.
    """
    if sc.dim < 0:
        return float('inf')
    return min(qd_to_simplex(p, s) for s in sc.simplices)


def sc_from_lists(lists: List[List[Point]]) -> SimplicialComplex:
    """Create a simplicial complex from a list of lists of points.
    
    Each inner list represents the vertices of a simplex.
    
    Args:
        lists: List of lists of points, where each inner list defines a simplex.
        
    Returns:
        A SimplicialComplex object.
    """
    simplices = [from_list(pts) for pts in lists]
    if not simplices:
        return SimplicialComplex([])
    n = len(simplices[0].points) - 1
    return SimplicialComplex.mk(n, simplices)


def sc_from_lists_prime(n: int, lists: List[List[Point]]) -> SimplicialComplex:
    """Create a simplicial complex with a specified dimension.
    
    Args:
        n: The dimension of the complex.
        lists: List of lists of points.
        
    Returns:
        A SimplicialComplex object.
    """
    simplices = [from_list(pts) for pts in lists]
    return SimplicialComplex.mk(n, simplices)


# Test data
if __name__ == "__main__":
    p0 = point_from_list([0, 0])
    p1 = point_from_list([10, 10])
    p2 = point_from_list([10, 1])
    
    s0 = from_list([p0, p1])
    s1 = from_list([p1, p2])
    
    my_sc = mk_simplicial_complex(1, [s0, s1])
    
    print(f"Simplicial complex: {my_sc}")
    print(f"Is valid: {my_sc.is_valid()}")
    
    test_point = point_from_list([5, 5])
    print(f"Distance from {test_point} to complex: {np.sqrt(qd_to_sc(test_point, my_sc))}")
