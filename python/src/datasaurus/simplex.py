"""
Simplex class for representing simplices in Euclidean space.

A simplex is a generalization of a triangle or tetrahedron to arbitrary dimensions.
In n-dimensional space, a k-simplex is the convex hull of (k+1) affinely independent points.
"""

from typing import List, Tuple, Optional
import numpy as np
import numpy.typing as npt
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from point import Point, point_from_list, distance, norm


class Simplex:
    """Represents a simplex in Euclidean space.
    
    Attributes:
        dim: The dimension of the simplex (number of vertices - 1).
        points: List of vertices of the simplex (as numpy arrays).
    """
    
    def __init__(self, points: List[Point]):
        """Create a simplex from a list of points.
        
        Args:
            points: List of vertices. The dimension is len(points) - 1.
        """
        self.dim = len(points) - 1
        # Ensure all points are numpy arrays
        self.points = [np.array(p, dtype=np.float64) for p in points]
    
    @classmethod
    def empty(cls) -> 'Simplex':
        """Create an empty simplex."""
        return cls([])
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Simplex):
            return False
        return self.dim == other.dim and np.allclose(self.points, other.points)
    
    def __repr__(self) -> str:
        return f"Simplex(dim={self.dim}, points={self.points})"
    
    def as_matrix(self) -> npt.NDArray[np.float64]:
        """Convert the simplex to a matrix where each row is a vertex.
        
        Returns:
            A 2D numpy array with shape (num_vertices, dimension).
        """
        return np.array(self.points, dtype=np.float64)
    
    def translate_to_origin(self) -> 'Simplex':
        """Translate the simplex so that its first vertex is at the origin.
        
        Returns:
            A new Simplex with the first vertex at the origin.
        """
        if not self.points:
            return Simplex.empty()
        offset = self.points[0]
        new_points = [p - offset for p in self.points]
        return Simplex(new_points)
    
    def as_vectors(self) -> 'Simplex':
        """Get the simplex as vectors from the first vertex.
        
        This is equivalent to translate_to_origin but returns a new Simplex
        with the first vertex removed (since it's now at the origin).
        
        Returns:
            A new Simplex representing the vectors from the first vertex.
        """
        translated = self.translate_to_origin()
        if translated.dim < 0:
            return Simplex.empty()
        return Simplex(translated.points[1:])
    
    def is_valid(self) -> bool:
        """Check if the simplex is valid (affinely independent vertices).
        
        A simplex is valid if its vertices are affinely independent.
        This is equivalent to checking that the rank of the matrix of
        vectors from the first vertex equals the dimension.
        
        Returns:
            True if the simplex is valid, False otherwise.
        """
        if self.dim < 0:
            return True  # Empty simplex is valid
        if self.dim == 0:
            return True  # Single point is valid
        vectors = self.as_vectors()
        if vectors.dim < 1:
            return True
        matrix = vectors.as_matrix()
        return np.linalg.matrix_rank(matrix) == self.dim
    
    def to_list(self) -> List[Point]:
        """Get the list of vertices.
        
        Returns:
            List of vertex points.
        """
        return self.points
    
    def initial_point(self) -> Optional[Point]:
        """Get the first vertex of the simplex.
        
        Returns:
            The first vertex, or None if the simplex is empty.
        """
        if self.points:
            return self.points[0]
        return None
    
    def nearest_point(self, p: Point) -> Tuple[Point, float]:
        """Find the nearest point in the simplex to a given point.
        
        This uses projection onto the affine subspace spanned by the simplex.
        
        Args:
            p: The query point.
            
        Returns:
            A tuple of (nearest_point, squared_distance).
        """
        if self.dim < 0:
            # Empty simplex - return infinity
            return (np.array([]), float('inf'))
        
        if self.dim == 0:
            # Single point
            q = self.points[0]
            return (p, distance(p, q) ** 2)
        
        # Translate to origin
        q = self.points[0]
        p_translated = np.array(p, dtype=np.float64) - q
        
        # Get vectors from first vertex
        vectors = self.as_vectors()
        if vectors.dim < 1:
            return (p, distance(p, q) ** 2)
        
        a_matrix = vectors.as_matrix()
        
        # Solve for coefficients: a_matrix @ alpha = p_translated
        try:
            alpha, residuals, rank, s = np.linalg.lstsq(a_matrix, p_translated, rcond=None)
        except np.linalg.LinAlgError:
            return (p, float('inf'))
        
        # Check if the projection is inside the simplex
        alpha_list = alpha.tolist() if isinstance(alpha, np.ndarray) else [alpha]
        
        if all(a >= 0 for a in alpha_list) and sum(alpha_list) <= 1:
            p_projected = a_matrix @ alpha + q
            return (p_projected, distance(p, p_projected) ** 2)
        else:
            return (p, float('inf'))
    
    def qd_to_simplex(self, p: Point) -> float:
        """Compute the squared distance from a point to the simplex.
        
        Args:
            p: The query point.
            
        Returns:
            The squared distance from p to the nearest point in the simplex.
        """
        _, qd = self.nearest_point(p)
        return qd
    
    def dist_to_simplex(self, p: Point) -> float:
        """Compute the distance from a point to the simplex.
        
        Args:
            p: The query point.
            
        Returns:
            The distance from p to the nearest point in the simplex.
        """
        return np.sqrt(self.qd_to_simplex(p))


# Factory functions (for compatibility with Haskell version)
def empty() -> Simplex:
    """Create an empty simplex."""
    return Simplex.empty()


def from_list(points: List[Point]) -> Simplex:
    """Create a simplex from a list of points.
    
    Args:
        points: List of vertices.
        
    Returns:
        A Simplex object.
    """
    return Simplex(points)


simplex_from_list = from_list


def as_matrix(s: Simplex) -> npt.NDArray[np.float64]:
    """Get the simplex as a matrix.
    
    Args:
        s: A Simplex object.
        
    Returns:
        A 2D numpy array.
    """
    return s.as_matrix()


def as_vectors(s: Simplex) -> Simplex:
    """Get the simplex as vectors from the first vertex.
    
    Args:
        s: A Simplex object.
        
    Returns:
        A new Simplex representing vectors from the first vertex.
    """
    return s.as_vectors()


def translate_to_origin(s: Simplex) -> Simplex:
    """Translate the simplex to the origin.
    
    Args:
        s: A Simplex object.
        
    Returns:
        A new Simplex with the first vertex at the origin.
    """
    return s.translate_to_origin()


def dim(s: Simplex) -> int:
    """Get the dimension of a simplex.
    
    Args:
        s: A Simplex object.
        
    Returns:
        The dimension (number of vertices - 1).
    """
    return s.dim


def valid(s: Simplex) -> bool:
    """Check if a simplex is valid.
    
    Args:
        s: A Simplex object.
        
    Returns:
        True if the simplex is valid.
    """
    return s.is_valid()


def to_list(s: Simplex) -> List[Point]:
    """Get the list of vertices from a simplex.
    
    Args:
        s: A Simplex object.
        
    Returns:
        List of vertex points.
    """
    return s.to_list()


def points(s: Simplex) -> List[Point]:
    """Get the list of vertices from a simplex.
    
    Args:
        s: A Simplex object.
        
    Returns:
        List of vertex points.
    """
    return s.to_list()


def initial_point(s: Simplex) -> Optional[Point]:
    """Get the first vertex of a simplex.
    
    Args:
        s: A Simplex object.
        
    Returns:
        The first vertex, or None if empty.
    """
    return s.initial_point()


def qd_to_simplex(p: Point, s: Simplex) -> float:
    """Compute the squared distance from a point to a simplex.
    
    Args:
        p: The query point.
        s: A Simplex object.
        
    Returns:
        The squared distance.
    """
    return s.qd_to_simplex(p)


def dist_to_simplex(p: Point, s: Simplex) -> float:
    """Compute the distance from a point to a simplex.
    
    Args:
        p: The query point.
        s: A Simplex object.
        
    Returns:
        The distance.
    """
    return s.dist_to_simplex(p)


# Test data
if __name__ == "__main__":
    orig = point_from_list([0, 0])
    p0 = point_from_list([8, 1])
    p1 = point_from_list([9, 5])
    p2 = point_from_list([7, 4])
    
    s0 = from_list([p0])
    s1 = from_list([p1, p2])
    s2 = from_list([p2, orig, p1])
    
    print(f"Simplex s0: dim={dim(s0)}, valid={valid(s0)}")
    print(f"Simplex s1: dim={dim(s1)}, valid={valid(s1)}")
    print(f"Simplex s2: dim={dim(s2)}, valid={valid(s2)}")
    
    test_point = point_from_list([5, 3])
    print(f"Distance from {test_point} to s1: {dist_to_simplex(test_point, s1)}")
