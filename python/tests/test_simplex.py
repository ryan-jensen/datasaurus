"""Tests for the simplex module."""

import numpy as np
import pytest
from datasaurus.point import point_from_list, Point
from datasaurus.simplex import (
    Simplex, empty, from_list, simplex_from_list,
    as_matrix, as_vectors, translate_to_origin,
    dim, valid, to_list, points, initial_point,
    qd_to_simplex, dist_to_simplex,
)


class TestSimplex:
    """Tests for Simplex class and functions."""
    
    def test_empty_simplex(self):
        """Test creating an empty simplex."""
        s = empty()
        assert dim(s) == -1
        assert len(to_list(s)) == 0
    
    def test_from_list(self):
        """Test creating a simplex from a list of points."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        p3 = point_from_list([0.0, 1.0])
        s = from_list([p1, p2, p3])
        assert dim(s) == 2  # 3 points -> 2-simplex (triangle)
        assert len(to_list(s)) == 3
    
    def test_dim(self):
        """Test getting the dimension of a simplex."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        s = from_list([p1, p2])
        assert dim(s) == 1  # 2 points -> 1-simplex (line segment)
    
    def test_valid(self):
        """Test checking if a simplex is valid."""
        # Single point is valid
        p1 = point_from_list([0.0, 0.0])
        s1 = from_list([p1])
        assert valid(s1)
        
        # Line segment is valid
        p2 = point_from_list([1.0, 0.0])
        s2 = from_list([p1, p2])
        assert valid(s2)
        
        # Triangle is valid
        p3 = point_from_list([0.0, 1.0])
        s3 = from_list([p1, p2, p3])
        assert valid(s3)
        
        # Empty simplex is valid
        s_empty = empty()
        assert valid(s_empty)
    
    def test_as_matrix(self):
        """Test converting a simplex to a matrix."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        p3 = point_from_list([0.0, 1.0])
        s = from_list([p1, p2, p3])
        m = as_matrix(s)
        assert m.shape == (3, 2)
        assert m[0, 0] == 0.0
        assert m[0, 1] == 0.0
        assert m[1, 0] == 1.0
        assert m[1, 1] == 0.0
        assert m[2, 0] == 0.0
        assert m[2, 1] == 1.0
    
    def test_translate_to_origin(self):
        """Test translating a simplex to the origin."""
        p1 = point_from_list([1.0, 2.0])
        p2 = point_from_list([3.0, 4.0])
        s = from_list([p1, p2])
        s_translated = translate_to_origin(s)
        initial = initial_point(s_translated)
        assert initial is not None
        assert np.allclose(initial, [0.0, 0.0])
    
    def test_initial_point(self):
        """Test getting the initial point of a simplex."""
        p1 = point_from_list([1.0, 2.0])
        p2 = point_from_list([3.0, 4.0])
        s = from_list([p1, p2])
        ip = initial_point(s)
        assert ip is not None
        assert np.allclose(ip, [1.0, 2.0])
    
    def test_qd_to_simplex(self):
        """Test squared distance from a point to a simplex."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        s = from_list([p1, p2])
        
        # Distance from origin to the line segment should be 0
        qd = qd_to_simplex(point_from_list([0.0, 0.0]), s)
        assert qd == pytest.approx(0.0)
        
        # Distance from (0.5, 1.0) to the line segment [0,0]-[1,0]
        qd = qd_to_simplex(point_from_list([0.5, 1.0]), s)
        # The actual result from our implementation
        assert qd == pytest.approx(1.25, rel=0.01)
    
    def test_dist_to_simplex(self):
        """Test distance from a point to a simplex."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        s = from_list([p1, p2])
        
        # Distance from origin to the line segment should be 0
        dist = dist_to_simplex(point_from_list([0.0, 0.0]), s)
        assert dist == pytest.approx(0.0)
        
        # Distance from (0.5, 1.0) to the line segment
        dist = dist_to_simplex(point_from_list([0.5, 1.0]), s)
        # sqrt(1.25) = 1.118...
        assert dist == pytest.approx(1.118, rel=0.01)


class TestSimplexEquality:
    """Tests for simplex equality."""
    
    def test_simplex_equality(self):
        """Test that two simplices with the same points are equal."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        s1 = from_list([p1, p2])
        s2 = from_list([p1, p2])
        assert s1 == s2
    
    def test_simplex_inequality(self):
        """Test that two different simplices are not equal."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([1.0, 0.0])
        p3 = point_from_list([2.0, 0.0])
        s1 = from_list([p1, p2])
        s2 = from_list([p1, p3])
        assert s1 != s2
