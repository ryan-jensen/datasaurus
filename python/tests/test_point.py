"""Tests for the point module."""

import numpy as np
import pytest
from datasaurus.point import (
    Point, PointCloud, LPointCloud,
    quadrance, qd, norm, distance,
    point_from_list, point_from_pair, point_from_triple,
    get_point_comp, get_all_comps,
    mpc_from_point_cloud, num_points, dim_pc,
)


class TestPoint:
    """Tests for point operations."""
    
    def test_point_from_list(self):
        """Test creating a point from a list."""
        p = point_from_list([1.0, 2.0, 3.0])
        assert len(p) == 3
        assert p[0] == 1.0
        assert p[1] == 2.0
        assert p[2] == 3.0
    
    def test_point_from_pair(self):
        """Test creating a 2D point from a pair."""
        p = point_from_pair((1.0, 2.0))
        assert len(p) == 2
        assert p[0] == 1.0
        assert p[1] == 2.0
    
    def test_point_from_triple(self):
        """Test creating a 3D point from a triple."""
        p = point_from_triple((1.0, 2.0, 3.0))
        assert len(p) == 3
        assert p[0] == 1.0
        assert p[1] == 2.0
        assert p[2] == 3.0
    
    def test_quadrance(self):
        """Test quadrance (squared norm) calculation."""
        p = point_from_list([3.0, 4.0])
        assert quadrance(p) == pytest.approx(25.0)  # 3^2 + 4^2 = 25
    
    def test_norm(self):
        """Test norm calculation."""
        p = point_from_list([3.0, 4.0])
        assert norm(p) == pytest.approx(5.0)  # hypotenuse of 3-4-5 triangle
    
    def test_distance(self):
        """Test distance calculation."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([3.0, 4.0])
        assert distance(p1, p2) == pytest.approx(5.0)
    
    def test_qd(self):
        """Test squared distance calculation."""
        p1 = point_from_list([0.0, 0.0])
        p2 = point_from_list([3.0, 4.0])
        assert qd(p1, p2) == pytest.approx(25.0)
    
    def test_get_point_comp(self):
        """Test getting a component of a point."""
        p = point_from_list([1.0, 2.0, 3.0])
        assert get_point_comp(p, 0) == 1.0
        assert get_point_comp(p, 1) == 2.0
        assert get_point_comp(p, 2) == 3.0
    
    def test_get_all_comps(self):
        """Test getting all components of a point cloud."""
        p1 = point_from_list([1.0, 2.0])
        p2 = point_from_list([3.0, 4.0])
        pc = [p1, p2]
        comps = get_all_comps(pc)
        assert len(comps) == 2  # 2 dimensions
        assert comps[0] == pytest.approx([1.0, 3.0])  # x-components
        assert comps[1] == pytest.approx([2.0, 4.0])  # y-components


class TestPointCloud:
    """Tests for point cloud operations."""
    
    def test_mpc_from_point_cloud(self):
        """Test converting list of points to matrix."""
        p1 = point_from_list([1.0, 2.0])
        p2 = point_from_list([3.0, 4.0])
        pc = mpc_from_point_cloud([p1, p2])
        assert pc.shape == (2, 2)
        assert pc[0, 0] == 1.0
        assert pc[0, 1] == 2.0
        assert pc[1, 0] == 3.0
        assert pc[1, 1] == 4.0
    
    def test_num_points(self):
        """Test getting number of points."""
        p1 = point_from_list([1.0, 2.0])
        p2 = point_from_list([3.0, 4.0])
        p3 = point_from_list([5.0, 6.0])
        pc = mpc_from_point_cloud([p1, p2, p3])
        assert num_points(pc) == 3
    
    def test_dim_pc(self):
        """Test getting dimension of points."""
        p1 = point_from_list([1.0, 2.0, 3.0])
        p2 = point_from_list([4.0, 5.0, 6.0])
        pc = mpc_from_point_cloud([p1, p2])
        assert dim_pc(pc) == 3
