#!/usr/bin/env python3
"""
Main entry point for the datasaurus Python package.

This script demonstrates the datasaurus algorithm by:
1. Loading the datasaurus dataset
2. Computing and displaying its statistics
3. Optionally generating animations of point clouds transforming to match target shapes
"""

import argparse
import numpy as np
from datasaurus import (
    datasaurus, datasaurus_raw,
    means, variances, co_var_matrix,
    v_lines2, v_lines4, h_lines2, h_lines4,
    sqr, sqr4, wedge, wedge4, grid_shape, x_shape, s1,
    PointCloudState, mk_point_cloud,
    iters, total_qd, cooling,
)
from datasaurus.point import point_from_pair, mpc_from_point_cloud
from datasaurus.simplex import from_list, simplex_from_list
from datasaurus.simplicial_complex import mk_simplicial_complex, qd_to_sc


def print_statistics(pc, name="Point Cloud"):
    """Print statistics for a point cloud."""
    print(f"\n{name}:")
    print(f"  Shape: {pc.shape}")
    print(f"  Means: {means(pc)}")
    print(f"  Variances: {variances(pc)}")
    cov = co_var_matrix(pc)
    print(f"  Covariance matrix:\n{cov}")
    if pc.shape[1] >= 2:
        corr = cov[0, 1] / np.sqrt(cov[0, 0] * cov[1, 1])
        print(f"  Correlation (x,y): {corr:.6f}")


def run_demo():
    """Run a demonstration of the datasaurus algorithm."""
    print("=" * 60)
    print("DATASAURUS PYTHON PORT DEMONSTRATION")
    print("=" * 60)
    
    # Display the original datasaurus dataset statistics
    print_statistics(datasaurus, "Original Datasaurus Dataset")
    
    # Show some target shapes
    print("\n" + "=" * 60)
    print("TARGET SHAPES")
    print("=" * 60)
    
    shapes = [
        ("Vertical Lines (2)", v_lines2),
        ("Vertical Lines (4)", v_lines4),
        ("Horizontal Lines (2)", h_lines2),
        ("Horizontal Lines (4)", h_lines4),
        ("Square", sqr()),
        ("Square with Cross", sqr4()),
        ("Wedge", wedge()),
        ("Wedge (4 circles)", wedge4()),
        ("Grid", grid_shape()),
        ("Circle", s1()),
        ("X Shape", x_shape()),
    ]
    
    for name, shape in shapes:
        print(f"\n{name}: dim={shape.dim}, {len(shape.simplices)} simplices")
    
    # Demonstrate the algorithm with a simple example
    print("\n" + "=" * 60)
    print("ALGORITHM DEMONSTRATION")
    print("=" * 60)
    
    # Create a point cloud state with the datasaurus and a target shape
    target = v_lines2
    state = mk_point_cloud(target)
    
    print(f"\nInitial point cloud (datasaurus):")
    print_statistics(state.point_cloud, "Initial")
    
    print(f"\nTarget simplicial complex: {target}")
    
    # Run a few iterations to show the algorithm working
    print("\nRunning 100 iterations...")
    pcs_list = iters(state, max_iters=100)
    
    final_pc = pcs_list[-1]
    print(f"\nAfter 100 iterations:")
    print_statistics(final_pc, "After 100 iterations")
    
    # Show the total squared distance improvement
    initial_qd = total_qd(state.point_cloud, target)
    final_qd = total_qd(final_pc, target)
    print(f"\nTotal squared distance to target:")
    print(f"  Initial: {initial_qd:.4f}")
    print(f"  Final:   {final_qd:.4f}")
    print(f"  Improvement: {((initial_qd - final_qd) / initial_qd * 100):.2f}%")
    
    print("\n" + "=" * 60)
    print("DEMONSTRATION COMPLETE")
    print("=" * 60)


def export_csv(filename="all.csv"):
    """Export the datasaurus dataset to CSV, similar to the Haskell version."""
    import csv
    
    print(f"Exporting datasaurus dataset to {filename}...")
    
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f)
        # Write header
        writer.writerow(['x', 'y'])
        # Write data
        for point in datasaurus_raw:
            writer.writerow(point)
    
    print(f"Exported {len(datasaurus_raw)} points to {filename}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Datasaurus - Generate point clouds that match statistics"
    )
    parser.add_argument(
        '--demo',
        action='store_true',
        help='Run the demonstration'
    )
    parser.add_argument(
        '--export',
        type=str,
        default=None,
        metavar='FILENAME',
        help='Export datasaurus dataset to CSV file'
    )
    parser.add_argument(
        '--stats',
        action='store_true',
        help='Print statistics for the datasaurus dataset'
    )
    
    args = parser.parse_args()
    
    if args.stats:
        print_statistics(datasaurus, "Datasaurus Dataset")
    
    if args.export:
        export_csv(args.export)
    
    if args.demo:
        run_demo()
    
    # If no arguments, show help
    if not (args.demo or args.export or args.stats):
        parser.print_help()


if __name__ == "__main__":
    main()
