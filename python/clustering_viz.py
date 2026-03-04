# =============================================================================
# Module: clustering_viz.py  (renamed from clustering_hdbscan.py)
#
# Description:
#   Thin Python wrapper for the Fortran watershed clustering.
#   All computation is in Fortran (mod_watershed + mod_clustering).
#   This module only handles:
#     - Triggering the Fortran clustering call
#     - Querying results for visualization
#     - Pretty-printing summaries
#
# Usage:
#   from clustering_viz import ClusterViz
#
#   viz = ClusterViz(mod_python_interface)
#   viz.update(tick=t)          # triggers Fortran watershed
#   viz.summary()               # prints cluster info
#   map_2d = viz.get_map()      # 2D numpy array for plotting
# =============================================================================

import numpy as np


class ClusterViz:
    """
    Python-side orchestration and visualization for watershed clustering.

    All computation happens in Fortran.  This class provides:
      - update(): trigger re-clustering via the Fortran interface
      - get_map(): retrieve the 2D cell → cluster map for plotting
      - get_clusters(): retrieve per-cluster info (id, size, centroid)
      - summary(): print a human-readable summary

    All calibration parameters (smooth_radius, threshold, update_interval)
    are read from the world config on the Fortran side.

    Parameters
    ----------
    fortran_interface : module
        The imported mod_python_interface Fortran module.
    """

    def __init__(self, fortran_interface):

        self.fi = fortran_interface

        self._nx = 0
        self._ny = 0
        self._last_result = None

    def _ensure_dims(self):
        """Fetch grid dimensions from Fortran if not yet known."""
        if self._nx == 0:
            self._nx = int(self.fi.get_grid_nx())
            self._ny = int(self.fi.get_grid_ny())

    # -----------------------------------------------------------------
    # Core: trigger Fortran watershed clustering
    # -----------------------------------------------------------------
    def update(self, tick=0):
        """
        Trigger a re-clustering in Fortran.

        Calls run_watershed_clustering(tick) in the Fortran interface,
        which internally:
          1. Extracts human density surface from the grid cells
          2. Runs watershed_cluster (smooth → maxima → gradient ascent)
             using smooth_radius and threshold from the cluster store
          3. Updates cluster_store with persistent ID matching
          4. Counts agents per cluster

        Parameters
        ----------
        tick : int
            Current simulation tick.
        """
        self.fi.run_watershed_clustering(tick)

        self._ensure_dims()
        self._fetch_result()

    # -----------------------------------------------------------------
    # Query results
    # -----------------------------------------------------------------
    def _fetch_result(self):
        """Pull clustering results from Fortran."""
        self._ensure_dims()

        # Get cluster count
        info = np.zeros(3, dtype=np.int32)
        self.fi.get_cluster_count(info)
        n_clusters = int(info[0])

        # Get cell cluster map
        cell_map = np.zeros((self._nx, self._ny), dtype=np.int32)
        self.fi.get_cell_cluster_map(cell_map, self._nx, self._ny)

        # Get per-cluster info
        clusters = []
        for k in range(1, n_clusters + 1):
            cinfo = np.zeros(3, dtype=np.int32)
            ccoords = np.zeros(2, dtype=np.float64)
            self.fi.get_cluster_info(k, cinfo, ccoords)
            clusters.append({
                'id': int(cinfo[0]),
                'n_cells': int(cinfo[1]),
                'n_agents': int(cinfo[2]),
                'centroid': (float(ccoords[0]), float(ccoords[1])),
            })

        noise_cells = int(np.sum(cell_map == -1))

        self._last_result = {
            'n_clusters': n_clusters,
            'map': cell_map,
            'clusters': clusters
        }

    def get_map(self):
        """
        Return the 2D cell → cluster ID map as a numpy array.
        Useful for visualization (e.g. contour plot or colour map).
        """
        if self._last_result is None:
            self._ensure_dims()
            return np.full((self._nx, self._ny), -2, dtype=np.int32)
        return self._last_result['map']

    def get_clusters(self):
        """Return list of cluster dicts: id, n_cells, n_agents, centroid."""
        if self._last_result is None:
            return []
        return self._last_result['clusters']

    # -----------------------------------------------------------------
    # Output and summaries
    # -----------------------------------------------------------------
    def summary(self):
        """Print a summary of the latest clustering."""
        if self._last_result is None:
            print("No clustering results. Call update() first.")
            return

        r = self._last_result
        n_clusters = r['n_clusters']
        print(f"--- Watershed Clustering Summary ---")
        print(f"  Grid Size: {self._nx} x {self._ny}")
        print(f"  Total Clusters: {n_clusters}")
        for c in r['clusters']:
            print(f"  ID={c['id']:3d}  "
                  f"cells={c['n_cells']:4d}  "
                  f"agents={c['n_agents']:5d}  "
                  f"centroid=({c['centroid'][0]:.4f}, {c['centroid'][1]:.4f})")
        print(f"=====================================")
