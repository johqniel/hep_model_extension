# Project State Summary - April 6, 2026

## Objective
The primary goals addressed recently were the implementation of **Auto K-Means**, resolving simulation restart stability issues, and performing a general codebase cleanup and UI refinement.

## Completed Work

### 1. Code Cleanup & UI Documentation
* **Redundant File Removal**: Deleted `python/clustering_viz.py` to streamline the repository and avoid confusion with integrated clustering visualizations.
* **UI Documentation**: Added comprehensive class and module-level comments to `python/application.py` to better describe the tabbed structure (Configuration, Spawn Editor, View Editor) and the underlying `f2py` interface.
* **Enhanced K-Means Controls**: Increased the `smooth` radius range for Auto K-Means from `[1, 10]` to `[1, 100]` in `application.py`, allowing for much more aggressive smoothing on highly complex density maps.

### 2. Auto K-Means Implementation (April 1st)
* **Objective**: Enhance clustering with an automatic mode that determines the number of clusters (k) using local density maxima.
* **Work Done**:
  * **Exposed Local Maxima**: Made `find_local_maxima` in `mod_watershed.f95` public for use by other modules.
  * **Updated K-Means Backend**: Added an `auto_k` flag to `kmeans_grid_cluster` in `mod_kmeans.f95`. If set, it uses `find_local_maxima` to detect clusters.
  * **Safety Measures**: Capped the automatic cluster count to **20** to prevent UI freezes/performance bottlenecks on noisy maps.
  * **Aggressive Smoothing**: Implemented a local box filter pass within the auto-k detection logic to filter out spurious noise before counting peaks.
  * **UI Integration**: 
    - Added "Auto K-Means" to the clustering algorithm selection in `application.py`.
    - Added a `k` spinbox for standard K-Means.
    - Added a `smooth` spinbox for Auto K-Means, allowing users to tune the detection sensitivity directly from the UI.
  * **Fortran-Python Bridge**: Added `set_kmeans_clusters` and `set_kmeans_auto_radius` to `python_interface.f95` to allow real-time parameter updates.

### 3. Simulation Stability & Research
* **Restart Bug Fix**: Resolved a **Segmentation Fault** on simulation restart by adding `_ready` guards in `simulation.py` and blocking signals during UI initialization.
* **`sigma_u` Usage**: Confirmed that `sigma_u` is read from the Fortran namelist and governs initial velocity spread, distinct from the Python-controlled position spread (`ini_spread`).

### 4. 3D Visualization & Interactive Tools (April 6th)
* **Objective**: Replace unstable OpenGL 3D views with robust, interactive Matplotlib 3D surfaces for cluster verification.
* **New Tool**: `testing/compare_smoothing.py`
  * **Interactive Console**: Prompts for smoothing radius and iteration counts.
  * **4-Way Comparison**: Renders Raw Density, Standard Filter, Local Agent Filter, and Iterative Standard Filter side-by-side.
  * **Interactive Navigation**: 
    - **Synchronized Zoom**: Mouse wheel scaling mapped across all four 3D subplots.
    - **Peak Visibility Toggle**: Integrated Matplotlib `CheckButton` to show/hide detected cluster peaks in real-time.
  * **Software Rasterization**: Fully compatible with VM/headless environments (no OpenGL drivers required).

### 5. Architectural Refinement & Critical Fixes
* **Density NaN Bug Fix**: Identified a race condition where grid area calculations occurred before geographic data was loaded. Reordered the primary simulation bootstrap sequence (`mod_agent_world.f95`) to ensure `lat/lon` metadata is populated before cell initialization.
* **Modular Filter Migration**: Moved `smooth_box_filter` from `mod_technical_modules` to `mod_watershed` to eliminate circular dependency loops (`mod_kmeans` -> `mod_watershed`).
* **Smoothing Algorithm Parity**:
  - **Auto K-Means**: Internal seed detection now uses a rigorous **4x iterative smoothing loop** (Radius 4) to ensure stable cluster formation.
  - **Watershed**: Updated the core `watershed_cluster` logic to also perform **4x iterative smoothing** internally, ensuring seed consistency across all clustering modes.
* **Python Interface Expansion**: Exposed `apply_smooth_box_filter`, `apply_local_box_filter`, and `apply_find_local_maxima` via F2PY to the `mod_python_interface`.

## Open Files and Workspaces
* Visualization: `testing/compare_smoothing.py`
* Fortran clustering logic: `mod_watershed.f95`, `mod_kmeans.f95`
* Simulation Core: `mod_agent_world.f95`, `python_interface.f95`
