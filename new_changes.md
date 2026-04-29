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

### 6. Rendering & Matrix Edge Geometry (April 9th)
* **Pyqtgraph Constraints**: Identified that hardware-accelerated OpenGL `IsocurveItem` vector layers in PyQtGraph severely struggle with parent Z-index mappings and scaling down-sampling over dense map arrays.
* **NumPy Mask Boundaries**: Resolved line rendering by fundamentally abandoning PyQtGraph's internal vector mapping algorithms. Instead, cluster maps were translated directly through NumPy boolean slicing filters (`np.pad` offsets).
* **Matrix Injection**: Successfully forced 1-pixel solid white boundaries explicitly onto the native 4-channel `[255, 255, 255, 255]` (`[1.0, 1.0, 1.0, 1.0]` for 3D faces) RGBA matrix outputs for absolute rendering certainty independent of background window traits.

### 7. K-Means Geospatial Spacing Bug (April 9th)
* **Algorithmic Defect**: The baseline `mod_kmeans.f95` clustering initializer (`select_top_n_indices`) exclusively relied on searching for absolute density peak values. This mechanically forced *all* K centroids to cluster redundantly inside a single massive mega-pack whenever distinct agent islands possessed variable sizes.
* **Farthest-First Traversal Resolution**: Removed `select_top_n_indices` logic, explicitly encoding a **Farthest First Traversal** (`kmeans_farthest_first_init`) K-Means++ subroutine mechanism directly inside Fortran. The new module mathematically anchors Centroid 1, then fiercely distances every subsequent centroid at exactly the highest possible squared-radius away from all current centroids, permanently curing cluster-loss and agent island abandonment definitively.

### 8. Cluster ID 0-Indexing Visualization Bug (April 19th)
* **Algorithmic Defect**: The persistent visualization of certain agent clusters was silently failing. This was traced to `mod_clustering.f95` mechanically assigning the initial cluster `ID = 0`. During UI rendering, Python filtered `valid_mask = cluster_map > 0` effectively erasing the entire 0th cluster from the mathematical plot rendering.
* **Resolution**: Realigned initialization parity. Explicitly set `next_cluster_id = 1` inside `store_cleanup` and the variable type default block inside `mod_clustering.f95` to establish permanent 1-based index assignment, allowing Native Python plotting logic to correctly mask out NOISE (`-1`) while perfectly displaying all mathematically active biological clusters natively.

### 9. Unified Density Smoothing & Dual Axis Plot Fixes (April 29th)
* **Algorithmic Consistency**: Unified the density smoothing mechanism across all clustering algorithms. `auto_k_means_agents` no longer performs its own redundant 4-pass local smoothing, but directly utilizes the global `human_density_smoothed` surface generated centrally by `update_density_and_hep_grid`.
* **Parameter Renaming & Expansion**: 
  - Renamed `watershed_smooth_radius` configuration parameter to `human_density_smoothing_radius` across all Namelists (`.nml`), interface definitions, and core modules to accurately reflect its global use.
  - Added a new configuration variable `human_density_smoothing_iterations` (default: 1) to allow for aggressive, iterative global smoothing passes identical to the previous Auto-K-Means behavior.
* **Obsolete Parameter Purge**: Completely eradicated `kmeans_auto_radius` and its associated UI spinbox (`spin_auto_k_radius`), Python-Fortran bridge setters, and cluster store fields to eliminate overlapping constraints.
* **Dual-Axis Plotting Fix**: 
  - Resolved a severe PyQtGraph closure and signal-binding bug where the secondary `ViewBox` (`p2`) in Dual-Axis plots mechanically overwrote the geometry of the primary PlotItem. This caused the plot canvas to collapse to a `0x0` rendering size, projecting a permanent black void.
  - Explicitly hardcoded the primary curve to **Red** (left Y-axis) and secondary curve to **Blue** (right Y-axis) for standardized analytical contrast.

### 10. Global Metrics Plotting Integration (April 29th)
* **Objective**: Enable real-time visualization of simulation-level dynamic states, accumulators, and debug counters inside the Python Plotting Engine (Time Series & Dual Axis).
* **Implementation**:
  - Exported global data (`t_dynamic_state` and `t_tick_accumulators`) from the `world_container` directly to Python by creating a new `get_dynamic_state_stats` subroutine inside the `f2py` bindings (`python_interface.f95`).
  - Populated the Python UI dropdown lists (`application.py`) with the newly exposed strings: `k_fertility (sim)`, `phi_death_acc (sim)`, `phi_birth_acc (sim)`, `n_alive_acc (sim)`, and various `death_* (sim)` debug counters.
  - Dynamically wired the `_resolve_var_value` data resolver in `simulation.py` to seamlessly query the new Fortran endpoints whenever these specific `(sim)` tags are requested.
  - **F2PY Namespace Hotfix**: Fixed a critical bug where localized `import mod_python_interface` inside nested functions shadowed the initialized F2PY extension module, causing `AttributeError` exceptions that silently crashed the entire 2D map update loop.

### 📝 Guide: How to Add New Accumulators to the UI
If you mathematically expand `t_tick_accumulators` in Fortran (`mod_counter.f95`), it will **not** magically appear in Python. You must complete a 3-step pipeline to expose it:
1. **Update Fortran Bindings** (`python_interface.f95`):
   Append your new variable to the `get_dynamic_state_stats` arguments, declare it as `intent(out)`, and point it to `world%accumulators_history(1)%your_new_variable`.
2. **Register the UI String** (`application.py`):
   Add the exact string (e.g., `"your_new_variable (sim)"`) to the `self._var_items` list around line 350 so it visibly populates the UI plotting dropdowns.
3. **Wire the Data Resolver** (`simulation.py`):
   Inside `_resolve_var_value`, update the tuple unpacking of `mod_python_interface.get_dynamic_state_stats()` to capture your new variable. Then add a routing rule: `if var_name == 'your_new_variable': return float(...)`.

# TODO later

## Codebase Documentation Responsibilities
Please ensure the following structural learnings are rigidly added to the code documentation logs or `latex` files so they are not lost:
1. **[LATEX/DOCS]**: Add an entry explaining that core K-Means clustering now securely utilizes a **Farthest-First Traversal (K-means++) algorithm** strictly to isolate geographically remote populations uniformly.
2. **[PYTHON/DOCS]**: Append notes declaring that UI borders and surface topology vectors are physically burned into the RGB/RGBA visualization image matrices inside Python (`simulation.py`) natively avoiding Z-index clipping via PyQtgraph tools. 
3. **[PYTHON/DOCS]**: Mark that the backend simulation pipeline expects arrays exactly formatted at 4-Channels `(R, G, B, Alpha)`. Altering structural boundaries above `idx = 3` will systematically crash rendering loops blindly.
4. **[PYTHON/DOCS]**: Distinctly outline that **2D and 3D visual rendering are structurally and fundamentally separated loops**. The `on_sim_progress` method possesses completely bifurcated arrays and independent mathematical paths for drawing flat grids (`ImageItem`) vs interpolating spherical terrains natively (`GLMeshItem`).
5. **[PYTHON/DOCS]**: Document that **local imports of F2PY modules** (e.g., `import mod_python_interface` inside a function) will pull the raw, unshadowed F2PY object hierarchy. Always use the globally initialized module object (where `mod_python_interface.mod_python_interface` has been correctly mapped) to avoid `AttributeError` crashes.
