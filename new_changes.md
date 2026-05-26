# Project State Summary - May 5, 2026

## Objective
The primary goals addressed recently were the implementation of **Auto K-Means and DBSCAN**, resolving simulation stability issues, performing a general codebase cleanup, and standardizing the **Birth-Death simulation modules** for consistent scientific modeling.

## Completed Work

### 1. Dead Agent Archive & NetCDF Storage (April 21st)
* **Objective**: Implement a performance-conscious system for archiving deceased agents for historical analysis.
* **Work Done**:
  * **Fortran Backend**: Added `store_dead_agents` flag to `world_container` and logic to archive agent data before deletion.
  * **Asynchronous Pipeline**: Built an async Python writer using `multiprocessing.Queue` to persist data to NetCDF without blocking the simulation thread.
  * **UI Integration**: Added a checkbox in the "Full Simulation" tab and status indicators for the writer queue.
  * **Bug Fixes**: Resolved segfaults in the F2PY interface by refactoring agent extraction into a clean dataclass-based pipeline.

### 2. New Module Pattern & World Data Access (April 23rd)
* **Objective**: Standardize how new agent-logic modules are added and how they interact with global state.
* **Work Done**:
  * **Accumulator Reset**: Established a mechanism for `t_tick_accumulators` that automatically reset at the end of each simulation tick.
  * **Module Registration**: Defined a clear 3-step pattern for adding modules (Fortran ID -> Python Interface -> SpawnPointEditor registration) following the `reviewed_agent_motion` template.
  * **World Access**: Identified and documented accessible agent variables via the `world` object to facilitate custom agent-logic development.

### 3. DBSCAN, Convex Hull & Performance Metrics (April 28th)
* **Objective**: Expand clustering options, improve cluster integrity, and track simulation efficiency.
* **Work Done**:
  * **DBSCAN Implementation**: Added point-based and grid-based DBSCAN clustering in Fortran.
  * **UI Controls**: Added dynamic control over DBSCAN parameters (`eps`, `minPts`) directly from the Python GUI.
  * **Convex Hull Fill**: Integrated a convex hull post-processing step into the grid-based voting mechanism to eliminate holes and noise gaps within identified clusters.
  * **Performance Monitoring**: Implemented `avg_ms_per_tick` tracking. The average time per tick is now displayed in the window title and is available as a plottable variable in the time-series UI.

### 4. Unified Density Smoothing & Global Metrics (April 29th)
* **Objective**: Standardize density smoothing across all clustering modes and enable real-time plotting of global states.
* **Work Done**:
  * **Unified Smoothing**: Replaced redundant local smoothing in Auto K-Means with a global `human_density_smoothing_radius` parameter.
  * **Iterative Smoothing**: Added `human_density_smoothing_iterations` to allow for aggressive, multi-pass global smoothing.
  * **Global Metrics**: Exported `t_dynamic_state` and `t_tick_accumulators` to Python. Added variables like `k_fertility (sim)`, `phi_death_acc (sim)`, and various `death_* (sim)` debug counters to the UI plotting engine.
  * **Dual-Axis Fix**: Resolved a PyQtGraph closure bug where secondary axes caused the plot canvas to collapse.

### 5. Birth-Death Refactoring & Logic Consolidation (May 4th)
* **Objective**: Standardize parameter naming and consolidate biological logic for easier scientific review.
* **Work Done**:
  * **Parameter Schema Migration**: Replaced legacy `b1-b4` identifiers with standardized ecological parameters: **`r`** (growth rate), **`NC`** (carrying capacity), **`Kmin`**, and **`Kmax`**.
  * **Module Consolidation**: Migrated birth/death logic from the obsolete `mod_birth_death_new.f95` into the reviewed and consolidated `mod_reviewed_modules.f95`.
  * **Build System Sync**: Updated the dependency graph and Python interface to support the new parameter schema and modular structure.

### 6. Architectural Refinement & Critical Fixes (April - May)
* **K-Means Farthest-First Traversal**: Replaced the peak-value search in K-Means with a **Farthest First Traversal (K-means++)** subroutine to ensure geographically remote populations are isolated uniformly.
* **Cluster ID 1-Indexing**: Fixed a visualization bug where Cluster ID 0 was being masked as noise; the system now uses 1-based indexing for all valid clusters.
* **Rendering & Matrix Edge Geometry**: Resolved line rendering issues by translating cluster maps directly through NumPy boolean slicing filters and "burning" 1-pixel white boundaries into the RGBA output matrices.
* **3D Visualization**: Replaced unstable OpenGL 3D views with Matplotlib 3D surfaces in the `compare_smoothing.py` tool for robust cluster verification in headless environments.
* **F2PY Namespace Hotfix**: Fixed a critical bug where localized imports of `mod_python_interface` shadowed the extension module, causing `AttributeError` crashes in the update loop.

## TODO later
1. **[LATEX/DOCS]**: Add an entry explaining the Farthest-First Traversal algorithm.
2. **[PYTHON/DOCS]**: Document that visual rendering loops for 2D and 3D are fundamentally separate.
3. **[PYTHON/DOCS]**: Mark that the backend expects exactly 4-channel RGBA arrays for map updates.

### 7. Cluster Population Control & Multi-Population Graphing (May 13th)
* **Objective**: Resolve population convergence discrepancies between global and cluster-based modules and improve the Python graphing capabilities.
* **Work Done**:
  * **Divergence Analysis**: Identified that the cluster-based module (`new_birth`) was severely under-suppressing births compared to the global (`reviewed_birth`) module. This was caused by the cluster module relying on the per-cluster accumulator `n_alive_acc(jp)`, which structurally misses agents not assigned to any cluster (e.g. `c_idx = 0`), resulting in massive population under-estimation. 
  * **Debug Tooling**: Implemented a global-vs-cluster diagnostic summation script within `python_interface.f95` to track the exact magnitude of unassigned agents escaping the control logic.
  * **Dynamic UI Plotting via Population Filters**: 
    * Enhanced `mod_python_interface.get_dynamic_state_stats` and `get_cluster_info` to optionally accept and return stats based on a specific `jp` population index.
    * Added a dedicated `pop:` spinbox to the Python GUI (in `application.py`) that feeds into the graph configuration.
    * Implemented **"Dominant Population" (`pop = -1`) logic**, allowing graphs to dynamically query the backend tick-by-tick and automatically select and plot statistics (`NC`, `n_agents`, `k_fertility`) specifically for whichever population currently has the most agents within the targeted cluster or globally.

### 8. Performance Metrics HUD & Population-Specific Mating (May 26th)
* **Objective**: Add real-time wall-clock profiling to identify simulation bottlenecks, and introduce optional reproductive isolation between populations.
* **Work Done**:
  * **Fortran Instrumentation**: Wrapped each phase of `step_simulation` in `python_interface.f95` with `system_clock` calls to measure wall-clock time for permanent modules, active modules, compaction, grid/density, and clustering.
  * **API**: Exposed `set_performance_timing_enabled` and `get_performance_stats` to Python via `mod_python_interface`. Implemented as optional with **zero overhead** when disabled.
  * **GUI HUD**: Added a "Show Performance Metrics" checkbox in the "View Editor" tab and a glassmorphic tech-cyan overlay in the simulation window displaying per-phase timing breakdowns.
  * **Population-Specific Mating (`allow_across_populations`)**: Added a new config flag `allow_across_populations` (default `.true.`). When set to `.false.`, the `get_male_from_cell` function is called with `only_population=jp`, enforcing strict reproductive isolation so that females can only mate with males of the same population.
  * **Files Modified**: `python_interface.f95`, `mod_agent_world.f95`, `mod_reviewed_modules.f95`, `mod_birth_death_new.f95`, `mod_config.f95`, `mod_read_inputs.f95`, `basic_config.nml`, `config_sandesh.nml`, `simulation.py`, `application.py`.

### 9. Critical Bugfix: Hashmap Population Corruption (May 26th)
* **Objective**: Fix a bug causing phantom agents of incorrect populations (e.g. Pop 1 red dots) to appear in single-population simulations.
* **Root Cause**: The `remove()` subroutine in `mod_hashmap.f95` used **open-address linear probing**. When an entry was deleted, subsequent entries in the probe chain were rehashed to fill the hole. The rehash code correctly saved and restored the `key` and `value` (array index) fields, but **failed to save or restore the `population` field**. The moved entry inherited a stale `population` value from the vacated bucket.
* **Impact**: When `get_agent(id, world)` later looked up a rehashed agent via `get_index_and_pop`, it received an incorrect population index. This caused an **out-of-bounds array access** into `agents(index, stale_population)`, reading garbage memory that could appear as an agent with `population = 1`, rendered as a red dot in the visualization.
* **Fix Applied** (4 changes to `mod_hashmap.f95`):
  1. Added `temp_population` variable to the rehash loop.
  2. Clear `population = -1` when initially removing an entry's bucket.
  3. Save `temp_population` alongside `temp_key`/`temp_value` when reading entries to move.
  4. Write `temp_population` to the destination bucket and clear `population = -1` on the source bucket after moving.
* **File Modified**: `src/data_structures/mod_hashmap.f95`.
 
### 10. Efficient Cluster-Restricted Density Updates (May 26th)
* **Objective**: Speed up the main bottleneck of the simulation step (the O(N) grid sweeps for raw density, combined agent flows, box smoothing, and carrying capacity copies) by restricting updates to cells that belong to active clusters.
* **Work Done**:
  * **Boundary Orphan Reset**: Designed and implemented a `was_clustered(:,:)` boundary logical grid array inside the `Grid` type. On re-clustering ticks (`mod(t, update_interval) == 0`), any cell that transitioned OUT of a cluster is immediately zeroed out.
  * **O(1) Neighborhood box smoothing**: Restricted the smoothed density box-filter calculations to cluster cells, looking up neighbor cells directly (which naturally evaluate to `0.0` outside clusters).
  * **O(1) HEP data copies**: Restricted carrying capacity copies strictly to cells within active clusters.
  * **Conditional Startup Fallback**: Embedded conditional fallback inside `step_simulation` in `python_interface.f95` to automatically pivot to the legacy `update_density_and_hep_grid` on tick 0 or when no clusters exist, ensuring robust carrying capacity (`NC`) initialization and preventing agent die-offs.
  * **Files Modified**: `python_interface.f95`, `mod_technical_modules.f95`, `mod_grid_id.f95`, `mod_config.f95`, `mod_read_inputs.f95`, `basic_config.nml`, `config_sandesh.nml`.

### 11. Exponential Moving Average (EMA) for Performance HUD (May 26th)
* **Objective**: Upgrade the performance metrics HUD from a global cumulative average to a responsive rolling average that forgets the distant past and immediately reacts to mid-run toggle switches.
* **Work Done**:
  * **Fortran EMA Implementation**: Replaced the global timing summation in `step_simulation` with an **Exponential Moving Average (EMA)** using a smoothing factor $\alpha = 0.05$ (remembers a window of ~20 ticks).
  * **Direct Stats Query**: Updated `get_performance_stats` to return the rolling average values directly, removing the need to divide by the global tick count and eliminating any side-effects of resetting the accumulators to zero.
  * **Files Modified**: `src/interfaces/python_interface.f95`.

