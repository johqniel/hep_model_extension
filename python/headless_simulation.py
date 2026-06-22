import sys
import os
import numpy as np
import time
from PyQt5 import QtCore, QtWidgets
import queue
import netCDF4
import tempfile
from dataclasses import dataclass

from utils import show_selectable_error, MODULE_NAMES_MAP

# Try to import PIL for GIF generation
try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False

# mod_python_interface is imported inside run_simulation_process (subprocess context)
mod_python_interface = None
try:
    from utils import load_mpi
    mod_python_interface = load_mpi(exit_on_failure=False)
except Exception:
    pass








def resolve_series_value(series, mpi, npops, t, tick_elapsed_total,
                         x, y, pop, age, gender, resources, children,
                         is_pregnant, avg_resources, ux, uy, is_dead,
                         cluster_rank, creativity, count):
    """Standalone version of SimulationWindow._resolve_series_value for use in subprocesses.

    Mirrors the logic in simulation.py so that plot definitions defined in the
    main-app viewport can be sampled inside run_simulation_process.

    REDUNDANCY NOTE
    ---------------
    This function is a NECESSARY DUPLICATE of SimulationWindow._resolve_series_value
    in simulation.py. It exists because run_simulation_process runs in a detached
    subprocess (spawn context) with no Qt/GUI available, so SimulationWindow cannot
    be instantiated. Any change to metric calculations in simulation.py MUST be
    mirrored here. Future refactor: extract both into a shared plot_utils.py module.
    """
    import numpy as np

    def get_data(var_name):
        if var_name == 'age':          return age
        elif var_name == 'resources':  return resources
        elif var_name == 'children':   return children
        elif var_name == 'population': return pop
        elif var_name == 'is_pregnant': return is_pregnant
        elif var_name == 'avg_resources': return avg_resources
        elif var_name == 'gender':     return gender
        elif var_name == 'ux':         return ux
        elif var_name == 'uy':         return uy
        elif var_name == 'is_dead':    return is_dead
        elif var_name == 'cluster_rank': return cluster_rank
        elif var_name == 'creativity': return creativity
        return None

    source   = series.get('source', 'agents')
    var_name = series['variable']
    agg      = series.get('aggregation', 'mean')

    # --- Global source ---
    if source == 'global':
        if var_name == 'agent_count':
            return float(count)
        elif var_name == 'avg_ms_per_tick':
            return (tick_elapsed_total / t * 1000) if t > 0 else 0.0
        elif var_name in ['k_fertility', 'phi_death_acc', 'phi_birth_acc', 'n_alive_acc', 'avg_creativity']:
            pop_idx = int(series.get('population', 0))
            if pop_idx == -1:
                max_pop, max_n = 1, -1
                for p in range(1, npops + 1):
                    _, _, _, n, _ = mpi.get_dynamic_state_stats(0, p)
                    if n > max_n:
                        max_n = n; max_pop = p
                pop_idx = max_pop
            elif pop_idx == -2:
                total_w, weighted = 0.0, 0.0
                for p in range(1, npops + 1):
                    k_f, p_d, p_b, n_a, a_c = mpi.get_dynamic_state_stats(0, p)
                    w = float(n_a)
                    if w > 0:
                        v = {'k_fertility': float(k_f), 'phi_death_acc': float(p_d),
                             'phi_birth_acc': float(p_b), 'n_alive_acc': w,
                             'avg_creativity': float(a_c)}.get(var_name, 0.0)
                        weighted += w * v; total_w += w
                return (weighted / total_w) if total_w > 0 else 0.0
            k_fert, p_death, p_birth, n_alive, avg_creat = mpi.get_dynamic_state_stats(0, pop_idx)
            return {'k_fertility': float(k_fert), 'phi_death_acc': float(p_death),
                    'phi_birth_acc': float(p_birth), 'n_alive_acc': float(n_alive),
                    'avg_creativity': float(avg_creat)}.get(var_name, 0.0)
        elif var_name in ['death_natural','death_starvation','death_oob','death_conflict','death_random']:
            natural, starv, oob, confl, rnd, _, _, _ = mpi.get_debug_stats()
            return {'death_natural': float(natural), 'death_starvation': float(starv),
                    'death_oob': float(oob), 'death_conflict': float(confl),
                    'death_random': float(rnd)}.get(var_name, 0.0)
        elif var_name.startswith('perf_') and var_name != 'perf_active_modules':
            try:
                _, p_avg, a_avg, c_avg, g_dens, g_flows, g_smooth, g_hep, cl_avg, t_avg = mpi.get_performance_stats()
                perf_map = {
                    'perf_permanent': p_avg, 'perf_active': a_avg, 'perf_compaction': c_avg,
                    'perf_grid_density': g_dens, 'perf_grid_flows': g_flows,
                    'perf_grid_smoothing': g_smooth, 'perf_grid_hep': g_hep,
                    'perf_clustering': cl_avg, 'perf_total': t_avg,
                }
                return perf_map.get(var_name, 0.0) * 1000.0
            except Exception:
                return 0.0
        elif var_name == 'perf_active_modules':
            try:
                num_active = mpi.get_active_modules_count()
                if num_active > 0:
                    mod_ids, mod_avgs = mpi.get_active_modules_performance_stats(num_active)
                    return {MODULE_NAMES_MAP.get(int(m), f"Module {int(m)}"): float(a) * 1000.0
                            for m, a in zip(mod_ids, mod_avgs)}
            except Exception:

                pass
            return {}
        return 0.0

    # --- Clusters source ---
    if source == 'clusters':
        cluster_rank_idx = int(series.get('filter_val', 1))
        if cluster_rank_idx < 1:
            cluster_rank_idx = 1
        if var_name in ['n_agents', 'n_cells', 'MC_cl', 'MC_cl_AV', 'MC_cl_shared', 'hep_sum']:
            try:
                n_clusters = mpi.get_cluster_count()[0]
                if cluster_rank_idx <= n_clusters:
                    pop_idx = int(series.get('population', 0))
                    if pop_idx == -1:
                        max_pop, max_n = 1, -1
                        for p in range(1, npops + 1):
                            iinfo, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            if iinfo[2] > max_n:
                                max_n = iinfo[2]; max_pop = p
                        pop_idx = max_pop
                    elif pop_idx == -2:
                        total_w, weighted = 0.0, 0.0
                        for p in range(1, npops + 1):
                            iinfo_p, rinfo_p = mpi.get_cluster_info(cluster_rank_idx, p)
                            w = float(iinfo_p[2])
                            if w > 0:
                                v = {'n_agents': w, 'n_cells': float(iinfo_p[1]),
                                     'MC_cl': float(rinfo_p[3]), 'MC_cl_AV': float(rinfo_p[4]),
                                     'MC_cl_shared': float(rinfo_p[5]),
                                     'hep_sum': float(rinfo_p[2])}.get(var_name, 0.0)
                                weighted += w * v; total_w += w
                        return (weighted / total_w) if total_w > 0 else 0.0
                    iinfo, rinfo = mpi.get_cluster_info(cluster_rank_idx, pop_idx)
                    return {'n_agents': float(iinfo[2]), 'n_cells': float(iinfo[1]),
                            'MC_cl': float(rinfo[3]), 'MC_cl_AV': float(rinfo[4]),
                            'MC_cl_shared': float(rinfo[5]),
                            'hep_sum': float(rinfo[2])}.get(var_name, 0.0)
            except Exception:
                pass
            return 0.0
        if var_name in ['k_fertility', 'phi_death_acc', 'phi_birth_acc', 'n_alive_acc', 'avg_creativity']:
            pop_idx = int(series.get('population', 0))
            if pop_idx == -1:
                try:
                    n_clusters = mpi.get_cluster_count()[0]
                    if cluster_rank_idx <= n_clusters:
                        max_pop, max_n = 1, -1
                        for p in range(1, npops + 1):
                            iinfo_p, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            if iinfo_p[2] > max_n:
                                max_n = iinfo_p[2]; max_pop = p
                        pop_idx = max_pop
                except Exception:
                    pop_idx = 1
            elif pop_idx == -2:
                try:
                    n_clusters = mpi.get_cluster_count()[0]
                    if cluster_rank_idx <= n_clusters:
                        total_w, weighted = 0.0, 0.0
                        for p in range(1, npops + 1):
                            iinfo_p, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            w = float(iinfo_p[2])
                            if w > 0:
                                k_f, p_d, p_b, n_a, a_c = mpi.get_dynamic_state_stats(cluster_rank_idx, p)
                                v = {'k_fertility': float(k_f), 'phi_death_acc': float(p_d),
                                     'phi_birth_acc': float(p_b), 'n_alive_acc': w,
                                     'avg_creativity': float(a_c)}.get(var_name, 0.0)
                                weighted += w * v; total_w += w
                        return (weighted / total_w) if total_w > 0 else 0.0
                except Exception:
                    pass
                return 0.0
            k_fert, p_death, p_birth, n_alive, avg_creat = mpi.get_dynamic_state_stats(cluster_rank_idx, pop_idx)
            return {'k_fertility': float(k_fert), 'phi_death_acc': float(p_death),
                    'phi_birth_acc': float(p_birth), 'n_alive_acc': float(n_alive),
                    'avg_creativity': float(avg_creat)}.get(var_name, 0.0)
        return 0.0

    # --- Agents source ---
    series_mask = np.ones(count, dtype=bool)
    fvar = series.get('filter_var')
    fval = series.get('filter_val', 0)
    if fvar and fvar != 'None':
        fdata = get_data(fvar)
        if fdata is not None:
            series_mask = series_mask & (fdata == int(fval))
    data = get_data(var_name)
    fcount = int(np.sum(series_mask))
    if data is not None and fcount > 0:
        d = data[series_mask]
        if agg == 'mean':   return float(np.mean(d))
        elif agg == 'sum':  return float(np.sum(d))
        elif agg == 'min':  return float(np.min(d))
        elif agg == 'max':  return float(np.max(d))
    return 0.0


def run_simulation_process(run_idx, start_year, end_year, save_interval, config_path, hep_paths,
                            active_modules, spawn_points, age_distribution, clustering_alg,
                            kmeans_k, dbscan_eps, dbscan_minpts, current_npops,
                            store_dead_agents, gif_path, nc_path, dead_nc_path, progress_queue,
                            ipc_interval=10, dead_export_interval=500, dead_export_threshold=1000,
                            log_path=None,
                            export_timeseries=False, ts_csv_path=None, plot_config=None,
                            temporal_interbreeding=False, interbreed_start_year=0, interbreed_end_year=0):
    """
    Main entry point for headless execution of the simulation (running in a dedicated process).
    
    This function handles both the full initialization sequence and the main stepping loop of
    the compiled Fortran backend (mod_python_interface):
    
    1. Initialization Sequence:
       - Configures custom paths for config/HEP data.
       - Registers active simulation modules.
       - Sequentially invokes:
           mpi.init_sim_step_1() (world allocation)
           mpi.init_sim_step_2_part_1() (namelist parsing)
           mpi.init_sim_step_2_part_2_arrays_only() (grid arrays allocation)
           mpi.init_sim_step_2_part_2_chunk() (grid data population chunk-by-chunk)
           mpi.init_sim_step_2_part_3() (load environmental NetCDF variables)
       - Sets up clustering configurations.
       - Applies custom spawn configurations (if provided) via mpi.set_spawn_configuration(...).
       - Generates agent records using mpi.init_sim_step_3(False).
       - Sets initial demographic structures and executes mpi.init_sim_step_4() for final checks.
       
    2. Execution Stepping Loop:
       - Iterates from 1 to total_ticks.
       - In each iteration, runs the Fortran step method:
           mpi.step_simulation(t)
       - Periodically gathers state from Fortran and reports progress or exports results.
    """
    import sys
    import os
    import numpy as np
    import time
    import queue
    import netCDF4

    # Redirect stdout and stderr (both Python and Fortran write through fd 1/2)
    # to a log file as early as possible, so crash output is captured.
    _log_file = None
    if log_path:
        try:
            _log_file = open(log_path, 'w', buffering=1)  # line-buffered so partial lines survive a crash
            os.dup2(_log_file.fileno(), 1)  # redirect fd 1 (stdout)
            os.dup2(_log_file.fileno(), 2)  # redirect fd 2 (stderr)
            # Also redirect Python-level streams so print() matches
            sys.stdout = _log_file
            sys.stderr = _log_file
        except Exception:
            pass  # If redirect fails, continue without it

    # Set up SIGTERM handler for clean aborts
    class TerminateInterrupt(BaseException):
        pass

    def sigterm_handler(signum, frame):
        print("Child Process: SIGTERM received. Raising TerminateInterrupt.", flush=True)
        raise TerminateInterrupt("SIGTERM")

    import signal
    try:
        signal.signal(signal.SIGTERM, sigterm_handler)
    except Exception as se:
        print(f"Could not register SIGTERM handler: {se}")

    try:
        from PIL import Image
        pil_available = True
    except ImportError:
        pil_available = False

    try:
        sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
        import mod_python_interface
        if hasattr(mod_python_interface, 'mod_python_interface'):
            mpi = mod_python_interface.mod_python_interface
        else:
            mpi = mod_python_interface
            
        if not mpi:
            progress_queue.put((run_idx, "error", "mod_python_interface not available"))
            return
            
        # 1. Initialize simulation context
        mpi.set_simulation_config_path(config_path.encode('utf-8'))
        
        encoded_hep_paths = [p.encode('utf-8') for p in hep_paths]
        mpi.set_custom_hep_paths(encoded_hep_paths, len(hep_paths))
        
        if active_modules:
            mpi.set_active_modules(np.array(active_modules, dtype=np.int32), len(active_modules))
        else:
            mpi.set_active_modules(np.array([], dtype=np.int32), 0)
            
        # 2. Check for custom spawn points
        npops = current_npops
        if spawn_points:
            points_by_pop = {}
            for p in spawn_points:
                pop = p['pop']
                if pop not in points_by_pop:
                    points_by_pop[pop] = []
                points_by_pop[pop].append(p)
                
            max_sources = max(len(pts) for pts in points_by_pop.values()) if points_by_pop else 0
            ns = max_sources
            
            x_ini = np.zeros((ns, npops), dtype=np.float64)
            y_ini = np.zeros((ns, npops), dtype=np.float64)
            spread = np.zeros((ns, npops), dtype=np.float64)
            counts = np.zeros((ns, npops), dtype=np.int32)
            
            for pop, points in points_by_pop.items():
                pop_idx = pop - 1 
                if pop_idx < npops:
                    for i, p in enumerate(points):
                        x_ini[i, pop_idx] = p['x']
                        y_ini[i, pop_idx] = p['y']
                        spread[i, pop_idx] = p['spread']
                        counts[i, pop_idx] = p['count']
            
            mpi.init_sim_step_1()
            mpi.init_sim_step_2_part_1()
            mpi.init_sim_step_2_part_2_arrays_only()
            
            nx = mpi.get_grid_nx()
            for i in range(1, nx + 1, 10):
                end_sub = min(i + 9, nx)
                mpi.init_sim_step_2_part_2_chunk(i, end_sub)
                
            mpi.init_sim_step_2_part_3()
            
            if clustering_alg is not None:
                mpi.set_clustering_algorithm(clustering_alg)
                if clustering_alg == 2:
                    mpi.set_kmeans_clusters(kmeans_k)
                elif clustering_alg == 3:
                    mpi.set_dbscan_eps(dbscan_eps)
                    mpi.set_dbscan_minpts(dbscan_minpts)
            
            mpi.set_spawn_configuration(x_ini, y_ini, spread, counts, ns, npops)
        else:
            mpi.init_sim_step_1()
            mpi.init_sim_step_2_part_1()
            mpi.init_sim_step_2_part_2_arrays_only()
            
            nx = mpi.get_grid_nx()
            for i in range(1, nx + 1, 10):
                end_sub = min(i + 9, nx)
                mpi.init_sim_step_2_part_2_chunk(i, end_sub)
                
            mpi.init_sim_step_2_part_3()
            
            if clustering_alg is not None:
                mpi.set_clustering_algorithm(clustering_alg)
                if clustering_alg == 2:
                    mpi.set_kmeans_clusters(kmeans_k)
                elif clustering_alg == 3:
                    mpi.set_dbscan_eps(dbscan_eps)
                    mpi.set_dbscan_minpts(dbscan_minpts)
                    
        # Step 3: Generate Agents
        mpi.init_sim_step_3(False)
        
        # Step Age Distribution
        if age_distribution is not None:
            mpi.set_age_distribution_interface(age_distribution, len(age_distribution))
            mpi.init_sim_step_apply_age_dist()
            
        # Step 4: Verify
        mpi.init_sim_step_4()
        
        # Re-enable active time phases in test/headless simulation if set in config
        try:
            use_active = False
            with open(config_path, 'r') as f:
                for line in f:
                    if 'use_active_time_phases' in line.lower() and '.true.' in line.lower():
                        use_active = True
                        break
            if use_active and hasattr(mpi, 'set_use_active_time_phases'):
                mpi.set_use_active_time_phases(True)
                print("Test/Headless Simulation: Enabled active time phases.")
        except Exception as e:
            print(f"Warning: Failed to set use_active_time_phases in process: {e}")
        
        # Enable Fortran internal performance timing so we can query detailed metrics
        if hasattr(mpi, 'set_performance_timing_enabled'):
            mpi.set_performance_timing_enabled(True)
        
        # Step 5: Run loop
        duration_years = end_year - start_year
        total_ticks = int(duration_years * 100) # 100 ticks per year
        
        # Get grid dimensions
        dlon, dlat, npops_actual = mpi.get_grid_dims()
        lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep = mpi.get_simulation_config()
        
        # Initialize Writers
        packages_total = 0
        
        if save_interval > 0 and nc_path is not None:
            out_dir = os.path.dirname(os.path.abspath(nc_path))
            if out_dir and not os.path.exists(out_dir):
                os.makedirs(out_dir)
            # Create NetCDF file in this process
            ds = netCDF4.Dataset(nc_path, 'w', format='NETCDF4')
            ds.createDimension('time', None)
            ds.createDimension('lon', dlon)
            ds.createDimension('lat', dlat)
            ds.createDimension('pop', npops_actual)
            
            var_tick = ds.createVariable('tick', 'i4', ('time',))
            var_hep = ds.createVariable('hep', 'f4', ('time', 'lon', 'lat', 'pop'), zlib=True, complevel=4)
            var_density = ds.createVariable('human_density', 'f8', ('time', 'lon', 'lat'), zlib=True, complevel=4)
            time_idx = 0
            
        if store_dead_agents and dead_nc_path:
            # Change extension to .csv if it ends with .nc
            if dead_nc_path.endswith('.nc'):
                dead_csv_path = dead_nc_path[:-3] + '.csv'
            else:
                dead_csv_path = dead_nc_path
            
            out_dir = os.path.dirname(os.path.abspath(dead_csv_path))
            if out_dir and not os.path.exists(out_dir):
                os.makedirs(out_dir)
            mpi.set_forget_dead_agents(False)
            
            import csv
            ds_dead = open(dead_csv_path, 'w', newline='', buffering=1)
            csv_writer = csv.writer(ds_dead)
            csv_writer.writerow([
                'agent_id', 'pos_x', 'pos_y', 'population', 'age_ticks',
                'gender', 'death_tick', 'birth_tick', 'father_id', 'mother_id'
            ])
            dead_nc_path = dead_csv_path
            agent_idx = 0
        else:
            mpi.set_forget_dead_agents(True)
            
        _frame_queue = None  # set up below if PIL is available
        _all_frames = []     # all PIL frames accumulated by the background thread

        if pil_available:
            import threading
            import queue as _queue_mod

            _frame_queue = _queue_mod.Queue(maxsize=400)  # cap memory: ~200 max frames × 2 buffer

            # Ensure gif output directory exists before starting thread
            _gif_out_dir = os.path.dirname(os.path.abspath(gif_path))
            if _gif_out_dir and not os.path.exists(_gif_out_dir):
                os.makedirs(_gif_out_dir, exist_ok=True)

            def _save_gif_snapshot():
                """Write all accumulated frames to disk as a complete (or partial) GIF.
                Called periodically so that an abort still produces a valid GIF file."""
                if not _all_frames:
                    return
                try:
                    _all_frames[0].save(
                        gif_path,
                        save_all=True,
                        append_images=_all_frames[1:],
                        duration=50,
                        loop=0
                    )
                except Exception as e:
                    print(f"[GIF] Snapshot write error: {e}")

            def _frame_builder_worker():
                """Accumulate frames in RAM and write a periodic snapshot GIF to disk.
                Every FLUSH_EVERY frames the current set is saved — so an abort
                will always leave a valid (partial) GIF on disk."""
                FLUSH_EVERY = 50  # flush to disk every N new frames
                while True:
                    item = _frame_queue.get()
                    if item is None:  # sentinel — loop is done
                        _frame_queue.task_done()
                        break
                    try:
                        img = Image.fromarray(item)
                        _all_frames.append(img)
                        if len(_all_frames) % FLUSH_EVERY == 0:
                            _save_gif_snapshot()
                    except Exception as e:
                        print(f"[GIF] Frame error: {e}")
                    _frame_queue.task_done()

            _frame_thread = threading.Thread(target=_frame_builder_worker, daemon=True)
            _frame_thread.start()

        # Pre-compute the static water / land HEP mask once so we don't re-allocate each frame.
        # We refresh the mask at each capture because HEP values change over simulation time.
        def capture_frame_local():
            """Extract current grid state and push raw RGB array to background thread.

            Background: original HEP style (water=blue, land=yellow/green).
            Agent dots: colored by dominant population per cell.
            NEA=red/orange, AMH=teal, MIX=purple.
            """
            POP_COLORS = [
                (220,  80,  60),   # NEA  — red/orange
                ( 60, 180, 140),   # AMH  — teal green
                (140,  80, 200),   # MIX  — purple
            ]

            hep_data = mpi.get_simulation_hep(1, dlon, dlat, npops_actual)
            grid = hep_data[:, :, 0].T   # (h, w)
            h, w = grid.shape

            # Original HEP background
            rgb = np.empty((h, w, 3), dtype=np.uint8)
            water_mask = grid < -0.5
            rgb[water_mask]  = (0,   0,   255)
            rgb[~water_mask] = (253, 253, 150)
            rgb[grid > 0.5]  = (119, 221, 119)

            # Agent dots — dominant population per cell
            try:
                count = mpi.get_agent_count()
                if count > 0:
                    x, y, pop, age, gender, resources, children, is_pregnant, \
                        avg_resources, ux, uy, is_dead, cluster_rank, creativity = \
                        mpi.get_simulation_agents(count)

                    lon_0, lat_0, delta_lon, delta_lat, _, _ = mpi.get_simulation_config()

                    if delta_lon > 0 and delta_lat > 0:
                        gx = np.clip(((x - lon_0) / delta_lon).astype(int), 0, w - 1)
                        gy = np.clip(((y - lat_0) / delta_lat).astype(int), 0, h - 1)

                        alive = ~is_dead.astype(bool)
                        pop_counts = np.zeros((npops_actual, h, w), dtype=np.int32)
                        for p in range(npops_actual):
                            mask = (pop == (p + 1)) & alive
                            if np.any(mask):
                                np.add.at(pop_counts[p], (gy[mask], gx[mask]), 1)

                        total    = pop_counts.sum(axis=0)
                        occupied = total > 0
                        dominant = pop_counts.argmax(axis=0)

                        n_colors = min(npops_actual, len(POP_COLORS))
                        for p in range(n_colors):
                            cell_mask = occupied & (dominant == p)
                            if np.any(cell_mask):
                                rgb[cell_mask] = POP_COLORS[p]
            except Exception:
                pass  # graceful degradation — keep terrain-only frame

            rgb = np.flipud(rgb)

            if _frame_queue is not None:
                try:
                    _frame_queue.put_nowait(rgb)
                except Exception:
                    pass  # queue full — skip frame rather than blocking simulation


        start_time = time.time()
        interval_fortran = 0.0
        interval_grid = 0.0
        interval_dead = 0.0
        last_t = 0
        cumulative_grid_mb = 0.0
        cumulative_dead_written = 0
        cumulative_fortran_time = 0.0
        capture_interval = max(1, total_ticks // 200)  # constant for the whole run

        # --- Timeseries CSV setup ---
        ts_csv_file = None
        ts_csv_writer = None
        ts_plot_defs = []  # list of (title, series, plot_type)
        if export_timeseries and ts_csv_path and plot_config:
            import csv
            try:
                os.makedirs(os.path.dirname(os.path.abspath(ts_csv_path)), exist_ok=True)
                ts_csv_file = open(ts_csv_path, 'w', newline='', buffering=1)
                ts_csv_writer = csv.writer(ts_csv_file)
                # Build column list from timeseries plots only (bucket plots are not time-scalar)
                header = ['tick', 'year']
                for pdef in plot_config.get('plots', []):
                    ptype = pdef.get('type', 'timeseries')
                    if ptype in ('timeseries', 'count', 'dualaxis'):
                        sa = pdef.get('series_a', {})
                        title = pdef.get('title', 'plot')
                        if ptype == 'dualaxis':
                            sb = pdef.get('series_b', sa)
                            header.append(f"{title}__L__{sa.get('variable', '?')}")
                            header.append(f"{title}__R__{sb.get('variable', '?')}")
                            ts_plot_defs.append((title, sa, sb, ptype))
                        else:
                            header.append(f"{title}__{sa.get('variable', '?')}")
                            ts_plot_defs.append((title, sa, None, ptype))
                ts_csv_writer.writerow(header)
                print(f"[Timeseries CSV] Writing to: {ts_csv_path} ({len(ts_plot_defs)} plots)")
            except Exception as e:
                print(f"[Timeseries CSV] Could not open file: {e}")
                ts_csv_file = None
                ts_csv_writer = None

        # --- Pre-compute temporal interbreeding tick bounds ---
        ticks_per_year = 100
        interbreed_start_tick = int((interbreed_start_year - start_year) * ticks_per_year) if temporal_interbreeding else -1
        interbreed_end_tick   = int((interbreed_end_year   - start_year) * ticks_per_year) if temporal_interbreeding else -1


        for t in range(1, total_ticks + 1):
            t0 = time.time()

            # --- Temporal interbreeding toggle ---
            if temporal_interbreeding and hasattr(mpi, 'set_allow_across_populations'):
                in_window = (interbreed_start_tick <= t <= interbreed_end_tick)
                mpi.set_allow_across_populations(in_window)

            mpi.step_simulation(t)
            dt = time.time() - t0
            interval_fortran += dt
            cumulative_fortran_time += dt
            
            if t % ipc_interval == 0 or t == total_ticks:
                count = mpi.get_agent_count()
                elapsed = time.time() - start_time
                python_used = max(0.0, elapsed - cumulative_fortran_time)
                progress = int((t / total_ticks) * 100)
                
                # Get current VmRSS RAM in MB
                ram_mb = 0.0
                try:
                    with open("/proc/self/status", "r") as f:
                        for line in f:
                            if line.startswith("VmRSS:"):
                                ram_mb = int(line.split()[1]) / 1024.0
                                break
                except Exception:
                    pass
                
                ticks_diff = t - last_t
                if ticks_diff > 0:
                    fortran_ms = (interval_fortran / ticks_diff) * 1000.0
                    grid_ms = (interval_grid / ticks_diff) * 1000.0
                    dead_ms = (interval_dead / ticks_diff) * 1000.0
                else:
                    fortran_ms = 0.0
                    grid_ms = 0.0
                    dead_ms = 0.0
                
                # Retrieve Fortran internal performance stats
                p_ms, a_ms, c_ms = 0.0, 0.0, 0.0
                g_dens_ms, g_flows_ms, g_smooth_ms, g_hep_ms = 0.0, 0.0, 0.0, 0.0
                cl_ms, fortran_total_ms = 0.0, 0.0
                active_modules_ms = {}
                if hasattr(mpi, 'get_performance_stats'):
                    try:
                        _, p_avg, a_avg, c_avg, g_dens_avg, g_flows_avg, g_smooth_avg, g_hep_avg, cl_avg, t_avg = mpi.get_performance_stats()
                        p_ms = p_avg * 1000.0
                        a_ms = a_avg * 1000.0
                        c_ms = c_avg * 1000.0
                        g_dens_ms = g_dens_avg * 1000.0
                        g_flows_ms = g_flows_avg * 1000.0
                        g_smooth_ms = g_smooth_avg * 1000.0
                        g_hep_ms = g_hep_avg * 1000.0
                        cl_ms = cl_avg * 1000.0
                        fortran_total_ms = t_avg * 1000.0
                    except Exception:
                        pass
                
                # Fetch active module performance stats
                if hasattr(mpi, 'get_active_modules_count') and hasattr(mpi, 'get_active_modules_performance_stats'):
                    try:
                        num_active = mpi.get_active_modules_count()
                        if num_active > 0:
                            mod_ids, mod_avgs = mpi.get_active_modules_performance_stats(num_active)
                            for m_id, m_avg in zip(mod_ids, mod_avgs):
                                active_modules_ms[int(m_id)] = m_avg * 1000.0
                    except Exception:
                        pass

                # Reset accumulators
                interval_fortran = 0.0
                interval_grid = 0.0
                interval_dead = 0.0
                last_t = t
                
                progress_queue.put((
                    run_idx, "progress", progress, t, count, elapsed, ram_mb,
                    fortran_ms, grid_ms, dead_ms, cumulative_grid_mb, cumulative_dead_written, python_used,
                    p_ms, a_ms, c_ms, g_dens_ms, g_flows_ms, g_smooth_ms, g_hep_ms, cl_ms, fortran_total_ms,
                    active_modules_ms
                ))

                # --- Timeseries CSV sampling ---
                if ts_csv_writer and ts_plot_defs:
                    try:
                        current_year = start_year + t / 100.0
                        # Fetch agent arrays only if any plot needs them
                        need_agents = any(
                            sa.get('source', 'agents') == 'agents' or
                            (sb is not None and sb.get('source', 'agents') == 'agents')
                            for _, sa, sb, _ in ts_plot_defs
                        )
                        _x = _y = _pop = _age = _gender = _res = _children = np.zeros(0)
                        _is_preg = _avg_res = _ux = _uy = _is_dead = _crank = _creat = np.zeros(0)
                        if need_agents and count > 0:
                            try:
                                (_x, _y, _pop, _age, _gender, _res, _children,
                                 _is_preg, _avg_res, _ux, _uy, _is_dead,
                                 _crank, _creat) = mpi.get_simulation_agents(count)
                            except Exception:
                                pass
                        row = [t, f"{current_year:.2f}"]
                        for _title, _sa, _sb, _ptype in ts_plot_defs:
                            if _ptype == 'dualaxis':
                                vL = resolve_series_value(
                                    _sa, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                vR = resolve_series_value(
                                    _sb, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                # perf_active_modules returns a dict; flatten to sum
                                if isinstance(vL, dict): vL = sum(vL.values())
                                if isinstance(vR, dict): vR = sum(vR.values())
                                row.extend([vL, vR])
                            else:
                                v = resolve_series_value(
                                    _sa, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                if isinstance(v, dict): v = sum(v.values())
                                row.append(v)
                        ts_csv_writer.writerow(row)
                    except Exception as _csv_err:
                        print(f"[Timeseries CSV] Sampling error at tick {t}: {_csv_err}")
                
            # Log NC data
            t_g0 = time.time()
            if save_interval > 0 and t % save_interval == 0:
                if 'ds' in locals():
                    hep_data = mpi.get_simulation_hep(1, dlon, dlat, npops_actual)
                    if hasattr(mpi, 'get_grid_density'):
                        density_data = mpi.get_grid_density(dlon, dlat)
                    else:
                        density_data = np.zeros((dlon, dlat))
                    var_tick[time_idx] = t
                    var_hep[time_idx, :, :, :] = hep_data
                    var_density[time_idx, :, :] = density_data
                    time_idx += 1
                    packages_total += 1
                    cumulative_grid_mb += (hep_data.nbytes + density_data.nbytes) / (1024.0 * 1024.0)
                    progress_queue.put((run_idx, "package_saved", packages_total))
            interval_grid += time.time() - t_g0
                    
            t_d0 = time.time()
            if store_dead_agents and 'ds_dead' in locals():
                if t % dead_export_interval == 0 or t == total_ticks:
                    dead_count = mpi.get_dead_agents_count()
                    if dead_count >= dead_export_threshold or t == total_ticks:
                        if dead_count > 0:
                            dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_death_tick, dead_birth_tick, dead_father, dead_mother = \
                                mpi.get_dead_simulation_agents(dead_count)
                                
                            num_new = len(dead_id)
                            rows = []
                            for i in range(num_new):
                                rows.append([
                                    int(dead_id[i]),
                                    float(dead_x[i]),
                                    float(dead_y[i]),
                                    int(dead_pop[i]),
                                    int(dead_age[i]),
                                    int(dead_gender[i]),
                                    int(dead_death_tick[i]),
                                    int(dead_birth_tick[i]),
                                    int(dead_father[i]),
                                    int(dead_mother[i])
                                ])
                            csv_writer.writerows(rows)
                            
                            agent_idx += num_new
                            cumulative_dead_written += num_new
                            mpi.clear_dead_agents()
                            packages_total += 1
                            progress_queue.put((run_idx, "package_saved", packages_total))
            interval_dead += time.time() - t_d0

            if t % capture_interval == 0 and pil_available:
                capture_frame_local()  # pushes to background thread; non-blocking
                
        # Wait for background frame-builder to finish all pending frames
        if _frame_queue is not None:
            _frame_queue.put(None)  # sentinel
            _frame_thread.join(timeout=30)  # wait up to 30s for remaining frames
                
        # Close NetCDF files
        if 'ds' in locals():
            try:
                ds.sync()
            except Exception:
                pass
            try:
                ds.close()
            except Exception as ce:
                print(f"Note: Error closing main NetCDF dataset: {ce}")
            
        if store_dead_agents and 'ds_dead' in locals():
            # One last extraction
            dead_count = mpi.get_dead_agents_count()
            if dead_count > 0:
                dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_death_tick, dead_birth_tick, dead_father, dead_mother = \
                    mpi.get_dead_simulation_agents(dead_count)
                num_new = len(dead_id)
                rows = []
                for i in range(num_new):
                    rows.append([
                        int(dead_id[i]),
                        float(dead_x[i]),
                        float(dead_y[i]),
                        int(dead_pop[i]),
                        int(dead_age[i]),
                        int(dead_gender[i]),
                        int(dead_death_tick[i]),
                        int(dead_birth_tick[i]),
                        int(dead_father[i]),
                        int(dead_mother[i])
                    ])
                csv_writer.writerows(rows)
                agent_idx += num_new
                mpi.clear_dead_agents()
            try:
                ds_dead.flush()
            except Exception:
                pass
            try:
                ds_dead.close()
            except Exception as ce:
                print(f"Note: Error closing dead agents CSV: {ce}")
            mpi.set_forget_dead_agents(True)
            
        # Save GIF (final snapshot with all frames)
        msg = []
        if pil_available and _all_frames:
            _save_gif_snapshot()  # ensure all frames are written
            msg.append(f"Video saved to: {os.path.abspath(gif_path)}")
        else:
            msg.append("No video generated.")
            
        if save_interval > 0 and nc_path:
            msg.append(f"Grid data saved to: {os.path.abspath(nc_path)}")
        if store_dead_agents and dead_nc_path:
            msg.append(f"Dead agents saved to: {os.path.abspath(dead_nc_path)}")

        # Close timeseries CSV
        if ts_csv_file is not None:
            try:
                ts_csv_file.flush()
                ts_csv_file.close()
                msg.append(f"Timeseries CSV saved to: {os.path.abspath(ts_csv_path)}")
                print(f"[Timeseries CSV] Closed: {ts_csv_path}")
            except Exception as _csv_close_err:
                print(f"[Timeseries CSV] Close error: {_csv_close_err}")

        progress_queue.put((run_idx, "finished", "\n".join(msg)))
        
    except TerminateInterrupt:
        print(f"Child Process {run_idx}: clean shutdown requested via SIGTERM. Finalizing data writing...")
        
        # 1. Stop background frame thread
        if '_frame_queue' in locals() and _frame_queue is not None:
            _frame_queue.put(None)  # sentinel
        if '_frame_thread' in locals() and _frame_thread is not None:
            _frame_thread.join(timeout=5)
            
        # 2. Close NetCDF files
        if 'ds' in locals():
            try: ds.sync()
            except Exception: pass
            try: ds.close()
            except Exception: pass
            
        if store_dead_agents and 'ds_dead' in locals():
            try: ds_dead.flush()
            except Exception: pass
            try: ds_dead.close()
            except Exception: pass
            try: mpi.set_forget_dead_agents(True)
            except Exception: pass

        # Close timeseries CSV on abort too
        if 'ts_csv_file' in locals() and ts_csv_file is not None:
            try:
                ts_csv_file.flush()
                ts_csv_file.close()
            except Exception:
                pass

        # 3. Save GIF (final snapshot with all frames)
        msg = []
        if 'pil_available' in locals() and pil_available and '_all_frames' in locals() and _all_frames:
            try:
                _save_gif_snapshot()
                msg.append(f"Video saved to: {os.path.abspath(gif_path)}")
            except Exception as ge:
                print(f"Error saving GIF on abort: {ge}")
                msg.append("No video generated (error during saving).")
        else:
            msg.append("No video generated.")
            
        if save_interval > 0 and nc_path:
            msg.append(f"Grid data saved to: {os.path.abspath(nc_path)}")
        if store_dead_agents and dead_nc_path:
            msg.append(f"Dead agents saved to: {os.path.abspath(dead_nc_path)}")
            
        progress_queue.put((run_idx, "finished", "\n".join(msg)))

    except Exception as e:
        import traceback
        err_msg = f"{str(e)}\n{traceback.format_exc()}"
        progress_queue.put((run_idx, "error", err_msg))
