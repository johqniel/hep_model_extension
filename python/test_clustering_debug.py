"""Quick test to debug clustering - runs simulation for a few hundred ticks and prints density stats."""
import sys
import os
import numpy as np

# Match how application.py runs
os.chdir('/work/dnoguesk/hep_test')
sys.path.insert(0, 'python')

import mod_python_interface
mpi = mod_python_interface.mod_python_interface

config_path = "input/config/basic_config.nml"
hep_path = "input/hep/AccHEP_80_30ka_d18Oweighted_20yr.nc"

# Set config path
mpi.set_simulation_config_path(config_path)
hep_paths = [hep_path, hep_path, hep_path]
mpi.set_custom_hep_paths(hep_paths, len(hep_paths))

modules = np.array([22, 23, 14, 24, 25], dtype=np.int32)
mpi.set_active_modules(modules, len(modules))

print(" --- Step 1: Init World ---")
mpi.init_sim_step_1()
print(" --- Step 2.1: Setup World Config ---")
mpi.init_sim_step_2_part_1()
print(" --- Step 2.2: Allocate Grid ---")
mpi.init_sim_step_2_part_2_arrays_only()
nx = mpi.get_grid_nx()
print(f" --- Step 2.2b: Initialize Grid (nx={nx}) ---")
for i in range(1, nx + 1, 10):
    end_sub = min(i + 9, nx)
    mpi.init_sim_step_2_part_2_chunk(i, end_sub)
print(" --- Step 2.3: Load Data ---")
mpi.init_sim_step_2_part_3()
mpi.set_clustering_algorithm(1)
print(" --- Step 3: Generate Agents ---")
mpi.init_sim_step_3(False)
print(" --- Step 4: Verify ---")
mpi.init_sim_step_4()

n_ticks = 200
print(f"\n=== Running {n_ticks} ticks ===")
for t in range(n_ticks):
    mpi.step_simulation(t)
    if t % 100 == 0:
        count = mpi.get_agent_count()
        print(f"  Tick {t}: agents={count}")

print("\nDone.")
