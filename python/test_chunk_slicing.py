import sys
sys.path.append('.')
import mod_python_interface as mpi
import numpy as np
import os

m = mpi.mod_python_interface

# 1. Setup paths
config_path = os.path.abspath("../input/config/config_sandesh.nml")
hep_path = os.path.abspath("../input/hep/europe/AUR.nc")

m.set_simulation_config_path(config_path.encode('utf-8'))
m.set_custom_hep_paths(hep_path.encode('utf-8'))

active = np.array([21], dtype=np.int32)
m.set_active_modules(active)

print("Initializing simulation...")
m.init_sim_step_1()
m.init_sim_step_2()
m.init_sim_step_3(False)
m.init_sim_step_4()

print("Verifying dynamic HEP slice loading...")
nx, ny, npops = m.get_grid_dims()
print(f"Dynamic grid dimensions: nx = {nx}, ny = {ny}, npops = {npops}")

# Trigger slice reading for t_hep = 1 (this should be in cache)
print("\n--- Querying t_hep = 1 (should be in cache) ---")
hep_1 = m.get_simulation_hep(1, nx, ny, 3)
print(f"HEP slice 1 loaded successfully. Shape: {hep_1.shape}")

# Trigger slice reading for t_hep = 300 (triggers chunk load/cache miss)
print("\n--- Querying t_hep = 300 (triggers chunk cache miss and loads next chunk) ---")
hep_300 = m.get_simulation_hep(300, nx, ny, 3)
print(f"HEP slice 300 loaded successfully. Shape: {hep_300.shape}")

# Trigger slice reading back to t_hep = 1 (triggers cache miss and loads previous chunk)
print("\n--- Querying t_hep = 1 again (triggers cache miss and loads first chunk back) ---")
hep_1_reload = m.get_simulation_hep(1, nx, ny, 3)
print(f"HEP slice 1 reloaded successfully. Shape: {hep_1_reload.shape}")

# Verify values are matching
difference = np.max(np.abs(hep_1 - hep_1_reload))
print(f"Reload verification: Max discrepancy = {difference}")
assert difference == 0.0, "Reloaded values do not match original!"
print("Assertion passed: Reloaded chunk values are 100% correct!")

m.cleanup_simulation()
print("\nAll chunked loading tests PASSED successfully!")
