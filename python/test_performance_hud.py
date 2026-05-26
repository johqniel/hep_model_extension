import sys
import os
import numpy as np
from PyQt5 import QtWidgets, QtCore

# Add current directory to path to locate mod_python_interface
sys.path.append('.')
import mod_python_interface as mpi

m = mpi.mod_python_interface

# 1. Initialize Simulation Backend
config_path = os.path.abspath("input/config/basic_config.nml")
hep_path = os.path.abspath("input/hep/background.nc")

m.set_simulation_config_path(config_path.encode('utf-8'))
m.set_custom_hep_paths(hep_path.encode('utf-8'))

active = np.array([21, 22, 23, 24, 25], dtype=np.int32)
m.set_active_modules(active)

print("Initializing simulation backend...")
m.init_sim_step_1()
m.init_sim_step_2()
m.init_sim_step_3(False)
m.init_sim_step_4()
m.init_cluster_store()

# 2. Instantiate QApplication and SimulationWindow
print("Initializing PyQt Application...")
app = QtWidgets.QApplication(sys.argv)

from python.simulation import SimulationWindow
# Using skip_init=True since we already manually initialized simulation
print("Instantiating SimulationWindow...")
window = SimulationWindow(skip_init=True)

# Verify that perf_label was successfully initialized
assert hasattr(window, 'perf_label'), "perf_label missing from SimulationWindow!"
assert window.perf_label.isHidden(), "perf_label should be hidden by default!"
print("✔ HUD Widget self.perf_label successfully initialized and hidden by default.")

# 3. Test update_view_settings enabling show_perf
print("\n--- Testing update_view_settings(show_perf=True) ---")
window.update_view_settings({'show_perf': True})

# Test tick step and verify timing measurements
print("Running 5 simulation step ticks with performance timing enabled...")
for t in range(1, 6):
    m.step_simulation(t)

# Update visualization to trigger label calculation and rendering
window.update_visualization()

# Verify that perf_label is now visible and populated with average timing stats
assert not window.perf_label.isHidden(), "perf_label should be visible when show_perf is True!"
hud_text = window.perf_label.text()
print("HUD rendered text:")
print(hud_text)

assert "PERFORMANCE METRICS" in hud_text, "HUD missing PERFORMANCE METRICS header!"
assert "Permanent Modules" in hud_text, "HUD missing Permanent Modules metric!"
assert "Active Modules" in hud_text, "HUD missing Active Modules metric!"
assert "Compaction" in hud_text, "HUD missing Compaction metric!"
assert "Grid & Density" in hud_text, "HUD missing Grid & Density metric!"
assert "Clustering" in hud_text, "HUD missing Clustering metric!"
assert "Total Sim Step" in hud_text, "HUD missing Total Sim Step!"
assert "Ticks Timed      : 5" in hud_text, "HUD showing incorrect ticks timed count!"

# Fetch raw metrics directly from backend to check average calculations
perf_count, p_avg, a_avg, c_avg, g_avg, cl_avg, t_avg = m.get_performance_stats()
print(f"\nRaw performance stats fetched from Fortran:")
print(f"  Timed Ticks: {perf_count}")
print(f"  Permanent average: {p_avg*1000:.4f} ms")
print(f"  Active average: {a_avg*1000:.4f} ms")
print(f"  Compaction average: {c_avg*1000:.4f} ms")
print(f"  Grid/Density average: {g_avg*1000:.4f} ms")
print(f"  Clustering average: {cl_avg*1000:.4f} ms")
print(f"  Total average: {t_avg*1000:.4f} ms")

assert perf_count == 5, "Fortran timed ticks count should be 5!"
assert t_avg > 0.0, "Total simulation average time should be greater than zero!"
print("✔ Live performance statistics successfully retrieved and calculated by Python.")

# 4. Test update_view_settings disabling show_perf
print("\n--- Testing update_view_settings(show_perf=False) ---")
window.update_view_settings({'show_perf': False})

# Verify that perf_label is hidden and timing stops accumulating
window.update_visualization()
assert window.perf_label.isHidden(), "perf_label should be hidden when show_perf is False!"

# Step again and fetch stats to verify zero overhead/no accumulation when disabled
for t in range(6, 11):
    m.step_simulation(t)

perf_count_after, p_after, a_after, c_after, g_after, cl_after, t_after = m.get_performance_stats()
assert perf_count_after == 0, "Timing counters should have reset to 0 when performance timing was disabled!"
print("✔ HUD successfully hidden and backend counters reset correctly.")

# Clean up
m.cleanup_simulation()
print("\n✔ All performance timing integration checks passed flawlessly!")
sys.exit(0)
