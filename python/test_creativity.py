import sys
sys.path.append('.')
import mod_python_interface as mpi
import numpy as np
import os

m = mpi.mod_python_interface

# 1. Init Simulation
config_path = os.path.abspath("../input/config/main_fortran_config.nml")
m.set_simulation_config_path(config_path.encode('utf-8'))
m.set_custom_hep_paths(
    os.path.abspath("../input/hep/background.nc").encode('utf-8')
)

# Active modules: 1 (Motion), 24 (Creativity)
active = np.array([1, 24], dtype=np.int32)
m.set_active_modules(active)

m.init_sim_step_1()
m.init_sim_step_2()
m.init_sim_step_3(False)
m.init_sim_step_4()

# Run a few steps
print("Running simulation steps...")
for i in range(10):
    m.step_simulation(0)

# Check agents
count = m.get_agent_count()
print(f"Agent count: {count}")

if count > 0:
    x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead, cluster_rank, creativity = m.get_simulation_agents(count)
    print(f"Mean creativity: {np.mean(creativity):.4f}")
    print(f"Min creativity: {np.min(creativity):.4f}")
    print(f"Max creativity: {np.max(creativity):.4f}")

m.cleanup_simulation()
print("Success!")
