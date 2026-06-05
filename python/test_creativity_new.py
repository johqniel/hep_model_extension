import sys
import os
import time
import numpy as np

sys.path.append('python')
import mod_python_interface as mpi

m = mpi.mod_python_interface

def run_simulation(module_id):
    print(f"\n--- Testing Module ID: {module_id} ---")
    config_path = os.path.abspath("input/config/basic_config.nml")
    hep_path = os.path.abspath("input/hep/background.nc").encode('utf-8')
    
    m.set_simulation_config_path(config_path.encode('utf-8'))
    m.set_custom_hep_paths([hep_path], 1)
    
    # Active modules: 1 (Motion), module_id
    active = np.array([1, module_id], dtype=np.int32)
    m.set_active_modules(active)
    
    m.init_sim_step_1()
    m.init_sim_step_2()
    
    # Set spawn configuration
    ns = 4
    npops = 3
    x_ini = np.zeros((ns, npops), dtype=np.float64)
    y_ini = np.zeros((ns, npops), dtype=np.float64)
    spread = np.zeros((ns, npops), dtype=np.float64)
    counts = np.zeros((ns, npops), dtype=np.int32)
    
    # Spawn 500 agents for each population at (10.0, 10.0)
    x_ini[0, :] = 10.0
    y_ini[0, :] = 10.0
    spread[0, :] = 2.0
    counts[0, :] = 500
    
    m.set_spawn_configuration(x_ini, y_ini, spread, counts, ns, npops)
    
    m.init_sim_step_3(False)
    m.init_sim_step_4()
    
    print("Running 200 simulation steps...")
    t_start = time.time()
    for i in range(1, 201):
        print(f"  Step {i}...", end="", flush=True)
        m.step_simulation(i)
        print(" done", flush=True)
    t_end = time.time()
        
    count = m.get_agent_count()
    print(f"Agent count: {count}")
    print(f"Time taken for 200 steps: {t_end - t_start:.4f} seconds")
    
    if count > 0:
        x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead, cluster_rank, creativity = m.get_simulation_agents(count)
        print(f"Mean creativity: {np.mean(creativity):.6f}")
        print(f"Min creativity: {np.min(creativity):.6f}")
        print(f"Max creativity: {np.max(creativity):.6f}")
        
    m.cleanup_simulation()

if __name__ == "__main__":
    # Test Module 24 (Individual Creativity)
    run_simulation(24)
    # Test Module 26 (Creativity Simple)
    run_simulation(26)
    # Test Module 27 (Creativity Fast)
    run_simulation(27)
    print("\nAll tests completed successfully!")
