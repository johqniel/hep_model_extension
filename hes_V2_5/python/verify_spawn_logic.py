import sys
import os
import numpy as np

# Add parent directory to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    sys.exit(1)

def verify_spawn_logic():
    print("--- Verifying Spawn Logic ---")
    
    # We need a valid config file. Let's find one.
    config_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'config'))
    config_files = [f for f in os.listdir(config_dir) if f.endswith('.nml')]
    if not config_files:
        print("No config files found.")
    # 1. Set Config Path
    config_path = os.path.abspath("input/config/basic_config.nml")
    print(f"Using config: {config_path}")
    mod_python_interface.set_simulation_config_path(config_path)
    
    # 1.5 Set HEP Paths
    hep_path = os.path.abspath("input/hep/gradient/AUR.nc")
    print(f"Using HEP file: {hep_path}")
    # mod_python_interface expects a list of strings
    mod_python_interface.set_custom_hep_paths([hep_path])

    # 2. Initialize Simulation()
    mod_python_interface.init_simulation()
    
    # 2. Get initial agent count
    initial_count = mod_python_interface.get_agent_count()
    print(f"Initial agent count: {initial_count}")
    
    # 4. Set Spawn Configuration
    print("\nSetting spawn configuration...")
    # x, y, spread, counts, ns, npops
    # Fortran expects arrays of shape (ns, npops)
    # We use 1 source per population for simplicity in this test
    
    # Define spawn points within grid:
    # Grid: Lon [-11, 5.95], Lat [35, 52.25]
    # Pop 1: x=-5.0, y=40.0, spread=1, count=100
    # Pop 2: x=0.0, y=45.0, spread=2, count=200
    
    ns = 1
    npops_in = 2
    
    x_ini = np.zeros((ns, npops_in), dtype=np.float64)
    y_ini = np.zeros((ns, npops_in), dtype=np.float64)
    spread = np.zeros((ns, npops_in), dtype=np.float64)
    counts = np.zeros((ns, npops_in), dtype=np.int32)
    
    x_ini[0, 0] = -5.0
    y_ini[0, 0] = 40.0
    spread[0, 0] = 1.0
    counts[0, 0] = 100
    
    x_ini[0, 1] = 0.0
    y_ini[0, 1] = 45.0
    spread[0, 1] = 2.0
    counts[0, 1] = 200
    
    try:
        mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts)
        print("Spawn configuration set successfully.")
    except Exception as e:
        print(f"Failed to set spawn configuration: {e}")
        sys.exit(1)

    # 5. Regenerate Agents
    print("\nRegenerating agents...")
    mod_python_interface.regenerate_agents()
    
    # 6. Verify Agents
    print("\nVerifying agents...")
    count = mod_python_interface.get_agent_count()
    print(f"Total agents: {count}")
    
    if count != 300:
        print(f"FAIL: Expected 300 agents, got {count}")
    else:
        print("PASS: Agent count matches.")
        
    # Get agent data
    x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead = mod_python_interface.get_simulation_agents(count)
    
    # Check Pop 1
    pop1_indices = np.where(pop == 1)[0]
    print(f"Pop 1 count: {len(pop1_indices)}")
    if len(pop1_indices) != 100:
        print(f"FAIL: Expected 100 agents in Pop 1, got {len(pop1_indices)}")
    else:
        x1 = x[pop1_indices]
        y1 = y[pop1_indices]
        mean_x1 = np.mean(x1)
        mean_y1 = np.mean(y1)
        std_x1 = np.std(x1)
        std_y1 = np.std(y1)
        print(f"Pop 1 Mean X: {mean_x1:.2f} (Expected -5.0)")
        print(f"Pop 1 Mean Y: {mean_y1:.2f} (Expected 40.0)")
        print(f"Pop 1 Std X: {std_x1:.2f} (Expected 1.0)")
        print(f"Pop 1 Std Y: {std_y1:.2f} (Expected 1.0)")
        
        if abs(mean_x1 - (-5.0)) < 0.5 and abs(mean_y1 - 40.0) < 0.5:
             print("PASS: Pop 1 location correct.")
        else:
             print("FAIL: Pop 1 location incorrect.")

    # Check Pop 2
    pop2_indices = np.where(pop == 2)[0]
    print(f"Pop 2 count: {len(pop2_indices)}")
    if len(pop2_indices) != 200:
        print(f"FAIL: Expected 200 agents in Pop 2, got {len(pop2_indices)}")
    else:
        x2 = x[pop2_indices]
        y2 = y[pop2_indices]
        mean_x2 = np.mean(x2)
        mean_y2 = np.mean(y2)
        print(f"Pop 2 Mean X: {mean_x2:.2f} (Expected 0.0)")
        print(f"Pop 2 Mean Y: {mean_y2:.2f} (Expected 45.0)")
        
        if abs(mean_x2 - 0.0) < 0.5 and abs(mean_y2 - 45.0) < 0.5:
             print("PASS: Pop 2 location correct.")
        else:
             print("FAIL: Pop 2 location incorrect.")
    
    # Ah, this is a problem. If `init_simulation` re-reads the config file, our custom spawn points will be lost.
    # We need to set the spawn configuration AFTER `setup_world` but BEFORE `generate_initial_agents`.
    # Or we need `init_simulation` to NOT re-read config if we don't want it to, or we need a separate `re_generate_agents` function.
    
    # Let's check `mod_python_interface.f95` again.
    # init_simulation calls:
    # 1. world%init_world
    # 2. world%setup_world (likely reads config)
    # 3. generate_initial_agents_old
    
    # If we want to inject custom spawn points, we need to do it after step 2 and before step 3.
    # But `init_simulation` does all 3.
    
    # Solution: Add a flag to `init_simulation` or split it?
    # Or, simpler: Add a function `regenerate_agents` exposed to Python.
    # This function would just call `generate_initial_agents` (and maybe clear existing agents first).
    
    # Let's verify this hypothesis first.
    pass

if __name__ == "__main__":
    verify_spawn_logic()
