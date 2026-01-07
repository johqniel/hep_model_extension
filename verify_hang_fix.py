import sys
import os
import numpy as np

# Add python dir to path
sys.path.append(os.path.abspath("python"))

try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    sys.exit(1)

config_path = "input/config/testing_config.nml"
hep_paths = [
    "input/hep/europe/AUR.nc",
    "input/hep/europe/NEA.nc",
    "input/hep/europe/MIX.nc"
]

print(f"Testing config: {config_path}")

try:
    mod_python_interface.set_simulation_config_path(config_path)
    mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))
    
    print("Initializing simulation with skip_generation=True...")
    # This should NOT hang even if config has no spawn points (because we skip generation)
    mod_python_interface.init_simulation(True)
    
    print("Initialization successful (skipped generation).")
    
    # Now set custom spawn points (dummy values)
    ns = 1
    npops = 3
    x_ini = np.zeros((ns, npops), dtype=np.float64)
    y_ini = np.zeros((ns, npops), dtype=np.float64)
    spread = np.zeros((ns, npops), dtype=np.float64)
    counts = np.zeros((ns, npops), dtype=np.int32)
    
    # Set one agent
    counts[0, 0] = 1
    x_ini[0, 0] = -10.0
    y_ini[0, 0] = 40.0
    
    print("Setting custom spawn config...")
    mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts)
    
    print("Regenerating agents...")
    mod_python_interface.regenerate_agents()
    
    count = mod_python_interface.get_agent_count()
    print(f"Agent count: {count}")
    
    if count > 0:
        print("Verification Successful!")
    else:
        print("Verification Failed: No agents spawned.")
        sys.exit(1)

except Exception as e:
    print(f"Verification Failed: {e}")
    sys.exit(1)
