import sys
import os
import numpy as np

# Add parent dir to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing interface: {e}")
    sys.exit(1)

def verify():
    config_path = os.path.abspath("input/config/global.nml")
    hep_path = os.path.abspath("input/hep/generated/earth_hep.nc")
    
    print(f"Testing Config: {config_path}")
    print(f"Testing HEP: {hep_path}")
    
    if not os.path.exists(config_path):
        print("Config file not found!")
        sys.exit(1)
        
    if not os.path.exists(hep_path):
        print("HEP file not found!")
        sys.exit(1)
        
    # Set Config
    mod_python_interface.set_simulation_config_path(config_path)
    # Set HEP (Single path)
    mod_python_interface.set_custom_hep_paths([hep_path], 1)
    
    print("Initializing simulation...")
    mod_python_interface.init_simulation()
    
    # Get Dimensions
    dlon, dlat, npops = mod_python_interface.get_grid_dims()
    print(f"Dimensions: {dlon}x{dlat}, Pops: {npops}")
    
    if dlon != 2400 or dlat != 1200:
        print("FAILED: Dimensions mismatch!")
        sys.exit(1)
        
    # Get HEP data
    # t_hep_index = 1
    # hep_data = mod_python_interface.get_simulation_hep(1, dlon, dlat, npops)
    # print(f"HEP Data Mean: {np.mean(hep_data)}")
    
    print("SUCCESS: Simulation initialized with global config.")

if __name__ == "__main__":
    verify()
