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

def verify_restart():
    config_path = os.path.abspath("input/config/basic_config.nml")
    hep_path = os.path.abspath("input/hep/gradient/AUR.nc")
    
    print("--- Test Run 1 ---")
    mod_python_interface.set_simulation_config_path(config_path)
    mod_python_interface.set_custom_hep_paths([hep_path], 1)
    mod_python_interface.init_simulation()
    print("Run 1 Initialized.")
    
    print("\n--- Test Run 2 (Restart) ---")
    # This should trigger the re-allocation logic
    mod_python_interface.init_simulation()
    print("Run 2 Initialized (Success if no crash).")

if __name__ == "__main__":
    verify_restart()
