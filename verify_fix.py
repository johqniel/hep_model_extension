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
print(f"HEP Paths: {hep_paths}")

try:
    # Mimic application.py sequence
    mod_python_interface.set_simulation_config_path(config_path)
    
    # Convert list of strings to numpy array of strings (fixed length)
    # f2py usually handles list of strings if signature matches, but let's be careful.
    # The interface expects character(len=256), dimension(:)
    
    mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))
    
    mod_python_interface.init_simulation()
    print("Verification Successful!")
except Exception as e:
    print(f"Verification Failed: {e}")
    sys.exit(1)
