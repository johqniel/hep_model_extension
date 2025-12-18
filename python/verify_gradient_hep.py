import sys
import os
import numpy as np

# Add the parent directory to sys.path to find the compiled module
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    # Fix for f2py module nesting
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    sys.exit(1)

config_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'config', 'minimal_config.nml'))
print(f"Setting config path to: {config_path}")
mod_python_interface.set_simulation_config_path(config_path)

# Use Gradient HEP file
hep_file = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'hep', 'gradient', 'AUR.nc'))
if not os.path.exists(hep_file):
    print(f"Error: Test file not found: {hep_file}")
    sys.exit(1)

# Replicate for 3 populations (as in minimal_config)
npops = 3
hep_paths = [hep_file] * npops

print(f"Setting custom HEP paths to: {hep_paths}")
mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))

print("Initializing simulation...")
try:
    mod_python_interface.init_simulation()
    print("Initialization successful (Gradient HEP loaded).")
except Exception as e:
    print(f"Initialization failed: {e}")

print("Gradient HEP verification script finished.")
