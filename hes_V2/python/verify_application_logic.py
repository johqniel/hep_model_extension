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

# Set Config Path
mod_python_interface.set_simulation_config_path(config_path)

print("Initializing simulation...")
mod_python_interface.init_simulation()

# Get Grid Dimensions
dlon, dlat, npops = mod_python_interface.get_grid_dims()
print(f"Grid Dimensions: {dlon}x{dlat}, Pops: {npops}")

if npops != 3:
    print("Error: npops should be 3")
    sys.exit(1)

print("Application logic verification successful.")
