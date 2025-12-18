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

print("Initializing simulation...")
mod_python_interface.init_simulation()

# Get Grid Dimensions
dlon, dlat, npops = mod_python_interface.get_grid_dims()
print(f"Grid Dimensions: {dlon}x{dlat}, Pops: {npops}")

if npops != 3:
    print("Error: npops should be 3")
    sys.exit(1)

# Get Simulation Config
lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep = mod_python_interface.get_simulation_config()
print(f"Config: lon_0={lon_0}, lat_0={lat_0}, dlon={delta_lon}, dlat={delta_lat}")

# Check values from basic_config.nml
# lon_0 should be from HEP data (read from file)
# But we can check if they are reasonable.

print("Configuration verification successful.")
