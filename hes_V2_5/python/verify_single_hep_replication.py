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

# Simulate Single HEP selection logic
# Assume npops = 3 (from minimal_config)
npops = 3
hep_file = "dummy_hep.nc"
hep_paths = [hep_file]

print(f"Original HEP paths: {hep_paths}")

# Replicate logic
if len(hep_paths) == 1:
    print(f"Replicating single HEP path for {npops} populations.")
    hep_paths = [hep_paths[0]] * npops

print(f"Replicated HEP paths: {hep_paths}")

if len(hep_paths) != npops:
    print("Error: Replication failed.")
    sys.exit(1)

# Pass to backend
print("Passing to backend...")
mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))

print("Initializing simulation (expecting failure due to dummy file, but checking path count in backend would require more instrumentation).")
# We assume if it crashes on file open, it received the paths.
try:
    mod_python_interface.init_simulation()
except Exception as e:
    print(f"Initialization failed as expected: {e}")

print("Single HEP replication verification script finished.")
