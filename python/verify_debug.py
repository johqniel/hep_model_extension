
import sys
import os
import numpy as np

# Add parent dir to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    # Fix for f2py module nesting
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
    print("Module loaded successfully.")
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    sys.exit(1)

# Check symbols first
print("Inspecting module symbols...")
attrs = dir(mod_python_interface)
if 'get_debug_stats' in attrs:
    print("SUCCESS: 'get_debug_stats' is present in the module.")
else:
    print("FAILURE: 'get_debug_stats' is NOT found in the module.")
print("All attributes:", attrs)

# Initialize mostly to ensure structures exist
mod_python_interface.init_simulation()

try:
    # Try calling the debug stats
    print("Calling get_debug_stats()...")
    natural, starv, oob, confl, rnd = mod_python_interface.get_debug_stats()
    print(f"Success! Stats: Nat={natural}, Starv={starv}, OOB={oob}, Confl={confl}, Rnd={rnd}")
except Exception as e:
    print(f"FAILED to call get_debug_stats: {e}")
    # Inspect module content
    print("\nModule attributes:")
    print(dir(mod_python_interface))
