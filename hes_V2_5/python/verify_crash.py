import sys
import os
import numpy as np
import time

# Add python directory to path
sys.path.append(os.path.join(os.getcwd(), 'python'))

# Import Fortran interface
try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError:
    print("Error: Could not import mod_python_interface. Make sure the extension is built.")
    sys.exit(1)

def test_crash():
    print("Initializing simulation...")
    mod_python_interface.set_simulation_config_path("input/config/basic_config.nml")
    
    # Create dummy HEP data (1x1 grid)
    # We need a valid HEP file or the simulation might complain.
    # But for now let's try to use the existing logic if possible.
    # We can pass empty HEP paths if we don't use them for spawning?
    # No, setup_world needs HEP.
    
    # Let's use the actual HEP files if available, or mock them.
    # Assuming basic_config.nml points to valid HEP files or we can override.
    # Let's try to use the "minimal_config.nml" if it exists, or just basic.
    
    # We need to set HEP paths.
    # Let's assume the user has them.
    # Or we can create a dummy netcdf file.
    
    # For now, let's try to just init and see if it works.
    # If it fails on HEP, we'll fix it.
    
    # Use valid HEP files from europe folder
    hep_paths = ["input/hep/europe/AUR.nc", "input/hep/europe/NEA.nc", "input/hep/europe/MIX.nc"]
    
    # Check if they exist
    if not all(os.path.exists(p) for p in hep_paths):
        print("Error: Europe HEP files not found.")
        sys.exit(1)
        
    mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))
    
    mod_python_interface.init_simulation()
    
    print("Simulation initialized.")
    
    # Now try to spawn MANY agents.
    # 3 populations.
    # Let's try 100,000 agents per population.
    n_agents = 100000
    n_pops = 3
    
    print(f"Attempting to spawn {n_agents * n_pops} agents...")
    
    # Create spawn points
    # x, y, pop_id, count
    # We need to pass 2D arrays (ns, npops)
    ns = 1
    npops = 3
    x_ini = np.zeros((ns, npops), dtype=np.float64)
    y_ini = np.zeros((ns, npops), dtype=np.float64)
    spread = np.zeros((ns, npops), dtype=np.float64)
    counts = np.zeros((ns, npops), dtype=np.int32)
    
    for p in range(npops):
        x_ini[0, p] = 0.0
        y_ini[0, p] = 40.0
        spread[0, p] = 0.5
        counts[0, p] = n_agents
        
    mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts)
    
    print("Calling regenerate_agents...")
    mod_python_interface.regenerate_agents()
    
    print("Success! Agents spawned.")
    
    # Enable modules
    # 1: Death, 2: Births, 3: Move, 4: Age, 5: Find Mate
    modules = np.array([1, 5, 2, 3, 4], dtype=np.int32)
    mod_python_interface.set_active_modules(modules, len(modules))
    
    # Now step simulation
    print("Stepping simulation...")
    for t in range(100): # Run longer to allow births
        mod_python_interface.step_simulation(t)
        if t % 10 == 0:
            print(f"Step {t} complete.")
            
    print("Test passed without crash.")

def create_dummy_hep(paths):
    import netCDF4
    for p in paths:
        if os.path.exists(p):
            os.remove(p) # Force recreate
        print(f"Creating {p}")
        ds = netCDF4.Dataset(p, 'w', format='NETCDF4')
        ds.createDimension('lon', 10)
        ds.createDimension('lat', 10)
        ds.createDimension('time', 1)
        
        lat = ds.createVariable('lat', 'f4', ('lat',))
        lon = ds.createVariable('lon', 'f4', ('lon',))
        acchep = ds.createVariable('AccHEP', 'f4', ('lon', 'lat', 'time'))
        watermask = ds.createVariable('watermask', 'i4', ('lon', 'lat'))
        
        lat[:] = np.linspace(30, 60, 10)
        lon[:] = np.linspace(-10, 30, 10)
        acchep[:] = 1.0
        watermask[:] = 1 # All land
        
        ds.close()

if __name__ == "__main__":
    test_crash()
