import sys
import os
import numpy as np

# Add the parent directory to sys.path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

import mod_python_interface

def test_interface():
    print("Testing mod_python_interface...")
    print(dir(mod_python_interface))
    
    # Check if there is a submodule
    if hasattr(mod_python_interface, 'mod_python_interface'):
        print("Found submodule mod_python_interface")
        mpi = mod_python_interface.mod_python_interface
    else:
        mpi = mod_python_interface

    print("get_simulation_agents doc:")
    print(mpi.get_simulation_agents.__doc__)
    print("get_simulation_hep doc:")
    print(mpi.get_simulation_hep.__doc__)

    # 1. Init
    print("Initializing...")
    mpi.init_simulation()
    
    # 2. Get Dims
    dlon, dlat, npops = mpi.get_grid_dims()
    print(f"Grid Dims: {dlon}x{dlat}, Pops: {npops}")

    # 2b. Get Config
    lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep = mpi.get_simulation_config()
    print(f"Config: lon_0={lon_0}, lat_0={lat_0}, dlon={delta_lon}, dlat={delta_lat}")
    
    # 3. Step
    print("Stepping...")
    for t in range(1, 11):
        mpi.step_simulation(t)
        
    # 4. Get Agents
    count = mpi.get_agent_count()
    print(f"Agent Count: {count}")
    
    if count > 0:
        x, y, pop = mpi.get_simulation_agents(count)
        print(f"First agent pos: {x[0]}, {y[0]}")
        
    # 5. Get HEP
    hep = mpi.get_simulation_hep(1, dlon, dlat, npops)
    print(f"HEP Max: {np.max(hep)}")
    
    print("Test Complete.")

if __name__ == "__main__":
    test_interface()
