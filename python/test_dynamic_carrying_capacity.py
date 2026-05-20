import sys
sys.path.append('.')
import mod_python_interface as mpi
import numpy as np
import os

m = mpi.mod_python_interface

# 1. Setup paths
config_path = os.path.abspath("../input/config/basic_config.nml")
hep_path = os.path.abspath("../input/hep/background.nc")

m.set_simulation_config_path(config_path.encode('utf-8'))
m.set_custom_hep_paths(hep_path.encode('utf-8'))

# 2. Toggle active modules:
# 21 = Reviewed Agent Motion
# 22 = Cluster Death
# 23 = Cluster Birth
# 24 = Creativity (Individual)
# 25 = Creativity (Cluster Collection)
active = np.array([21, 22, 23, 24, 25], dtype=np.int32)
m.set_active_modules(active)

print("Initializing simulation...")
m.init_sim_step_1()
m.init_sim_step_2()
m.init_sim_step_3(False)
m.init_sim_step_4()

# Setup clustering store
print("Initializing clustering store...")
m.init_cluster_store()
m.set_clustering_algorithm(2) # 2 = K-Means

# Set cluster update interval to 1 so we get clustering updates every tick
# We do this directly using python interface setting if available,
# or we just trigger clustering manually.

print("\n--- Running 15 simulation ticks ---")
for t in range(1, 16):
    m.step_simulation(t)
    
    # Check cluster counts and details
    n_clusters = int(m.get_cluster_count()[0])
    print(f"\nTick {t}: Found {n_clusters} spatial clusters.")
    
    # Loop over all active clusters and print their carrying capacities
    for c_rank in range(1, n_clusters + 1):
        # get_cluster_info returns (iinfo, rinfo)
        # jp_in = 1 (Population index 1)
        iinfo, rinfo = m.get_cluster_info(c_rank, 1)
        
        cid = iinfo[0]
        n_cells = iinfo[1]
        n_agents = iinfo[2]
        
        centroid_x = rinfo[0]
        centroid_y = rinfo[1]
        hep_sum = rinfo[2]
        adapted_nc = rinfo[3]  # This will contain pop_NC_AV if creativity is active
        
        print(f"  Cluster ID {cid}: Cells={n_cells}, Agents={n_agents}, Dynamic Carrying Capacity (adapted Nc)={adapted_nc:.2f}")

print("\nRetrieving agent creativity values...")
count = m.get_agent_count()
if count > 0:
    x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead, cluster_rank, creativity = m.get_simulation_agents(count)
    print(f"Total living agents: {count}")
    print(f"Mean creativity: {np.mean(creativity):.4f}")
    print(f"Min creativity: {np.min(creativity):.4f}")
    print(f"Max creativity: {np.max(creativity):.4f}")

m.cleanup_simulation()
print("\nDynamic carrying capacity simulation run finished successfully!")
