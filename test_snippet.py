import numpy as np

# Mock data
x = np.array([12.0, 15.0])
y = np.array([-40.0, 45.0])
lon_0 = -180.0
lat_0 = -90.0
delta_lon = 0.5
delta_lat = 0.5
dlon = 720
dlat = 360

cmap = np.zeros((dlon, dlat), dtype=int)
cmap[384, 100] = 5

try:
    gx = np.floor((np.array(x) - lon_0) / delta_lon).astype(int)
    gy = np.floor((np.array(y) - lat_0) / delta_lat).astype(int)
    valid = (gx >= 0) & (gx < dlon) & (gy >= 0) & (gy < dlat)
    
    agent_ids = cmap[gx[valid], gy[valid]]
    print("agent_ids:", agent_ids)
    
    uq_agent, cts_agent = np.unique(agent_ids, return_counts=True)
    agent_id_dump = ", ".join([f"Val {v}:{c}ag" for v, c in zip(uq_agent, cts_agent)])
    print("agent_id_dump:", agent_id_dump)
    
    unclustered_count = np.sum(agent_ids <= 0)
    print("unclustered:", unclustered_count)
    
    uq, cts = np.unique(cmap[cmap > 0], return_counts=True)
    cluster_sizes_str = ", ".join([f"ID {u}:{c}px" for u, c in zip(uq, cts)])
    print("cluster sizes:", cluster_sizes_str)
    print("SUCCESS")
except Exception as e:
    print(f"Exception: {e}")
