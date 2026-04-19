import numpy as np
cmap = np.zeros((10, 10))
cmap[...] = 5
gx = np.array([-1, -2])
gy = np.array([-1, -2])
valid = (gx >= 0) & (gx < 10) & (gy >= 0) & (gy < 10)
agent_ids = cmap[gx[valid], gy[valid]]
uq, cts = np.unique(agent_ids, return_counts=True)
val_str = ", ".join([f"Val {v}:{c}ag" for v, c in zip(uq, cts)])
unclustered = np.sum(agent_ids <= 0)
print("val_str:", val_str)
print("unclustered:", unclustered)
