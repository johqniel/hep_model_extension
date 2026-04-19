import sys
import numpy as np
sys.path.insert(0, './python')
from mod_python_interface import mod_python_interface

try:
    cmap = mod_python_interface.get_cell_cluster_map(10, 5)
    print("Requested 10, 5")
    print("cmap shape:", cmap.shape)
except Exception as e:
    print(f"Exception: {e}")
