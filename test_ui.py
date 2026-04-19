import sys
import numpy as np

sys.path.insert(0, './python')
import mod_python_interface

try:
    mod_python_interface.init_sim_step_1("input/config/basic_config.nml", "input/config/main_fortran_config.nml")
    res = mod_python_interface.get_debug_stats()
    print("get_debug_stats:", res)
except Exception as e:
    print(f"Exception: {e}")
