import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'python')))
import mod_python_interface
mod_python_interface = mod_python_interface.mod_python_interface

print("Init sim step 1")
mod_python_interface.init_sim_step_1()

print("Setting config path")
mod_python_interface.set_simulation_config_path("input/config/basic_config.nml")

full_paths = ["input/hep/hepParams_2d_Europe.nc"] * 3
# Let's set it
print("Setting custom hep paths")
mod_python_interface.set_custom_hep_paths(full_paths, len(full_paths))

print("Init sim step 2.1")
mod_python_interface.init_sim_step_2_part_1()

print("Success!")
