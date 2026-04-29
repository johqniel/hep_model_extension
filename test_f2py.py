import sys
sys.path.append('python')
import mod_python_interface
print(dir(mod_python_interface))
try:
    print(mod_python_interface.get_dynamic_state_stats())
except Exception as e:
    print("Error:", e)
