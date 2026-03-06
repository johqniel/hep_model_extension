import sys
import os
import time
from PyQt5 import QtCore, QtWidgets

# Add parent directory to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from full_simulation import HeadlessSimulationThread

def verify_headless_sim():
    app = QtWidgets.QApplication(sys.argv)
    
    print("Verifying Headless Simulation...")
    
    # Config
    config_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'config', 'minimal_config.nml'))
    hep_paths = [os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'hep', 'europe', 'AUR.nc'))]
    
    # Short run
    start_year = -43000
    end_year = -42999 # 1 year run
    
    output_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'custom_output.gif'))
    
    # Enable Module 6 (Distribute Resources) and 7 (Resource Mortality)
    modules = [6, 7]
    
    thread = HeadlessSimulationThread(start_year, end_year, config_path, hep_paths, modules, [], 1, output_path)
    
    def on_progress(p, t, a, e):
        print(f"Progress: {p}% | Tick: {t} | Agents: {a} | Time: {e:.2f}s")
        
    def on_finished(f):
        print(f"Finished! File: {f}")
        app.quit()
        
    def on_error(e):
        print(f"Error: {e}")
        app.quit()
        
    thread.progress_update.connect(on_progress)
    thread.finished.connect(on_finished)
    thread.error.connect(on_error)
    
    thread.start()
    
    # Run loop
    sys.exit(app.exec_())

if __name__ == "__main__":
    verify_headless_sim()
