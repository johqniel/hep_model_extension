import sys
import os
import glob
import json
import numpy as np
from PyQt5 import QtWidgets, QtCore, QtGui

# Add the parent directory to sys.path to find the compiled module
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    # Fix for f2py module nesting
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    sys.exit(1)

from simulation import SimulationWindow
from spawn_editor import SpawnPointEditor

class MainApplication(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("HEP Simulation Suite")
        self.resize(1200, 800)

        # Central Widget & Tabs
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.layout = QtWidgets.QVBoxLayout(self.central_widget)
        
        self.tabs = QtWidgets.QTabWidget()
        self.layout.addWidget(self.tabs)
        
        # Tab 1: Configuration
        self.tab_config = QtWidgets.QWidget()
        self.setup_config_tab()
        self.tabs.addTab(self.tab_config, "Configuration")
        
        # Tab 2: Spawn Editor
        self.spawn_editor = SpawnPointEditor()
        self.tabs.addTab(self.spawn_editor, "Spawn Editor")
        
        # Bottom Bar: Run Button
        self.btn_run = QtWidgets.QPushButton("Run Simulation")
        self.btn_run.clicked.connect(self.run_simulation)
        self.btn_run.setFixedHeight(40)
        self.btn_run.setStyleSheet("font-size: 16px; font-weight: bold; background-color: #4CAF50; color: white;")
        self.layout.addWidget(self.btn_run)
        
        # Session State
        self.session_file = os.path.join(os.path.dirname(__file__), "session_state.json")
        
        # Load Config Files
        self.config_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'config'))
        self.hep_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'hep'))
        self.populate_config_list()
        
        # Initial HEP inputs
        self.update_hep_inputs()
        
        self.sim_window = None
        
        # Load Session
        self.load_session()
        
        # Connect Tab Change
        self.tabs.currentChanged.connect(self.on_tab_changed)

    def setup_config_tab(self):
        self.config_layout = QtWidgets.QHBoxLayout(self.tab_config)
        
        # Left Panel: Config List
        self.left_panel = QtWidgets.QWidget()
        self.left_layout = QtWidgets.QVBoxLayout(self.left_panel)
        self.config_layout.addWidget(self.left_panel, 1)

        self.lbl_list = QtWidgets.QLabel("Select Configuration:")
        self.left_layout.addWidget(self.lbl_list)

        self.list_widget = QtWidgets.QListWidget()
        self.list_widget.itemClicked.connect(self.on_config_selected)
        self.left_layout.addWidget(self.list_widget)

        # --- HEP Input Selection ---
        self.left_layout.addSpacing(20)
        self.lbl_hep = QtWidgets.QLabel("HEP Input:")
        self.left_layout.addWidget(self.lbl_hep)
        
        # Radio Buttons
        self.rb_single = QtWidgets.QRadioButton("Single HEP File (All Pops)")
        self.rb_multi = QtWidgets.QRadioButton("Multiple HEP Files (Per Pop)")
        self.rb_single.setChecked(True)
        self.rb_single.toggled.connect(self.update_hep_inputs)
        self.left_layout.addWidget(self.rb_single)
        self.left_layout.addWidget(self.rb_multi)
        
        # Container for file inputs
        self.hep_input_container = QtWidgets.QWidget()
        self.hep_input_layout = QtWidgets.QVBoxLayout(self.hep_input_container)
        self.hep_input_layout.setContentsMargins(0, 0, 0, 0)
        self.left_layout.addWidget(self.hep_input_container)
        
        self.hep_widgets = [] # List of (label, line_edit, button) tuples
        self.current_npops = 1

        # Right Panel: Config Content
        self.right_panel = QtWidgets.QWidget()
        self.right_layout = QtWidgets.QVBoxLayout(self.right_panel)
        self.config_layout.addWidget(self.right_panel, 2)

        self.lbl_content = QtWidgets.QLabel("Configuration Content:")
        self.right_layout.addWidget(self.lbl_content)

        self.text_edit = QtWidgets.QTextEdit()
        self.text_edit.setReadOnly(True)
        self.text_edit.setFont(QtGui.QFont("Monospace"))
        self.right_layout.addWidget(self.text_edit)

    def populate_config_list(self):
        self.list_widget.clear()
        if not os.path.exists(self.config_dir):
            self.text_edit.setText(f"Error: Config directory not found: {self.config_dir}")
            return

        nml_files = glob.glob(os.path.join(self.config_dir, "*.nml"))
        nml_files.sort()
        
        for f in nml_files:
            filename = os.path.basename(f)
            item = QtWidgets.QListWidgetItem(filename)
            item.setData(QtCore.Qt.UserRole, f) # Store full path
            self.list_widget.addItem(item)

    def on_config_selected(self, item):
        filepath = item.data(QtCore.Qt.UserRole)
        
        try:
            with open(filepath, 'r') as f:
                content = f.read()
            self.text_edit.setText(content)
            
            # Parse npops from content
            self.current_npops = 1
            for line in content.splitlines():
                if 'npops' in line.lower():
                    try:
                        # simple parsing: npops = 3
                        parts = line.split('=')
                        if len(parts) > 1:
                            val = parts[1].strip().split(',')[0].strip() # handle comma if present
                            self.current_npops = int(val)
                    except:
                        pass
            
            self.update_hep_inputs()
            
        except Exception as e:
            self.text_edit.setText(f"Error reading file: {e}")

    def update_hep_inputs(self):
        # Clear existing widgets
        for i in reversed(range(self.hep_input_layout.count())): 
            self.hep_input_layout.itemAt(i).widget().setParent(None)
        self.hep_widgets = []
        
        num_inputs = 1 if self.rb_single.isChecked() else self.current_npops
        
        for i in range(num_inputs):
            row_widget = QtWidgets.QWidget()
            row_layout = QtWidgets.QHBoxLayout(row_widget)
            row_layout.setContentsMargins(0, 0, 0, 0)
            
            label_text = "HEP File:" if num_inputs == 1 else f"Pop {i+1} HEP:"
            lbl = QtWidgets.QLabel(label_text)
            row_layout.addWidget(lbl)
            
            le = QtWidgets.QLineEdit()
            row_layout.addWidget(le)
            
            btn = QtWidgets.QPushButton("...")
            btn.setFixedWidth(30)
            # Use closure to capture line edit
            btn.clicked.connect(lambda checked, l=le: self.browse_hep_file(l))
            row_layout.addWidget(btn)
            
            self.hep_input_layout.addWidget(row_widget)
            self.hep_widgets.append(le)
            
    def browse_hep_file(self, line_edit):
        fname, _ = QtWidgets.QFileDialog.getOpenFileName(self, 'Open HEP File', self.hep_dir, "NetCDF Files (*.nc)")
        if fname:
            line_edit.setText(fname)

    def get_selected_config_path(self):
        item = self.list_widget.currentItem()
        if item:
            return item.data(QtCore.Qt.UserRole)
        return None

    def get_hep_paths(self):
        paths = []
        for le in self.hep_widgets:
            path = le.text().strip()
            if path:
                paths.append(path)
        
        if paths and self.rb_single.isChecked() and len(paths) == 1:
            # Replicate for all pops
            paths = [paths[0]] * self.current_npops
            
        return paths

    def on_tab_changed(self, index):
        if index == 1: # Spawn Editor Tab
            config_path = self.get_selected_config_path()
            hep_paths = self.get_hep_paths()
            
            if not config_path:
                QtWidgets.QMessageBox.warning(self, "Warning", "Please select a configuration first.")
                self.tabs.setCurrentIndex(0)
                return
                
            if not hep_paths:
                QtWidgets.QMessageBox.warning(self, "Warning", "Please select HEP file(s) first.")
                self.tabs.setCurrentIndex(0)
                return

            # Pass context to editor
            self.spawn_editor.set_hep_context(config_path, hep_paths)

    def run_simulation(self):
        config_path = self.get_selected_config_path()
        if not config_path:
            QtWidgets.QMessageBox.warning(self, "Warning", "Please select a configuration.")
            return

        hep_paths = self.get_hep_paths()
        if not hep_paths:
             QtWidgets.QMessageBox.warning(self, "Warning", "Please select HEP file(s).")
             return

        print(f"Running simulation with config: {config_path}")
        print(f"HEP Paths: {hep_paths}")

        # Set Config and HEP in Fortran
        mod_python_interface.set_simulation_config_path(config_path)
        mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))
        
        # Check for custom spawn points
        spawn_points = self.spawn_editor.get_spawn_points()
        
        if spawn_points:
            print("Using custom spawn points from editor.")
            # Prepare data for Fortran
            # Group by population
            points_by_pop = {}
            for p in spawn_points:
                pop = p['pop']
                if pop not in points_by_pop:
                    points_by_pop[pop] = []
                points_by_pop[pop].append(p)
                
            max_sources = 0
            for pop in points_by_pop:
                max_sources = max(max_sources, len(points_by_pop[pop]))
                
            ns = max_sources
            npops = self.current_npops # Use current npops from config parsing
            
            x_ini = np.zeros((ns, npops), dtype=np.float64)
            y_ini = np.zeros((ns, npops), dtype=np.float64)
            spread = np.zeros((ns, npops), dtype=np.float64)
            counts = np.zeros((ns, npops), dtype=np.int32)
            
            for pop, points in points_by_pop.items():
                pop_idx = pop - 1 
                if pop_idx < npops:
                    for i, p in enumerate(points):
                        x_ini[i, pop_idx] = p['x']
                        y_ini[i, pop_idx] = p['y']
                        spread[i, pop_idx] = p['spread']
                        counts[i, pop_idx] = p['count']
            
            try:
                # Initialize simulation first (loads config, setup world, default agents)
                mod_python_interface.init_simulation()
                
                # Overwrite with custom spawn configuration
                mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts)
                
                # Regenerate agents with new config
                mod_python_interface.regenerate_agents()
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Error", f"Failed to set spawn configuration: {e}")
                return
        else:
            print("Using default spawn points from config.")
            mod_python_interface.init_simulation()

        # Launch Simulation Window
        if self.sim_window is not None:
            self.sim_window.close()
        
        self.sim_window = SimulationWindow(skip_init=True) # Already initialized above
        self.sim_window.show()

    def load_session(self):
        if not os.path.exists(self.session_file):
            return
            
        try:
            with open(self.session_file, 'r') as f:
                state = json.load(f)
                
            # Restore Config
            config_path = state.get('config_path')
            if config_path:
                # Find item in list
                for i in range(self.list_widget.count()):
                    item = self.list_widget.item(i)
                    if item.data(QtCore.Qt.UserRole) == config_path:
                        self.list_widget.setCurrentItem(item)
                        self.on_config_selected(item)
                        break
            
            # Restore HEP Paths
            saved_hep_paths = state.get('hep_paths', [])
            if saved_hep_paths:
                # Check if single or multi
                # If we have multiple paths but they are all the same, it might have been single mode.
                # But let's just fill what we have.
                
                # If saved paths count > 1 and distinct, switch to multi
                if len(saved_hep_paths) > 1 and len(set(saved_hep_paths)) > 1:
                    self.rb_multi.setChecked(True)
                else:
                    self.rb_single.setChecked(True)
                
                # Force update inputs to match mode
                self.update_hep_inputs()
                
                # Fill widgets
                for i, path in enumerate(saved_hep_paths):
                    if i < len(self.hep_widgets):
                        self.hep_widgets[i].setText(path)
                        
            # Restore Spawn Points
            spawn_points = state.get('spawn_points')
            if spawn_points:
                self.spawn_editor.set_spawn_points(spawn_points)
                
        except Exception as e:
            print(f"Failed to load session: {e}")

    def save_session(self):
        state = {}
        
        # Save Config
        config_path = self.get_selected_config_path()
        if config_path:
            state['config_path'] = config_path
            
        # Save HEP Paths
        # We save what is in the widgets, but maybe we should save the resolved list?
        # Let's save the raw inputs from widgets to restore UI state exactly.
        raw_hep_paths = []
        for le in self.hep_widgets:
            raw_hep_paths.append(le.text())
        state['hep_paths'] = raw_hep_paths
        
        # Save Spawn Points
        state['spawn_points'] = self.spawn_editor.get_spawn_points()
        
        try:
            with open(self.session_file, 'w') as f:
                json.dump(state, f, indent=4)
        except Exception as e:
            print(f"Failed to save session: {e}")

    def closeEvent(self, event):
        self.save_session()
        event.accept()

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    app.setStyle("Fusion")
    window = MainApplication()
    window.show()
    sys.exit(app.exec_())
