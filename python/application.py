import sys
import os
import glob
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

class ConfigSelectionWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("HEP Simulation Launcher")
        self.resize(800, 600)

        # Central Widget
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.layout = QtWidgets.QHBoxLayout(self.central_widget)

        # Left Panel: Config List
        self.left_panel = QtWidgets.QWidget()
        self.left_layout = QtWidgets.QVBoxLayout(self.left_panel)
        self.layout.addWidget(self.left_panel, 1)

        self.lbl_list = QtWidgets.QLabel("Select Configuration:")
        self.left_layout.addWidget(self.lbl_list)

        self.list_widget = QtWidgets.QListWidget()
        self.list_widget.itemClicked.connect(self.on_config_selected)
        self.left_layout.addWidget(self.list_widget)

        self.btn_run = QtWidgets.QPushButton("Run Simulation")
        self.btn_run.clicked.connect(self.run_simulation)
        self.btn_run.setEnabled(False) # Disabled until selection
        self.left_layout.addWidget(self.btn_run)
        
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
        self.layout.addWidget(self.right_panel, 2)

        self.lbl_content = QtWidgets.QLabel("Configuration Content:")
        self.right_layout.addWidget(self.lbl_content)

        self.text_edit = QtWidgets.QTextEdit()
        self.text_edit.setReadOnly(True)
        self.text_edit.setFont(QtGui.QFont("Monospace"))
        self.right_layout.addWidget(self.text_edit)

        # Load Config Files
        self.config_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'config'))
        self.hep_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'input', 'hep'))
        self.populate_config_list()
        
        # Initial HEP inputs
        self.update_hep_inputs()

        self.sim_window = None

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
        self.btn_run.setEnabled(True)
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

    def run_simulation(self):
        item = self.list_widget.currentItem()
        if not item:
            return

        filepath = item.data(QtCore.Qt.UserRole)
        print(f"Selected config: {filepath}")

        # Set the config path in Fortran
        mod_python_interface.set_simulation_config_path(filepath)
        
        # Set HEP Paths
        hep_paths = []
        for le in self.hep_widgets:
            path = le.text().strip()
            if path:
                hep_paths.append(path)
        
        if hep_paths:
            # If Single HEP is selected, replicate the path for all populations
            if self.rb_single.isChecked() and len(hep_paths) == 1:
                print(f"Replicating single HEP path for {self.current_npops} populations.")
                hep_paths = [hep_paths[0]] * self.current_npops

            print(f"Setting custom HEP paths: {hep_paths}")
            # Convert list of strings to what f2py expects (list of strings works usually, or char array)
            # f2py handles list of strings for char array arguments
            mod_python_interface.set_custom_hep_paths(hep_paths, len(hep_paths))

        # Launch Simulation Window
        if self.sim_window is not None:
            self.sim_window.close()
        
        self.sim_window = SimulationWindow()
        self.sim_window.show()

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    
    # Set style
    app.setStyle("Fusion")
    
    window = ConfigSelectionWindow()
    window.show()
    sys.exit(app.exec_())
