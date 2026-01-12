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
from full_simulation import HeadlessSimulationThread

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
        
        # Tab 3: View Editor
        self.tab_view_editor = QtWidgets.QWidget()
        self.setup_view_editor_tab()
        self.tabs.addTab(self.tab_view_editor, "View Editor")
        
        # Tab 4: Full Simulation
        self.tab_full_sim = QtWidgets.QWidget()
        self.setup_full_sim_tab()
        self.tabs.addTab(self.tab_full_sim, "Full Simulation")
        
        # Bottom Bar: Run Buttons
        self.btn_layout = QtWidgets.QHBoxLayout()
        
        self.btn_run_live = QtWidgets.QPushButton("Live View")
        self.btn_run_live.clicked.connect(self.run_live_simulation)
        self.btn_run_live.setFixedHeight(40)
        self.btn_run_live.setStyleSheet("font-size: 16px; font-weight: bold; background-color: #4CAF50; color: white;")
        self.btn_layout.addWidget(self.btn_run_live)
        
        self.btn_run_full = QtWidgets.QPushButton("Full Simulation")
        self.btn_run_full.clicked.connect(self.run_full_simulation)
        self.btn_run_full.setFixedHeight(40)
        self.btn_run_full.setStyleSheet("font-size: 16px; font-weight: bold; background-color: #2196F3; color: white;")
        self.btn_layout.addWidget(self.btn_run_full)
        
        self.layout.addLayout(self.btn_layout)
        
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

        # Buttons for Config Management
        self.config_btn_layout = QtWidgets.QHBoxLayout()
        
        self.btn_save_config = QtWidgets.QPushButton("Save Changes")
        self.btn_save_config.clicked.connect(self.save_current_config)
        self.btn_save_config.setEnabled(False) # Disabled by default
        self.config_btn_layout.addWidget(self.btn_save_config)
        
        self.btn_new_config = QtWidgets.QPushButton("Create New Config")
        self.btn_new_config.clicked.connect(self.create_new_config)
        self.config_btn_layout.addWidget(self.btn_new_config)
        
        self.right_layout.addLayout(self.config_btn_layout)

    def setup_view_editor_tab(self):
        layout = QtWidgets.QVBoxLayout(self.tab_view_editor)
        
        # --- Map View Section ---
        group_map = QtWidgets.QGroupBox("Map View")
        map_layout = QtWidgets.QVBoxLayout()
        
        # View Mode
        lbl = QtWidgets.QLabel("View Mode:")
        map_layout.addWidget(lbl)
        
        self.rb_view_2d = QtWidgets.QRadioButton("2D Flat View")
        self.rb_view_3d = QtWidgets.QRadioButton("3D Globe View")
        self.rb_view_2d.setChecked(True)
        
        hbox_mode = QtWidgets.QHBoxLayout()
        hbox_mode.addWidget(self.rb_view_2d)
        hbox_mode.addWidget(self.rb_view_3d)
        hbox_mode.addStretch()
        map_layout.addLayout(hbox_mode)
        
        # Show Agents Switch
        self.chk_show_agents = QtWidgets.QCheckBox("Show Agents")
        self.chk_show_agents.setChecked(True)
        
        # Show Debug Switch
        self.chk_show_debug = QtWidgets.QCheckBox("Show Debug Counters")
        self.chk_show_debug.setChecked(True)
        
        hbox_switches = QtWidgets.QHBoxLayout()
        hbox_switches.addWidget(self.chk_show_agents)
        hbox_switches.addWidget(self.chk_show_debug)
        hbox_switches.addStretch()
        
        map_layout.addLayout(hbox_switches)
        
        map_layout.addSpacing(10)
        
        # Color Settings
        lbl_colors = QtWidgets.QLabel("Visualization Colors (3D Globe Only):")
        map_layout.addWidget(lbl_colors)
        
        color_layout = QtWidgets.QFormLayout()
        
        self.color_buttons = {}
        
        self.defaults = {
            'bg_water': (0, 0, 0, 255),
            'bg_land': (128, 128, 128, 255),
            'hep_water': (25, 50, 200, 255),
            'hep_low': (128, 128, 128, 255),
            'hep_high': (50, 200, 50, 255)
        }
        
        # Define fields
        fields = [
            ('bg_water', 'Background Water'),
            ('bg_land', 'Background Land'),
            ('hep_water', 'HEP Water (Active)'),
            ('hep_low', 'HEP Low Land'),
            ('hep_high', 'HEP High Land')
        ]
        
        for key, name in fields:
            btn = QtWidgets.QPushButton()
            btn.setFixedWidth(100)
            btn.clicked.connect(lambda checked, k=key: self.select_color(k))
            
            # Set initial color (default)
            r, g, b, a = self.defaults[key]
            self.set_button_color(btn, (r, g, b, a))
            
            self.color_buttons[key] = btn
            color_layout.addRow(name + ":", btn)
            
        map_layout.addLayout(color_layout)
        group_map.setLayout(map_layout)
        layout.addWidget(group_map)
        
        layout.addSpacing(10)

        # --- Plots Section ---
        group_plots = QtWidgets.QGroupBox("Plots")
        plot_layout = QtWidgets.QVBoxLayout()
        
        # Frequency
        form_freq = QtWidgets.QFormLayout()
        self.spin_update_freq = QtWidgets.QSpinBox()
        self.spin_update_freq.setRange(1, 1000)
        self.spin_update_freq.setValue(10) # Default 10 ticks
        self.spin_update_freq.setSuffix(" ticks")
        self.spin_update_freq.valueChanged.connect(self.push_plot_settings)
        form_freq.addRow("Update Every:", self.spin_update_freq)
        plot_layout.addLayout(form_freq)
        
        # Plot List
        self.list_plots = QtWidgets.QListWidget()
        plot_layout.addWidget(self.list_plots)

        # Filter Settings (Moved Up)
        filter_group = QtWidgets.QGroupBox("New Plot Filter (Optional)")
        filter_layout = QtWidgets.QHBoxLayout()
        self.combo_filter_var = QtWidgets.QComboBox()
        self.combo_filter_var.addItem("None")
        self.combo_filter_var.addItems(["population (int)", "gender (int)"]) 
        
        self.spin_filter_val = QtWidgets.QSpinBox()
        self.spin_filter_val.setRange(0, 100)
        self.spin_filter_val.setPrefix("Val: ")
        
        filter_layout.addWidget(QtWidgets.QLabel("Filter By:"))
        filter_layout.addWidget(self.combo_filter_var)
        filter_layout.addWidget(self.spin_filter_val)
        filter_group.setLayout(filter_layout)
        plot_layout.addWidget(filter_group)
        
        # Plot Name (Optional)
        hbox_name = QtWidgets.QHBoxLayout()
        self.edit_plot_name = QtWidgets.QLineEdit()
        self.edit_plot_name.setPlaceholderText("Custom Plot Name (Optional)")
        hbox_name.addWidget(QtWidgets.QLabel("Name:"))
        hbox_name.addWidget(self.edit_plot_name)
        plot_layout.addLayout(hbox_name)

        # Add Plot Interface
        hbox_add = QtWidgets.QHBoxLayout()
        self.combo_plot_type = QtWidgets.QComboBox()
        self.combo_plot_type.addItems(["Time Series", "Bucket (Demographic)", "Count (Condition)"])
        self.combo_plot_type.currentIndexChanged.connect(self.update_plot_ui_state)
        
        self.combo_var = QtWidgets.QComboBox()
        self.combo_var.addItems([
            "population (int)", 
            "age (int)", 
            "gender (int)", 
            "resources (int)", 
            "children (int)",
            "is_pregnant (int)",
            "avg_resources (float)"
        ])
        
        # Condition Widgets (Count)
        self.combo_op = QtWidgets.QComboBox()
        self.combo_op.addItems(["==", "<=", ">=", "<", ">", "!="])
        self.edit_cond_val = QtWidgets.QLineEdit()
        self.edit_cond_val.setPlaceholderText("Val")
        self.edit_cond_val.setFixedWidth(60)

        # Aggregation selection (for Time Series)
        self.combo_agg = QtWidgets.QComboBox()
        self.combo_agg.addItems(["mean", "sum", "min", "max"])
        self.combo_agg.setToolTip("Aggregation Function (Time Series)")

        btn_add_plot = QtWidgets.QPushButton("Add Plot")
        btn_add_plot.clicked.connect(self.add_plot)

        hbox_add.addWidget(self.combo_plot_type)
        hbox_add.addWidget(self.combo_var)
        hbox_add.addWidget(self.combo_op)
        hbox_add.addWidget(self.edit_cond_val)
        hbox_add.addWidget(self.combo_agg)
        hbox_add.addWidget(btn_add_plot)
        
        plot_layout.addLayout(hbox_add)
        
        # Plot Management Buttons
        hbox_manage = QtWidgets.QHBoxLayout()
        btn_remove_plot = QtWidgets.QPushButton("Remove Selected")
        btn_remove_plot.clicked.connect(self.remove_plot)
        
        btn_clear_plots = QtWidgets.QPushButton("Clear All")
        btn_clear_plots.clicked.connect(self.clear_plots)
        
        hbox_manage.addWidget(btn_remove_plot)
        hbox_manage.addWidget(btn_clear_plots)
        plot_layout.addLayout(hbox_manage)
        
        group_plots.setLayout(plot_layout)
        layout.addWidget(group_plots)
        
        # Trigger initial state
        self.update_plot_ui_state()

        layout.addStretch()
        
        # Init Config Structure
        self.plot_config = {'update_freq': 10, 'plots': []}

    def update_plot_ui_state(self):
        ptype = self.combo_plot_type.currentIndex() # 0=TimeSeries, 1=Bucket, 2=Count
        
        # Default visibility
        self.combo_agg.setVisible(False)
        self.combo_op.setVisible(False)
        self.edit_cond_val.setVisible(False)
        
        if ptype == 0: # TimeSeries
            self.combo_agg.setVisible(True)
        elif ptype == 2: # Count
            self.combo_op.setVisible(True)
            self.edit_cond_val.setVisible(True)

    def add_plot(self):
        idx = self.combo_plot_type.currentIndex()
        if idx == 0: ptype = "timeseries"
        elif idx == 1: ptype = "bucket"
        else: ptype = "count"
        
        var_full = self.combo_var.currentText()
        var = var_full.split(" ")[0] # Extract name
        agg = self.combo_agg.currentText()
        
        # Check for custom name
        custom_name = self.edit_plot_name.text().strip()
        
        if custom_name:
            title = custom_name
        else:
            # Auto-generate title
            title = f"{ptype.title()}: {var}"
            
            op = None
            cond_val = None
            
            if ptype == 'timeseries':
                title += f" ({agg})"
            elif ptype == 'count':
                op = self.combo_op.currentText()
                cond_val = self.edit_cond_val.text()
                if not cond_val: cond_val = "0"
                title += f" {op} {cond_val}"
            
            # Filter
            fvar_full = self.combo_filter_var.currentText()
            if fvar_full != "None": 
                fvar = fvar_full.split(" ")[0]
                fval = self.spin_filter_val.value()
                title += f" [{fvar}={fval}]"
        
        # Re-extract params for pdef (needed even if title is custom)
        op = self.combo_op.currentText()
        cond_val = self.edit_cond_val.text()
        if not cond_val: cond_val = "0"
        
        fvar_full = self.combo_filter_var.currentText()
        if fvar_full == "None": 
            fvar = None
        else: 
            fvar = fvar_full.split(" ")[0]
            
        fval = self.spin_filter_val.value()

        pdef = {
            'type': ptype,
            'variable': var,
            'title': title,
            'filter_var': fvar,
            'filter_val': fval,
            'aggregation': agg,
            'operator': op,
            'condition_val': cond_val
        }
        
        # Specifics
        if ptype == 'bucket':
             pdef['buckets'] = 20 
             if var == 'gender':
                 QtWidgets.QMessageBox.warning(self, "Invalid Plot", "Cannot make bucket plot of gender.")
                 return

        self.plot_config['plots'].append(pdef)
        self.list_plots.addItem(title)
        self.push_plot_settings()

    def remove_plot(self):
        row = self.list_plots.currentRow()
        if row >= 0:
            self.list_plots.takeItem(row)
            if row < len(self.plot_config['plots']):
                del self.plot_config['plots'][row]
            self.push_plot_settings()

    def clear_plots(self):
        self.plot_config['plots'] = []
        self.list_plots.clear()
        self.push_plot_settings()

    def push_plot_settings(self):
        self.plot_config['update_freq'] = self.spin_update_freq.value()
        
        if self.sim_window and self.sim_window.isVisible():
            if hasattr(self.sim_window, 'update_plot_config'):
                self.sim_window.update_plot_config(self.plot_config)

    def set_button_color(self, btn, rgba):
        r, g, b, a = rgba
        # Store color in button property for retrieval
        btn.setProperty('color', rgba)
        # Update style
        btn.setStyleSheet(f"background-color: rgb({r},{g},{b}); border: 1px solid black;")

    def select_color(self, key):
        btn = self.color_buttons[key]
        current = btn.property('color')
        if not current:
            r, g, b, a = self.defaults[key]
            current = (r, g, b, a)
            
        initial = QtGui.QColor(current[0], current[1], current[2], current[3])
        color = QtWidgets.QColorDialog.getColor(initial, self, "Select Color")
        
        if color.isValid():
            new_rgba = (color.red(), color.green(), color.blue(), 255)
            self.set_button_color(btn, new_rgba)
            self.push_view_settings()

    def get_view_settings(self):
        # Convert to 0-1 range for OpenGL
        settings = {}
        for key, btn in self.color_buttons.items():
            rgba = btn.property('color')
            if rgba:
                settings[key] = (rgba[0]/255.0, rgba[1]/255.0, rgba[2]/255.0, 1.0)
        
        # Add Switches
        settings['show_agents'] = self.chk_show_agents.isChecked()
        settings['show_debug'] = self.chk_show_debug.isChecked()
        
        return settings

    def push_view_settings(self):
        if self.sim_window and self.sim_window.isVisible():
            if hasattr(self.sim_window, 'update_view_settings'):
                settings = self.get_view_settings()
                self.sim_window.update_view_settings(settings)

    def setup_full_sim_tab(self):
        layout = QtWidgets.QVBoxLayout(self.tab_full_sim)
        
        # Inputs
        form_layout = QtWidgets.QFormLayout()
        
        self.spin_start_year = QtWidgets.QSpinBox()
        self.spin_start_year.setRange(-100000, 100000)
        self.spin_start_year.setValue(-43000)
        form_layout.addRow("Start Time (Years):", self.spin_start_year)
        
        self.spin_end_year = QtWidgets.QSpinBox()
        self.spin_end_year.setRange(-100000, 100000)
        self.spin_end_year.setValue(-38000)
        form_layout.addRow("End Time (Years):", self.spin_end_year)
        
        # Output Path
        self.le_output_path = QtWidgets.QLineEdit()
        self.btn_browse_output = QtWidgets.QPushButton("...")
        self.btn_browse_output.setFixedWidth(30)
        self.btn_browse_output.clicked.connect(self.browse_output_path)
        
        out_layout = QtWidgets.QHBoxLayout()
        out_layout.addWidget(self.le_output_path)
        out_layout.addWidget(self.btn_browse_output)
        
        form_layout.addRow("Output File:", out_layout)
        
        layout.addLayout(form_layout)
        
        # Progress
        self.lbl_progress = QtWidgets.QLabel("Ready")
        layout.addWidget(self.lbl_progress)
        
        self.progress_bar = QtWidgets.QProgressBar()
        layout.addWidget(self.progress_bar)
        
        self.lbl_status = QtWidgets.QLabel("")
        layout.addWidget(self.lbl_status)
        
        layout.addStretch()

    def browse_output_path(self):
        fname, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Save Simulation Video", "", "GIF Files (*.gif)")
        if fname:
            if not fname.lower().endswith('.gif'):
                fname += '.gif'
            self.le_output_path.setText(fname)

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
        filename = os.path.basename(filepath)
        
        try:
            with open(filepath, 'r') as f:
                content = f.read()
            self.text_edit.setText(content)
            
            # Check if basic_config.nml
            if filename == "basic_config.nml":
                self.text_edit.setReadOnly(True)
                self.btn_save_config.setEnabled(False)
                self.btn_save_config.setText("Save Changes (Protected)")
            else:
                self.text_edit.setReadOnly(False)
                self.btn_save_config.setEnabled(True)
                self.btn_save_config.setText("Save Changes")
            
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

    def save_current_config(self):
        filepath = self.get_selected_config_path()
        if not filepath:
            return
            
        filename = os.path.basename(filepath)
        if filename == "basic_config.nml":
            QtWidgets.QMessageBox.warning(self, "Protected File", "basic_config.nml cannot be modified.")
            return
            
        content = self.text_edit.toPlainText()
        try:
            with open(filepath, 'w') as f:
                f.write(content)
            QtWidgets.QMessageBox.information(self, "Success", "Configuration saved.")
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to save config: {e}")

    def create_new_config(self):
        name, ok = QtWidgets.QInputDialog.getText(self, "New Configuration", "Enter name for new config (e.g. my_config.nml):")
        if ok and name:
            if not name.endswith(".nml"):
                name += ".nml"
                
            new_path = os.path.join(self.config_dir, name)
            if os.path.exists(new_path):
                QtWidgets.QMessageBox.warning(self, "Error", "File already exists.")
                return
                
            # Read basic_config.nml as template
            basic_config_path = os.path.join(self.config_dir, "basic_config.nml")
            if not os.path.exists(basic_config_path):
                QtWidgets.QMessageBox.critical(self, "Error", "basic_config.nml template not found.")
                return
                
            try:
                with open(basic_config_path, 'r') as f:
                    template_content = f.read()
                    
                with open(new_path, 'w') as f:
                    f.write(template_content)
                    
                self.populate_config_list()
                
                # Select the new item
                for i in range(self.list_widget.count()):
                    item = self.list_widget.item(i)
                    if item.data(QtCore.Qt.UserRole) == new_path:
                        self.list_widget.setCurrentItem(item)
                        self.on_config_selected(item)
                        break
                        
                QtWidgets.QMessageBox.information(self, "Success", f"Created {name} from template.")
                
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Error", f"Failed to create config: {e}")

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

    def run_live_simulation(self):
        self.prepare_simulation()
        
        # Launch Simulation Window
        if self.sim_window is not None:
            self.sim_window.close()
        
        view_mode = '3d' if self.rb_view_3d.isChecked() else '2d'
        view_settings = self.get_view_settings()
        self.sim_window = SimulationWindow(skip_init=True, view_mode=view_mode, view_settings=view_settings)
        # Apply Plot Settings
        self.sim_window.update_plot_config(self.plot_config)
        self.sim_window.show()

    def run_full_simulation(self):
        config_path = self.get_selected_config_path()
        hep_paths = self.get_hep_paths()
        
        if not config_path or not hep_paths:
            QtWidgets.QMessageBox.warning(self, "Warning", "Please select configuration and HEP files.")
            return
            
        start_year = self.spin_start_year.value()
        end_year = self.spin_end_year.value()
        
        if end_year <= start_year:
            QtWidgets.QMessageBox.warning(self, "Warning", "End time must be greater than start time.")
            return
            
        # Disable buttons
        self.btn_run_live.setEnabled(False)
        self.btn_run_full.setEnabled(False)
        self.lbl_progress.setText("Running Simulation...")
        self.progress_bar.setValue(0)
        
        # Get Configs
        module_config = self.spawn_editor.get_module_configuration()
        spawn_points = self.spawn_editor.get_spawn_points()
        npops = self.current_npops
        output_path = self.le_output_path.text().strip()
        
        # Start Thread
        self.sim_thread = HeadlessSimulationThread(start_year, end_year, config_path, hep_paths, module_config, spawn_points, npops, output_path)
        self.sim_thread.progress_update.connect(self.on_sim_progress)
        self.sim_thread.finished.connect(self.on_sim_finished)
        self.sim_thread.error.connect(self.on_sim_error)
        self.sim_thread.start()
        
    def on_sim_progress(self, progress, tick, agents, elapsed):
        self.progress_bar.setValue(progress)
        self.lbl_status.setText(f"Tick: {tick} | Agents: {agents} | Time: {elapsed:.1f}s")
        
    def on_sim_finished(self, output_file):
        self.btn_run_live.setEnabled(True)
        self.btn_run_full.setEnabled(True)
        self.lbl_progress.setText("Simulation Complete")
        self.progress_bar.setValue(100)
        QtWidgets.QMessageBox.information(self, "Success", f"Simulation finished.\nVideo saved to: {output_file}")
        
    def on_sim_error(self, error_msg):
        self.btn_run_live.setEnabled(True)
        self.btn_run_full.setEnabled(True)
        self.lbl_progress.setText("Error")
        QtWidgets.QMessageBox.critical(self, "Error", f"Simulation failed: {error_msg}")

    def prepare_simulation(self):
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
        
        # Set Active Modules
        modules = self.spawn_editor.get_module_configuration()
        if modules:
            print(f"Setting active modules: {modules}")
            mod_python_interface.set_active_modules(np.array(modules, dtype=np.int32), len(modules))
        else:
            print("Using default module configuration.")
            mod_python_interface.set_active_modules(np.array([], dtype=np.int32), 0)
        
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
                mod_python_interface.init_simulation(True)
                
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
                if len(saved_hep_paths) > 1 and len(set(saved_hep_paths)) > 1:
                    self.rb_multi.setChecked(True)
                else:
                    self.rb_single.setChecked(True)
                self.update_hep_inputs()
                for i, path in enumerate(saved_hep_paths):
                    if i < len(self.hep_widgets):
                        self.hep_widgets[i].setText(path)
                        
            # Restore Spawn Points
            spawn_points = state.get('spawn_points')
            if spawn_points:
                self.spawn_editor.set_spawn_points(spawn_points)

            # Restore Module Config
            module_config = state.get('module_config')
            if module_config:
                self.spawn_editor.set_module_configuration(module_config)
                
            # Restore Full Sim Config
            full_sim = state.get('full_sim', {})
            if full_sim:
                self.spin_start_year.setValue(full_sim.get('start_year', -43000))
                self.spin_end_year.setValue(full_sim.get('end_year', -38000))
                self.le_output_path.setText(full_sim.get('output_path', ''))
                
            # Restore View Mode
            view_mode = state.get('view_mode', '2d')
            if view_mode == '3d':
                self.rb_view_3d.setChecked(True)
            else:
                self.rb_view_2d.setChecked(True)
                
            # Restore View Colors
            view_colors = state.get('view_colors', {})
            for key, val in view_colors.items():
                if key in self.color_buttons:
                    self.set_button_color(self.color_buttons[key], tuple(val))
            
            # Restore Plot Config
            if 'plot_config' in state:
                self.plot_config = state['plot_config']
                # Restore UI
                self.spin_update_freq.setValue(self.plot_config.get('update_freq', 1))
                self.list_plots.clear()
                for p in self.plot_config.get('plots', []):
                    self.list_plots.addItem(p['title'])
                    
        except Exception as e:
            print(f"Failed to load session: {e}")

    def save_session(self):
        state = {}
        
        # Save Config
        config_path = self.get_selected_config_path()
        if config_path:
            state['config_path'] = config_path
            
        # Save HEP Paths
        raw_hep_paths = []
        for le in self.hep_widgets:
            raw_hep_paths.append(le.text())
        state['hep_paths'] = raw_hep_paths
        
        # Save Spawn Points
        state['spawn_points'] = self.spawn_editor.get_spawn_points()

        # Save Module Config
        state['module_config'] = self.spawn_editor.get_module_names()
        
        # Save Full Sim Config
        state['full_sim'] = {
            'start_year': self.spin_start_year.value(),
            'end_year': self.spin_end_year.value(),
            'output_path': self.le_output_path.text()
        }
        
        # Save View Mode
        state['view_mode'] = '3d' if self.rb_view_3d.isChecked() else '2d'
        
        # Save View Colors
        colors = {}
        for key, btn in self.color_buttons.items():
            colors[key] = btn.property('color')
        state['view_colors'] = colors
        
        # Save Plot Config
        state['plot_config'] = self.plot_config
        
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
