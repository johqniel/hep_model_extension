import sys
import os
import glob
import json
import numpy as np
import multiprocessing
try:
    multiprocessing.set_start_method('spawn')
except RuntimeError:
    pass
from PyQt5 import QtWidgets, QtCore, QtGui
import time

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This Script defines the Python interface that is used to setup a simulation. 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The class MainApplication is the main window of the application. 
# It contains the following tabs:
# 1. Configuration: Allows the user to select a configuration file.
# 2. Spawn Editor: Allows the user to edit the spawn points.
# 3. View Editor: Allows the user to edit the view.
# 4. Full Simulation: Allows the user to setup a full run of the simulation, that also exports data

# This script relies on f2py, which is a tool that is used to compile Fortran code to Python code.



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
from full_simulation import HeadlessSimulationThread, FullSimulationWindow, MultiSimulationWindow
from test_simulation import TestSimulationConfigDialog, TestSimulationSuiteWindow


def show_selectable_error(parent, title, text):
    msg_box = QtWidgets.QMessageBox(parent)
    msg_box.setIcon(QtWidgets.QMessageBox.Critical)
    msg_box.setWindowTitle(title)
    msg_box.setText(text)
    msg_box.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
    msg_box.exec_()


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
        
        self.btn_run_test = QtWidgets.QPushButton("Test Simulation")
        self.btn_run_test.clicked.connect(self.run_test_simulation)
        self.btn_run_test.setFixedHeight(40)
        self.btn_run_test.setStyleSheet("font-size: 16px; font-weight: bold; background-color: #FF9800; color: white;")
        self.btn_layout.addWidget(self.btn_run_test)
        
        self.layout.addLayout(self.btn_layout)
        
        # Session State
        self.session_file = os.path.join(os.path.dirname(__file__), "session_state.json")
        self.test_sim_state = {}
        
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
        self.rb_view_graphs = QtWidgets.QRadioButton("Graphs Only View")
        self.rb_view_2d.setChecked(True)
        
        hbox_mode = QtWidgets.QHBoxLayout()
        hbox_mode.addWidget(self.rb_view_2d)
        hbox_mode.addWidget(self.rb_view_3d)
        hbox_mode.addWidget(self.rb_view_graphs)
        hbox_mode.addStretch()
        map_layout.addLayout(hbox_mode)
        
        # Show Agents Switch
        self.chk_show_agents = QtWidgets.QCheckBox("Show Agents")
        self.chk_show_agents.setChecked(True)
        self.chk_show_agents.stateChanged.connect(self.push_view_settings)
        
        # Show Debug Switch
        self.chk_show_debug = QtWidgets.QCheckBox("Show Debug Counters")
        self.chk_show_debug.setChecked(True)
        self.chk_show_debug.stateChanged.connect(self.push_view_settings)

        # Show Performance Switch
        self.chk_show_perf = QtWidgets.QCheckBox("Show Performance Metrics")
        self.chk_show_perf.setChecked(False)
        self.chk_show_perf.stateChanged.connect(self.push_view_settings)
        
        hbox_switches = QtWidgets.QHBoxLayout()
        hbox_switches.addWidget(self.chk_show_agents)
        hbox_switches.addWidget(self.chk_show_debug)
        hbox_switches.addWidget(self.chk_show_perf)
        
        # Show Clusters Switch
        self.chk_show_clusters = QtWidgets.QCheckBox("Show Clusters")
        self.chk_show_clusters.setChecked(False)
        self.chk_show_clusters.stateChanged.connect(self.push_view_settings)
        hbox_switches.addWidget(self.chk_show_clusters)

        # Clustering Algorithm Dropdown
        lbl_cluster_alg = QtWidgets.QLabel("Clustering Algorithm:")
        hbox_switches.addWidget(lbl_cluster_alg)
        self.combo_clustering_alg = QtWidgets.QComboBox()
        self.combo_clustering_alg.addItem("Watershed", 1)
        self.combo_clustering_alg.addItem("K-Means", 2)
        self.combo_clustering_alg.addItem("DBSCAN", 3)
        self.combo_clustering_alg.addItem("Auto K-Means", 4)
        self.combo_clustering_alg.currentIndexChanged.connect(
            self.on_clustering_algorithm_changed)
        hbox_switches.addWidget(self.combo_clustering_alg)

        self.spin_kmeans_k = QtWidgets.QSpinBox()
        self.spin_kmeans_k.setRange(1, 100)
        self.spin_kmeans_k.setValue(5)
        self.spin_kmeans_k.setPrefix("k: ")
        self.spin_kmeans_k.setVisible(False)
        self.spin_kmeans_k.valueChanged.connect(self.on_kmeans_k_changed)
        hbox_switches.addWidget(self.spin_kmeans_k)


        self.spin_dbscan_eps = QtWidgets.QDoubleSpinBox()
        self.spin_dbscan_eps.setRange(0.1, 100.0)
        self.spin_dbscan_eps.setSingleStep(0.1)
        self.spin_dbscan_eps.setValue(3.0)
        self.spin_dbscan_eps.setPrefix("eps: ")
        self.spin_dbscan_eps.setVisible(False)
        self.spin_dbscan_eps.valueChanged.connect(self.on_dbscan_eps_changed)
        hbox_switches.addWidget(self.spin_dbscan_eps)

        self.spin_dbscan_minpts = QtWidgets.QSpinBox()
        self.spin_dbscan_minpts.setRange(1, 100)
        self.spin_dbscan_minpts.setValue(10)
        self.spin_dbscan_minpts.setPrefix("minPts: ")
        self.spin_dbscan_minpts.setVisible(False)
        self.spin_dbscan_minpts.valueChanged.connect(self.on_dbscan_minpts_changed)
        hbox_switches.addWidget(self.spin_dbscan_minpts)

        # Step-by-Step Debug Mode
        self.chk_step_debug = QtWidgets.QCheckBox("Step-by-Step Debug Mode")
        self.chk_step_debug.setChecked(False)
        hbox_switches.addWidget(self.chk_step_debug)
        
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

        # Plot Name (Optional)
        hbox_name = QtWidgets.QHBoxLayout()
        self.edit_plot_name = QtWidgets.QLineEdit()
        self.edit_plot_name.setPlaceholderText("Custom Plot Name (Optional)")
        hbox_name.addWidget(QtWidgets.QLabel("Name:"))
        hbox_name.addWidget(self.edit_plot_name)
        plot_layout.addLayout(hbox_name)

        # --- Axis Type Selection ---
        hbox_axis = QtWidgets.QHBoxLayout()
        hbox_axis.addWidget(QtWidgets.QLabel("Plot Type:"))
        self.combo_plot_type = QtWidgets.QComboBox()
        self.combo_plot_type.addItems(["Time Series", "Bucket (Demographic)", "Count (Condition)", "Dual Axis"])
        self.combo_plot_type.currentIndexChanged.connect(self._on_plot_type_changed)
        hbox_axis.addWidget(self.combo_plot_type)
        hbox_axis.addStretch()
        plot_layout.addLayout(hbox_axis)

        # --- Variable items per source type ---
        self._agent_var_items = [
            "age", "resources", "children", "is_pregnant",
            "avg_resources", "ux", "uy", "is_dead",
            "population", "gender", "cluster_rank", "creativity"
        ]
        self._cluster_var_items = [
            "n_agents", "n_cells", "MC_cl", "MC_cl_AV", "hep_sum",
            "k_fertility", "phi_death_acc", "phi_birth_acc", "n_alive_acc", "avg_creativity"
        ]
        self._global_var_items = [
            "agent_count", "avg_ms_per_tick",
            "k_fertility", "phi_death_acc", "phi_birth_acc", "n_alive_acc", "avg_creativity",
            "death_natural", "death_starvation", "death_oob",
            "death_conflict", "death_random",
            "perf_permanent", "perf_active", "perf_compaction",
            "perf_grid_density", "perf_clustering", "perf_total",
            "perf_active_modules"
        ]

        # --- Series Configuration Area (holds 1 or 2 columns) ---
        self.series_container = QtWidgets.QWidget()
        self.series_hlayout = QtWidgets.QHBoxLayout(self.series_container)
        self.series_hlayout.setContentsMargins(0, 0, 0, 0)

        # Build two series panels (Series A and optional Series B)
        self.series_a = self._build_series_panel("Series A (Left Y)")
        self.series_b = self._build_series_panel("Series B (Right Y)")
        
        self.series_hlayout.addWidget(self.series_a['group'])
        self.series_hlayout.addWidget(self.series_b['group'])

        plot_layout.addWidget(self.series_container)

        # --- Count-specific widgets (only visible for Count type) ---
        self.count_widgets_box = QtWidgets.QGroupBox("Count Condition")
        count_layout = QtWidgets.QHBoxLayout()
        self.combo_op = QtWidgets.QComboBox()
        self.combo_op.addItems(["==", "<=", ">=", "<", ">", "!="])
        self.edit_cond_val = QtWidgets.QLineEdit()
        self.edit_cond_val.setPlaceholderText("Val")
        self.edit_cond_val.setFixedWidth(60)
        count_layout.addWidget(QtWidgets.QLabel("Operator:"))
        count_layout.addWidget(self.combo_op)
        count_layout.addWidget(QtWidgets.QLabel("Value:"))
        count_layout.addWidget(self.edit_cond_val)
        count_layout.addStretch()
        self.count_widgets_box.setLayout(count_layout)
        plot_layout.addWidget(self.count_widgets_box)

        # --- Add / Manage Buttons ---
        hbox_add = QtWidgets.QHBoxLayout()
        btn_add_plot = QtWidgets.QPushButton("Add Plot")
        btn_add_plot.clicked.connect(self.add_plot)
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
        self._on_plot_type_changed()

        layout.addStretch()

        
        # Init Config Structure
        self.plot_config = {'update_freq': 10, 'plots': []}

    def _build_series_panel(self, title):
        """Build a single series configuration panel (source → filter → variable → agg)."""
        group = QtWidgets.QGroupBox(title)
        layout = QtWidgets.QVBoxLayout()

        # 1. Source selection
        source_layout = QtWidgets.QHBoxLayout()
        source_layout.addWidget(QtWidgets.QLabel("Source:"))
        combo_source = QtWidgets.QComboBox()
        combo_source.addItems(["Agents", "Clusters", "Global"])
        source_layout.addWidget(combo_source)
        source_layout.addStretch()
        layout.addLayout(source_layout)

        # 2. Filter (dynamic, depends on source)
        filter_box = QtWidgets.QGroupBox("Filter")
        filter_main_layout = QtWidgets.QVBoxLayout()
        filter_row_layout = QtWidgets.QHBoxLayout()
        
        combo_filter_var = QtWidgets.QComboBox()
        combo_filter_var.addItem("None")
        combo_filter_var.addItems(["population", "gender", "cluster_rank"])
        spin_filter_val = QtWidgets.QSpinBox()
        spin_filter_val.setRange(0, 100)
        spin_filter_val.setPrefix("= ")
        spin_population = QtWidgets.QSpinBox()
        spin_population.setRange(-2, 100)
        spin_population.setPrefix("pop: ")
        spin_population.setToolTip("-2 = Weighted Mean (n_alive_acc)\n-1 = Dominant (Most Agents)\n0 = All Populations\n1+ = Specific Population")
        spin_population.setSpecialValueText("W.Mean")
        lbl_pop_hint = QtWidgets.QLabel("(pop: W.Mean = weighted avg, -1 = dominant, 0 = all, 1+ = specific)")
        lbl_pop_hint.setStyleSheet("color: gray; font-size: 11px; font-style: italic;")
        
        filter_row_layout.addWidget(QtWidgets.QLabel("By:"))
        filter_row_layout.addWidget(combo_filter_var)
        filter_row_layout.addWidget(spin_filter_val)
        filter_row_layout.addWidget(spin_population)
        
        filter_main_layout.addLayout(filter_row_layout)
        filter_main_layout.addWidget(lbl_pop_hint)
        filter_box.setLayout(filter_main_layout)
        layout.addWidget(filter_box)

        # 3. Variable selection
        var_layout = QtWidgets.QHBoxLayout()
        var_layout.addWidget(QtWidgets.QLabel("Variable:"))
        combo_var = QtWidgets.QComboBox()
        combo_var.addItems(self._agent_var_items)  # Default: agents
        var_layout.addWidget(combo_var)
        layout.addLayout(var_layout)

        # 4. Aggregation
        agg_layout = QtWidgets.QHBoxLayout()
        agg_layout.addWidget(QtWidgets.QLabel("Aggregation:"))
        combo_agg = QtWidgets.QComboBox()
        combo_agg.addItems(["mean", "sum", "min", "max"])
        agg_layout.addWidget(combo_agg)
        agg_layout.addStretch()
        layout.addLayout(agg_layout)

        layout.addStretch()
        group.setLayout(layout)

        panel = {
            'group': group,
            'combo_source': combo_source,
            'filter_box': filter_box,
            'combo_filter_var': combo_filter_var,
            'spin_filter_val': spin_filter_val,
            'spin_population': spin_population,
            'lbl_pop_hint': lbl_pop_hint,
            'combo_var': combo_var,
            'combo_agg': combo_agg,
        }

        # Connect source change to dynamic updates
        combo_source.currentIndexChanged.connect(lambda idx, p=panel: self._on_source_changed(p))

        return panel

    def _on_source_changed(self, panel):
        """Update filter and variable options based on the selected source."""
        source = panel['combo_source'].currentText()

        # Update variable list
        panel['combo_var'].clear()
        if source == "Agents":
            panel['combo_var'].addItems(self._agent_var_items)
        elif source == "Clusters":
            panel['combo_var'].addItems(self._cluster_var_items)
        elif source == "Global":
            panel['combo_var'].addItems(self._global_var_items)

        # Update filter options
        panel['combo_filter_var'].clear()
        if source == "Agents":
            panel['combo_filter_var'].addItem("None")
            panel['combo_filter_var'].addItems(["population", "gender", "cluster_rank"])
            panel['combo_filter_var'].setVisible(True)
            panel['spin_filter_val'].setVisible(True)
            panel['spin_population'].setVisible(False)
            panel['lbl_pop_hint'].setVisible(False)
            panel['filter_box'].setVisible(True)
            panel['combo_agg'].setVisible(True)
        elif source == "Clusters":
            panel['combo_filter_var'].clear()
            panel['combo_filter_var'].addItem("cluster_rank")
            panel['spin_filter_val'].setValue(1)
            panel['combo_filter_var'].setVisible(True)
            panel['spin_filter_val'].setVisible(True)
            panel['spin_population'].setVisible(True)
            panel['lbl_pop_hint'].setVisible(True)
            panel['filter_box'].setVisible(True)
            panel['combo_agg'].setVisible(False)  # Cluster vars are already scalar per cluster
        elif source == "Global":
            panel['combo_filter_var'].clear()
            panel['combo_filter_var'].addItem("All / Pop")
            panel['combo_filter_var'].setVisible(False)
            panel['spin_filter_val'].setVisible(False)
            panel['spin_population'].setVisible(True)
            panel['lbl_pop_hint'].setVisible(True)
            panel['filter_box'].setVisible(True)
            panel['combo_agg'].setVisible(False)  # Global vars are already scalar

    def _on_plot_type_changed(self):
        """Show/hide series panels and count widgets based on plot type."""
        ptype = self.combo_plot_type.currentIndex()  # 0=TimeSeries, 1=Bucket, 2=Count, 3=DualAxis

        # Series B only visible for Dual Axis
        self.series_b['group'].setVisible(ptype == 3)

        # Count condition widgets only visible for Count type
        self.count_widgets_box.setVisible(ptype == 2)

        # For Bucket: aggregation doesn't apply
        if ptype == 1:
            self.series_a['combo_agg'].setVisible(False)
        else:
            # Re-trigger source-aware visibility
            self._on_source_changed(self.series_a)

    def _read_series_config(self, panel):
        """Read the current state of a series panel into a dict."""
        source = panel['combo_source'].currentText().lower()  # agents, clusters, global
        variable = panel['combo_var'].currentText()
        agg = panel['combo_agg'].currentText()

        filter_var = None
        filter_val = 0
        population = 0
        if panel['filter_box'].isVisible():
            if panel['combo_filter_var'].isVisible():
                fvar = panel['combo_filter_var'].currentText()
                if fvar != "None":
                    filter_var = fvar
                    filter_val = panel['spin_filter_val'].value()
            if panel['spin_population'].isVisible():
                population = panel['spin_population'].value()

        return {
            'source': source,
            'variable': variable,
            'aggregation': agg,
            'filter_var': filter_var,
            'filter_val': filter_val,
            'population': population,
        }

    def add_plot(self):
        idx = self.combo_plot_type.currentIndex()
        if idx == 0: ptype = "timeseries"
        elif idx == 1: ptype = "bucket"
        elif idx == 2: ptype = "count"
        else: ptype = "dualaxis"

        # Read series A config
        sa = self._read_series_config(self.series_a)

        # Build title
        custom_name = self.edit_plot_name.text().strip()

        if ptype == 'dualaxis':
            sb = self._read_series_config(self.series_b)
            if custom_name:
                title = f"{custom_name}  [{sa['source']}/{sa['variable']}] vs [{sb['source']}/{sb['variable']}]"
            else:
                title = f"Dual: {sa['source']}/{sa['variable']} vs {sb['source']}/{sb['variable']}"
        else:
            sb = None
            if custom_name:
                title = f"{custom_name}  [{ptype.title()}: {sa['source']}/{sa['variable']}]"
            else:
                title = f"{ptype.title()}: {sa['source']}/{sa['variable']}"
                if sa['filter_var']:
                    title += f" [filter: {sa['filter_var']}={sa['filter_val']}]"
                if ptype == 'timeseries':
                    title += f" [{sa['aggregation']}]"

        # Build pdef
        pdef = {
            'type': ptype,
            'title': title,
            'series_a': sa,
        }

        if ptype == 'dualaxis':
            pdef['series_b'] = sb

        if ptype == 'bucket':
            pdef['buckets'] = 20
            if sa['variable'] == 'gender':
                QtWidgets.QMessageBox.warning(self, "Invalid Plot", "Cannot make bucket plot of gender.")
                return

        if ptype == 'count':
            pdef['operator'] = self.combo_op.currentText()
            cval = self.edit_cond_val.text()
            pdef['condition_val'] = cval if cval else "0"

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
        settings['show_perf'] = self.chk_show_perf.isChecked()
        settings['show_clusters'] = self.chk_show_clusters.isChecked()
        settings['debug_mode'] = self.chk_step_debug.isChecked()
        settings['clustering_algorithm'] = self.combo_clustering_alg.currentData()
        
        return settings

    def on_clustering_algorithm_changed(self, index):
        """Called when the user selects a different clustering algorithm."""
        alg_id = self.combo_clustering_alg.currentData()
        self.spin_kmeans_k.setVisible(alg_id == 2)
        self.spin_dbscan_eps.setVisible(alg_id == 3)
        self.spin_dbscan_minpts.setVisible(alg_id == 3)
        try:
            import mod_python_interface
            if hasattr(mod_python_interface, 'set_clustering_algorithm'):
                mod_python_interface.set_clustering_algorithm(alg_id)
                print(f"Clustering algorithm set to: {self.combo_clustering_alg.currentText()} (ID={alg_id})")
        except Exception as e:
            print(f"Note: Could not set clustering algorithm (simulation may not be initialized): {e}")

    def on_kmeans_k_changed(self, value):
        try:
            import mod_python_interface
            if hasattr(mod_python_interface, 'set_kmeans_clusters'):
                mod_python_interface.set_kmeans_clusters(value)
                print(f"K-Means k set to: {value}")
        except Exception as e:
            print(f"Note: Could not set K-Means k: {e}")


    def on_dbscan_eps_changed(self, value):
        try:
            import mod_python_interface
            if hasattr(mod_python_interface, 'set_dbscan_eps'):
                mod_python_interface.set_dbscan_eps(value)
                print(f"DBSCAN eps set to: {value}")
        except Exception as e:
            print(f"Note: Could not set DBSCAN eps: {e}")

    def on_dbscan_minpts_changed(self, value):
        try:
            import mod_python_interface
            if hasattr(mod_python_interface, 'set_dbscan_minpts'):
                mod_python_interface.set_dbscan_minpts(value)
                print(f"DBSCAN minPts set to: {value}")
        except Exception as e:
            print(f"Note: Could not set DBSCAN minPts: {e}")

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
        
        self.spin_save_interval = QtWidgets.QSpinBox()
        self.spin_save_interval.setRange(0, 9999999)
        self.spin_save_interval.setValue(100)
        form_layout.addRow("Data Save Interval (Ticks):", self.spin_save_interval)
        
        self.spin_num_sims = QtWidgets.QSpinBox()
        self.spin_num_sims.setRange(1, 100)
        self.spin_num_sims.setValue(1)
        form_layout.addRow("Total Simulations to Run:", self.spin_num_sims)
        
        # Store Dead Agents Checkbox
        self.chk_store_dead_agents = QtWidgets.QCheckBox("Store dead agents (exports to NetCDF)")
        self.chk_store_dead_agents.setChecked(False)
        self.chk_store_dead_agents.setToolTip(
            "When enabled, all agents that die during the simulation will be " 
            "archived and written to a NetCDF file alongside the grid data. "
            "Uses the same Data Save Interval for extraction."
        )
        form_layout.addRow("", self.chk_store_dead_agents)
        
        # Store Grid Data Checkbox
        self.chk_store_grid_data = QtWidgets.QCheckBox("Store grid data (exports to NetCDF)")
        self.chk_store_grid_data.setChecked(True)
        self.chk_store_grid_data.setToolTip(
            "When enabled, grid density and environmental data will be "
            "written to a NetCDF file at the specified Save Interval."
        )
        form_layout.addRow("", self.chk_store_grid_data)
        
        # Output Path
        self.le_output_path = QtWidgets.QLineEdit("output/test.gif")
        self.btn_browse_output = QtWidgets.QPushButton("...")
        self.btn_browse_output.setFixedWidth(30)
        self.btn_browse_output.clicked.connect(self.browse_output_path)
        
        out_layout = QtWidgets.QHBoxLayout()
        out_layout.addWidget(self.le_output_path)
        out_layout.addWidget(self.btn_browse_output)
        
        form_layout.addRow("Output File Base/GIF:", out_layout)
        
        layout.addLayout(form_layout)
        
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
            show_selectable_error(self, "Error", f"Failed to save config: {e}")

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
                show_selectable_error(self, "Error", "basic_config.nml template not found.")
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
                show_selectable_error(self, "Error", f"Failed to create config: {e}")

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
        success = self.prepare_simulation()
        if not success:
            return
        
        # Launch Simulation Window
        if self.sim_window is not None:
            self.sim_window.close()
            self.sim_window.deleteLater()
            self.sim_window = None
            QtWidgets.QApplication.processEvents()
        
        if self.rb_view_3d.isChecked():
            view_mode = '3d'
        elif self.rb_view_graphs.isChecked():
            view_mode = 'graphs_only'
        else:
            view_mode = '2d'
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
        save_interval = self.spin_save_interval.value()
        
        if end_year <= start_year:
            QtWidgets.QMessageBox.warning(self, "Warning", "End time must be greater than start time.")
            return
            
        # Guarantee mathematical parity with Live View initialization
        if not self.prepare_simulation(self.btn_run_full):
            return
            
        num_sims = self.spin_num_sims.value()
        output_path = self.le_output_path.text().strip()
        
        # Automatically redirect Desktop output paths to the project root 'output/' folder
        if "Desktop" in output_path or not output_path:
            output_path = "output/test.gif"
            self.le_output_path.setText(output_path)
            
        store_dead_agents = self.chk_store_dead_agents.isChecked()
        store_grid_data = self.chk_store_grid_data.isChecked()
        
        if num_sims > 1:
            # Multi-simulation parallel run
            modules = self.spawn_editor.get_module_configuration()
            if modules is None:
                modules = []
            spawn_points = self.spawn_editor.get_spawn_points()
            age_dist = self.spawn_editor.get_age_distribution()
            clustering_alg = self.combo_clustering_alg.currentData()
            kmeans_k = self.spin_kmeans_k.value()
            dbscan_eps = self.spin_dbscan_eps.value()
            dbscan_minpts = self.spin_dbscan_minpts.value()
            
            self.full_sim_window = MultiSimulationWindow(
                num_sims, start_year, end_year, save_interval, output_path, store_dead_agents, store_grid_data,
                config_path, hep_paths, modules, spawn_points, age_dist,
                clustering_alg, kmeans_k, dbscan_eps, dbscan_minpts, self.current_npops
            )
            self.full_sim_window.show()
        else:
            # Guarantee mathematical parity with Live View initialization for single run
            if not self.prepare_simulation(self.btn_run_full):
                return
            
            # Launch standalone Full Simulation Window
            self.full_sim_window = FullSimulationWindow(start_year, end_year, save_interval, output_path, store_dead_agents, store_grid_data)
            self.full_sim_window.show()

    def run_test_simulation(self):
        """Open the Test Simulation config dialog and launch the sweep suite."""
        config_path = self.get_selected_config_path()
        hep_paths = self.get_hep_paths()
        if not config_path or not hep_paths:
            QtWidgets.QMessageBox.warning(self, "Warning", "Please select configuration and HEP files.")
            return

        start_year = self.spin_start_year.value()
        end_year = self.spin_end_year.value()
        save_interval = self.spin_save_interval.value()

        if end_year <= start_year:
            QtWidgets.QMessageBox.warning(self, "Warning", "End time must be greater than start time.")
            return

        output_path = self.le_output_path.text().strip()
        if "Desktop" in output_path or not output_path:
            output_path = "output/test.gif"
            self.le_output_path.setText(output_path)

        store_dead_agents = self.chk_store_dead_agents.isChecked()
        store_grid_data = self.chk_store_grid_data.isChecked()

        # Open config dialog
        dialog = TestSimulationConfigDialog(start_year, end_year, save_interval, store_dead_agents, store_grid_data, self)
        if dialog.exec_() != QtWidgets.QDialog.Accepted or dialog.result_configs is None:
            return

        run_configs = dialog.result_configs
        start_year = dialog.start_year
        end_year = dialog.end_year
        save_interval = dialog.save_interval
        store_dead_agents = dialog.store_dead_agents
        store_grid_data = dialog.store_grid_data
        config_path = getattr(dialog, 'config_path', config_path)

        # Parse target_npops from the selected configuration file
        target_npops = self.current_npops
        if config_path and os.path.exists(config_path):
            try:
                with open(config_path, 'r') as f:
                    content = f.read()
                for line in content.splitlines():
                    if 'npops' in line.lower():
                        parts = line.split('=')
                        if len(parts) > 1:
                            val = parts[1].strip().split(',')[0].strip()
                            target_npops = int(val)
            except Exception as e:
                print(f"Warning: Failed to parse npops from config {config_path}: {e}")

        # Adjust hep_paths to match target_npops
        if hep_paths:
            if len(hep_paths) == 1:
                hep_paths = [hep_paths[0]] * target_npops
            elif len(hep_paths) < target_npops:
                hep_paths = hep_paths + [hep_paths[-1]] * (target_npops - len(hep_paths))
            elif len(hep_paths) > target_npops:
                hep_paths = hep_paths[:target_npops]

        # Gather shared parameters
        spawn_points = self.spawn_editor.get_spawn_points()
        age_dist = self.spawn_editor.get_age_distribution()
        clustering_alg = self.combo_clustering_alg.currentData()
        kmeans_k = self.spin_kmeans_k.value()
        dbscan_eps = self.spin_dbscan_eps.value()
        dbscan_minpts = self.spin_dbscan_minpts.value()

        self.test_sim_window = TestSimulationSuiteWindow(
            run_configs, start_year, end_year, save_interval,
            dialog.output_folder, store_dead_agents, store_grid_data,
            config_path, hep_paths, spawn_points, age_dist,
            clustering_alg, kmeans_k, dbscan_eps, dbscan_minpts, target_npops,
            getattr(dialog, 'ipc_interval', 10),
            getattr(dialog, 'dead_export_interval', 500),
            getattr(dialog, 'dead_export_threshold', 1000)
        )
        self.test_sim_window.show()


    def update_button_progress(self, button, percent, text, color="green"):
        # Create a linear gradient background
        # color at 0% to percent%, then standard gray/white
        if percent >= 100:
             button.setStyleSheet("")
             button.setText(text) # Should probably restore original text usually, but caller can do that
             return

        # QSS Linear Gradient
        # We need a clean way.
        # color: #90EE90 (LightGreen) or #FFaaaa (LightRed) 
        c_code = "#90EE90" if color == "green" else "#ff6666"
        
        style = f"""
            QPushButton {{
                background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0, 
                                                  stop:0 {c_code}, stop:{percent/100.0} {c_code}, 
                                                  stop:{percent/100.0+0.001} #e1e1e1, stop:1 #e1e1e1);
                border: 1px solid #777;
                border-radius: 4px;
                padding: 4px;
                color: black;
            }}
        """
        button.setStyleSheet(style)
        button.setText(f"{text} ({int(percent)}%)")
        QtWidgets.QApplication.processEvents()

    def prepare_simulation(self, target_button=None):
        if target_button is None:
            target_button = self.btn_run_live
            
        final_text = "Live View" if target_button == self.btn_run_live else "Run Full Simulation"
            
        config_path = self.get_selected_config_path()
        if not config_path:
            QtWidgets.QMessageBox.warning(self, "Warning", "Please select a configuration.")
            return False

        hep_paths = self.get_hep_paths()
        if not hep_paths:
             QtWidgets.QMessageBox.warning(self, "Warning", "Please select HEP file(s).")
             return False

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
                # Initialize simulation step-by-step
                
                # Step 1: Init World (10%)
                self.update_button_progress(target_button, 10, "Initializing World", "green")
                time.sleep(0.01)
                mod_python_interface.init_sim_step_1()
                
                # Step 2: Setup World (Config/Grid)
                self.update_button_progress(target_button, 15, "Reading Config", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_1()

                self.update_button_progress(target_button, 25, "Allocating Grid", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_2_arrays_only()
                
                # Granular Grid Init
                nx = mod_python_interface.get_grid_nx()
                chunk_size = 10
                for i in range(1, nx + 1, chunk_size):
                    end_sub = min(i + chunk_size - 1, nx)
                    mod_python_interface.init_sim_step_2_part_2_chunk(i, end_sub)
                    
                    # Progress from 25% to 45%
                    progress = 25 + (20 * (end_sub / nx))
                    self.update_button_progress(target_button, progress, f"Initializing Grid ({int((end_sub/nx)*100)}%)", "green")
                    QtWidgets.QApplication.processEvents()

                self.update_button_progress(target_button, 45, "Loading Data", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_3()

                # Apply clustering algorithm from View Editor
                alg_id = self.combo_clustering_alg.currentData()
                if alg_id is not None:
                    mod_python_interface.set_clustering_algorithm(alg_id)
                
                # Overwrite with custom spawn configuration
                mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts, ns, npops)
                
                # Step 3: Generate Agents (Using new config) (replaces regenerate_agents calls essentially)
                self.update_button_progress(target_button, 60, "Generating Agents", "green")
                time.sleep(0.1)
                # We call init_sim_step_3(False) -> Generate
                mod_python_interface.init_sim_step_3(False)
                
                # Step Age Distribution
                age_dist = self.spawn_editor.get_age_distribution()
                if age_dist is not None:
                    self.update_button_progress(target_button, 75, "Applying Age Distribution", "green")
                    QtWidgets.QApplication.processEvents()
                    mod_python_interface.set_age_distribution_interface(age_dist, len(age_dist))
                    mod_python_interface.init_sim_step_apply_age_dist()
                
                # Step 4: Verify (90%)
                self.update_button_progress(target_button, 90, "Verifying", "green")
                time.sleep(0.1)
                mod_python_interface.init_sim_step_4()
                
                # Done
                self.update_button_progress(target_button, 100, final_text, "green")
                return True
                
            except Exception as e:
                self.update_button_progress(target_button, 100, final_text, "green") # Reset
                show_selectable_error(self, "Error", f"Failed to set spawn configuration: {e}")
                return False
        else:
            print("Using default spawn points from config.")
            try:
                # Default Init Steps
                target_button.setEnabled(False) # Prevent double click
                
                self.update_button_progress(target_button, 10, "Initializing World", "green")
                mod_python_interface.init_sim_step_1()
                
                self.update_button_progress(target_button, 15, "Reading Config", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_1()

                self.update_button_progress(target_button, 25, "Allocating Grid", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_2_arrays_only()
                
                # Granular Grid Init
                nx = mod_python_interface.get_grid_nx()
                chunk_size = 10
                for i in range(1, nx + 1, chunk_size):
                    end_sub = min(i + chunk_size - 1, nx)
                    mod_python_interface.init_sim_step_2_part_2_chunk(i, end_sub)
                    
                    # Progress from 25% to 45%
                    progress = 25 + (20 * (end_sub / nx))
                    self.update_button_progress(target_button, progress, f"Initializing Grid ({int((end_sub/nx)*100)}%)", "green")
                    QtWidgets.QApplication.processEvents()

                self.update_button_progress(target_button, 45, "Loading Data", "green")
                QtWidgets.QApplication.processEvents()
                mod_python_interface.init_sim_step_2_part_3()

                # Apply clustering algorithm from View Editor
                alg_id = self.combo_clustering_alg.currentData()
                if alg_id is not None:
                    mod_python_interface.set_clustering_algorithm(alg_id)
                if alg_id == 2 and hasattr(mod_python_interface, 'set_kmeans_clusters'):
                    mod_python_interface.set_kmeans_clusters(self.spin_kmeans_k.value())
                if alg_id == 3 and hasattr(mod_python_interface, 'set_dbscan_eps'):
                    mod_python_interface.set_dbscan_eps(self.spin_dbscan_eps.value())
                if alg_id == 3 and hasattr(mod_python_interface, 'set_dbscan_minpts'):
                    mod_python_interface.set_dbscan_minpts(self.spin_dbscan_minpts.value())
                
                self.update_button_progress(target_button, 60, "Process Agents", "green")
                # Step 3: Generate (default)
                mod_python_interface.init_sim_step_3(False)
                
                # Step Age Distribution
                age_dist = self.spawn_editor.get_age_distribution()
                if age_dist is not None:
                    self.update_button_progress(target_button, 75, "Applying Age Distribution", "green")
                    QtWidgets.QApplication.processEvents()
                    mod_python_interface.set_age_distribution_interface(age_dist, len(age_dist))
                    mod_python_interface.init_sim_step_apply_age_dist()
                
                self.update_button_progress(target_button, 90, "Verifying", "green")
                mod_python_interface.init_sim_step_4()
                
                self.update_button_progress(target_button, 100, final_text, "green")
                target_button.setEnabled(True)
                return True
                
            except Exception as e:
                target_button.setEnabled(True)
                self.update_button_progress(target_button, 100, final_text, "green")
                show_selectable_error(self, "Error", f"Initialization failed: {e}")
                return False

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

            # Restore Age Distribution Config
            age_dist_cfg = state.get('age_dist_config')
            if age_dist_cfg:
                self.spawn_editor.set_age_dist_config(age_dist_cfg)
                
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
            elif view_mode == 'graphs_only':
                self.rb_view_graphs.setChecked(True)
            else:
                self.rb_view_2d.setChecked(True)
                
            # Restore View Colors
            view_colors = state.get('view_colors', {})
            for key, val in view_colors.items():
                if key in self.color_buttons:
                    self.set_button_color(self.color_buttons[key], tuple(val))
                    
            # Restore clustering view options
            if 'show_clusters' in state:
                self.chk_show_clusters.setChecked(state['show_clusters'])
            
            if 'clustering_alg_index' in state:
                idx = state['clustering_alg_index']
                if 0 <= idx < self.combo_clustering_alg.count():
                    self.combo_clustering_alg.setCurrentIndex(idx)
            
            # Restore Show Performance Metrics
            if 'show_perf' in state:
                self.chk_show_perf.setChecked(state['show_perf'])

            # Restore Plot Config
            if 'plot_config' in state:
                self.plot_config = state['plot_config']
                # Restore UI
                self.spin_update_freq.setValue(self.plot_config.get('update_freq', 1))
                self.list_plots.clear()
                for p in self.plot_config.get('plots', []):
                    self.list_plots.addItem(p['title'])
            
            # Restore Test Simulation Config
            self.test_sim_state = state.get('test_sim', {})
                    
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

        # Save Age Distribution Config
        state['age_dist_config'] = self.spawn_editor.get_age_dist_config()
        
        # Save Full Sim Config
        state['full_sim'] = {
            'start_year': self.spin_start_year.value(),
            'end_year': self.spin_end_year.value(),
            'output_path': self.le_output_path.text()
        }
        
        # Save View Mode
        if self.rb_view_3d.isChecked():
            state['view_mode'] = '3d'
        elif self.rb_view_graphs.isChecked():
            state['view_mode'] = 'graphs_only'
        else:
            state['view_mode'] = '2d'
        
        # Save View Colors
        colors = {}
        for key, btn in self.color_buttons.items():
            colors[key] = btn.property('color')
        state['view_colors'] = colors
        
        # Save Clustering settings
        state['show_clusters'] = self.chk_show_clusters.isChecked()
        alg_index = self.combo_clustering_alg.currentIndex()
        if alg_index >= 0:
            state['clustering_alg_index'] = alg_index
            
        # Save Show Performance Metrics
        state['show_perf'] = self.chk_show_perf.isChecked()

        # Save Plot Config
        state['plot_config'] = self.plot_config
        
        # Save Test Simulation state
        state['test_sim'] = getattr(self, 'test_sim_state', {})
        
        try:
            with open(self.session_file, 'w') as f:
                json.dump(state, f, indent=4)
        except Exception as e:
            print(f"Failed to save session: {e}")

    def closeEvent(self, event):
        self.save_session()
        event.accept()

if __name__ == '__main__':
    QtWidgets.QApplication.setAttribute(QtCore.Qt.AA_ShareOpenGLContexts)
    app = QtWidgets.QApplication(sys.argv)
    app.setStyle("Fusion")
    window = MainApplication()
    window.show()
    sys.exit(app.exec_())
