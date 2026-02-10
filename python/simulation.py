import netCDF4
import sys
import os
import numpy as np

# --- OpenGL Context Monkeypatch ---
# Fix for "Attempt to retrieve context when no valid context" error
import OpenGL.contextdata
import OpenGL.error

_original_getContext = OpenGL.contextdata.getContext

def safe_getContext(context=None):
    try:
        return _original_getContext(context)
    except OpenGL.error.Error:
        return 0

OpenGL.contextdata.getContext = safe_getContext
# ----------------------------------

import pyqtgraph as pg
import pyqtgraph.opengl as gl
from pyqtgraph.Qt import QtCore, QtWidgets, QtGui
import OpenGL.GL as GL # Explicit import
import time

# Add the parent directory to sys.path to find the compiled module
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    # Fix for f2py module nesting
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Error importing mod_python_interface: {e}")
    print("Make sure the extension is built and in the parent directory.")
    sys.exit(1)

class SimulationWindow(QtWidgets.QMainWindow):
    def __init__(self, skip_init=False, view_mode='2d', view_settings=None):
        super().__init__()
        self.setWindowTitle("HEP Simulation")
        self.resize(1600, 900) # Increased size for plots

        # View Settings (Colors)
        if view_settings:
            self.view_settings = view_settings
        else:
            self.view_settings = {
                'bg_water': (0.0, 0.0, 0.0, 1.0),
                'bg_land': (0.5, 0.5, 0.5, 1.0),
                'hep_water': (0.1, 0.2, 0.8, 1.0),
                'hep_low': (0.5, 0.5, 0.5, 1.0),
                'hep_high': (0.2, 0.8, 0.2, 1.0)
            }
        
        # Plot Settings
        self.plot_config = {
            'update_freq': 1, # Update every N ticks
            'plots': []       # List of plot definitions
        }
        self.plot_data_history = {} # Store history for time series

        # Store background data to allowing recoloring
        self.bg_faces = None
        self.bg_verts = None
        self.bg_vals_flat = None
        self.bg_sphere = None

        # Central Widget and Layout
        # We need a splitter now to separate Visualization and Plots
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.main_layout = QtWidgets.QHBoxLayout(self.central_widget)
        
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Horizontal)
        self.main_layout.addWidget(self.splitter)

        # Left: Visualization Container
        self.viz_container = QtWidgets.QWidget()
        self.viz_widget = self.viz_container # Alias for use in setup_viz_plots
        self.viz_layout = QtWidgets.QVBoxLayout(self.viz_container)
        self.viz_layout.setContentsMargins(0,0,0,0)
        self.splitter.addWidget(self.viz_container)
        
        # Right: Plots Container (Scrollable)
        self.plots_scroll = QtWidgets.QScrollArea()
        self.plots_scroll.setWidgetResizable(True)
        self.plots_container = QtWidgets.QWidget()
        self.plots_layout = QtWidgets.QVBoxLayout(self.plots_container)
        self.plots_scroll.setWidget(self.plots_container)
        
        self.splitter.addWidget(self.plots_scroll)
        self.splitter.setSizes([900, 300]) # Initial split

        # Initialize Simulation
        if not skip_init:
            print("Initializing simulation...")
            mod_python_interface.init_simulation()
        else:
            print("Skipping initialization (assumed already initialized).")
        
        # Get Grid Dimensions
        self.dlon, self.dlat, self.npops = mod_python_interface.get_grid_dims()
        print(f"Grid Dimensions: {self.dlon}x{self.dlat}, Pops: {self.npops}")
        
        if self.dlon == 0 or self.dlat == 0:
            print("Error: Invalid grid dimensions (0x0). Initialization failed.")
            QtWidgets.QMessageBox.critical(self, "Error", "Simulation initialization failed (Invalid Grid). Check console for details.")
            # We delay close effectively or just dont setup viz?
            # If we call self.close() here it might be too early for event loop.
            # We can use QTimer.singleShot
            QtCore.QTimer.singleShot(0, self.close)
            self.running = False
            return

        # Get Simulation Config
        self.lon_0, self.lat_0, self.delta_lon, self.delta_lat, self.dlon_hep, self.dlat_hep = mod_python_interface.get_simulation_config()
        print(f"Config: lon_0={self.lon_0}, lat_0={self.lat_0}, dlon={self.delta_lon}, dlat={self.delta_lat}")

        # Setup Plots (Viz)
        self.view_mode = view_mode
        self.debug_mode = self.view_settings.get('debug_mode', False)
        self.setup_viz_layout() # Modified to separate layout setup
        self.setup_viz_plots()

        # Simulation State
        self.t = 0
        self.running = True
        self.steps_per_frame = 1

        # Timer
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.update_simulation)
        # self.timer.start(0) # Moved to showEvent

    def showEvent(self, event):
        super().showEvent(event)
        if not self.timer.isActive():
            self.timer.start(0)

    def closeEvent(self, event):
        print("Closing Simulation Window...")
        self.running = False
        if self.timer.isActive():
            self.timer.stop()
        
        # If we already cleaned up via abort sequence, skip.
        # But closeEvent is called by self.close() in abort_sequence AFTER cleanup.
        # So we need a flag.
        if getattr(self, 'cleanup_done', False):
            event.accept()
            return
            
        try:
            # Fallback monolithic cleanup if closed via X button
            mod_python_interface.cleanup_simulation()
            print("Simulation cleanup requested (fallback).")
        except Exception as e:
            print(f"Error during simulation cleanup: {e}")
            
        event.accept()
            
    def update_plot_config(self, config):
        self.plot_config = config
        self.setup_analysis_plots()

    def setup_analysis_plots(self):
        # Clear existing
        # Note: Ideally we reuse widgets, but for simplicity let's rebuild
        for i in reversed(range(self.plots_layout.count())): 
            self.plots_layout.itemAt(i).widget().setParent(None)
            
        self.active_plots = []
        
        for pdef in self.plot_config['plots']:
            ptype = pdef['type']
            title = pdef['title']
            
            # Create Plot Widget using PyQtGraph
            pw = pg.PlotWidget(title=title)
            pw.setMinimumHeight(200)
            self.plots_layout.addWidget(pw)
            
            plot_item = {
                'def': pdef,
                'widget': pw,
                'items': [] # Store graph items
            }
            
            if ptype == 'timeseries' or ptype == 'count':
                # Init history if needed
                key = f"{title}_{pdef['variable']}"
                if key not in self.plot_data_history:
                    from collections import deque
                    self.plot_data_history[key] = {
                        'x': deque(), 
                        'y': deque()
                    }
                
                curve = pw.plot(pen='y')
                plot_item['items'].append(curve)
                
            elif ptype == 'bucket':
                 # Demographic (Male/Female)
                 # Left: Female (Negative?), Right: Male (Positive)
                 # Or just stacked bars. The prompt says "left female and right male".
                 # Pyramid chart?
                 # Let's use BarGraphItem
                 bg = pg.BarGraphItem(x=[], height=[], width=0.8, brush='b')
                 pw.addItem(bg)
                 plot_item['items'].append(bg)

            self.active_plots.append(plot_item)
            
        self.plots_layout.addStretch()
        
    def setup_viz_layout(self):
        # 1. Visualization Widget (GLView or GraphicsLayout)
        if self.view_mode == '2d':
             self.glw = pg.GraphicsLayoutWidget()
             self.viz_layout.addWidget(self.glw)
        elif self.view_mode == '3d':
             self.gl_view = gl.GLViewWidget()
             self.viz_layout.addWidget(self.gl_view)
             
        # 2. Control Bar (Buttons)
        control_layout = QtWidgets.QHBoxLayout()
        
        self.btn_abort = QtWidgets.QPushButton("Abort Simulation")
        self.btn_abort.setStyleSheet("background-color: red; color: white; font-weight: bold;")
        self.btn_abort.clicked.connect(self.abort_sequence) # CALL ABORT SEQUENCE
        control_layout.addWidget(self.btn_abort)
        
        control_layout.addStretch()
        
        if self.debug_mode:
            self.btn_step = QtWidgets.QPushButton("Simulate Next Step")
            self.btn_step.setStyleSheet("background-color: blue; color: white; font-weight: bold;")
            self.btn_step.clicked.connect(self.manual_step)
            control_layout.addWidget(self.btn_step)

        # Show Clusters Checkbox
        self.cb_show_clusters = QtWidgets.QCheckBox("Show Clusters")
        self.cb_show_clusters.setStyleSheet("color: white; font-weight: bold;")
        # Set initial state from view_settings
        self.cb_show_clusters.setChecked(self.view_settings.get('show_clusters', False))
        self.cb_show_clusters.stateChanged.connect(self.update_visualization)
        control_layout.addWidget(self.cb_show_clusters)
            
        self.viz_layout.addLayout(control_layout)

    def update_button_progress(self, button, percent, text, color="green"):
        # Helper for progress bar button
        if percent >= 100:
             # We assume we are closing, so maybe don't even reset, but let's reset to be safe
             button.setStyleSheet("background-color: red; color: white; font-weight: bold;") 
             button.setText("Abort Simulation")
             return

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

    def abort_sequence(self):
        # Abort with progress
        self.running = False
        if self.timer.isActive():
            self.timer.stop()
            
        # Hide GL View to prevent painting during cleanup
        if hasattr(self, 'gl_view'):
            self.gl_view.setVisible(False)
            
        self.btn_abort.setEnabled(False)
        print("Starting abort sequence...")
        sys.stdout.flush()
        
        try:
             # Step 1: Cleanup Grid (Longest part probably)
             self.update_button_progress(self.btn_abort, 10, "Cleaning Grid", "red")
             time.sleep(0.2) # Force UI time
             mod_python_interface.cleanup_sim_step_1()
             
             # Step 2: Cleanup Agents
             self.update_button_progress(self.btn_abort, 60, "Cleaning Agents", "red")
             time.sleep(0.2)
             mod_python_interface.cleanup_sim_step_2()
             
             # Step 3: Finalize
             self.update_button_progress(self.btn_abort, 90, "Finalizing", "red")
             time.sleep(0.2)
             mod_python_interface.cleanup_sim_step_3()
             
             self.update_button_progress(self.btn_abort, 100, "Aborted", "red")
             time.sleep(0.2)
             
             print("Abort sequence complete.")
             sys.stdout.flush()
             self.cleanup_done = True
             self.close() # Close window
             
        except Exception as e:
            print(f"Error during abort: {e}")
            sys.stdout.flush()
            self.close()

    def manual_step(self):
        # Perform N ticks
        n_ticks = self.plot_config.get('update_freq', 10)
        for _ in range(n_ticks):
            self.t += 1
            mod_python_interface.step_simulation(self.t)
            
        self.update_visualization()

    def setup_viz_plots(self):
        # Persistent Debug Overlay - Parent to viz_widget to float over whatever view is active
        self.debug_label = QtWidgets.QLabel("", self.viz_widget)
        self.debug_label.setStyleSheet("QLabel { color: red; font-weight: bold; font-size: 14pt; background-color: rgba(0, 0, 0, 180); border: 1px solid white; padding: 10px; border-radius: 5px; }")
        self.debug_label.move(10, 10) 
        self.debug_label.hide() # Hidden until update loop showing it

        # Views are already added in setup_viz_layout, just configure them here
        if self.view_mode == '2d':
            self.setup_2d_view()
        elif self.view_mode == '3d':
            self.setup_3d_view()
            
    def setup_2d_view(self):
        # self.glw is created in setup_viz_layout
        
        # HEP Plot (Heatmap)
        self.plot_hep = self.glw.addPlot(title="HEP Density")
        
        # Enforce square grid cells
        if self.delta_lat != 0:
            ratio = self.delta_lon / self.delta_lat
            self.plot_hep.setAspectLocked(True, ratio=ratio)
        else:
            self.plot_hep.setAspectLocked(True)

        self.img_hep = pg.ImageItem()
        self.plot_hep.addItem(self.img_hep)
        
        # Set Image Transform
        tr = QtGui.QTransform()
        tr.translate(self.lon_0, self.lat_0)
        tr.scale(self.delta_lon, self.delta_lat)
        self.img_hep.setTransform(tr)
        
        # Colormap
        pos = np.array([0.0, 0.5, 1.0])
        color = np.array([
            [0, 0, 255, 255],      # Blue for -1
            [253, 253, 150, 255],  # Pastel Yellow for 0
            [119, 221, 119, 255]   # Pastel Green for 1
        ], dtype=np.ubyte)
        cmap = pg.ColorMap(pos, color)
        self.img_hep.setLookupTable(cmap.getLookupTable(0.0, 1.0, 256))
        self.img_hep.setLevels([-1, 1])

        # Agents Plot (Scatter)
        self.scatter_agents = pg.ScatterPlotItem(size=5, pen=pg.mkPen(None))
        self.plot_hep.addItem(self.scatter_agents)

        # Set ranges
        self.plot_hep.setXRange(self.lon_0, self.lon_0 + self.dlon_hep * self.delta_lon)
        self.plot_hep.setYRange(self.lat_0, self.lat_0 + self.dlat_hep * self.delta_lat)
        
        # Clusters Overlay (2D)
        self.img_clusters = pg.ImageItem()
        self.img_clusters.setZValue(10) # Draw on top of HEP (Z=0) and Agents? Agents are Scatter, usually Z=0.
        # Let's put clusters below agents but above HEP. Scatter default Z is 0?
        # Actually ImageItem draws at Z=0 by default.
        # Valid Z values?
        self.img_clusters.setZValue(5) 
        self.plot_hep.addItem(self.img_clusters)
        
        # Set Transform for clusters (same as HEP)
        self.img_clusters.setTransform(tr)
        
        # Population Colors
        self.setup_pop_colors()

    def setup_3d_view(self):
        # self.gl_view created in setup_viz_layout
        # Add to viz layout instead of setting central widget
        # self.viz_layout.addWidget(self.gl_view)
        
        # Set Camera
        self.gl_view.setCameraPosition(distance=40)
        
        # --- 1. Background Globe (Land/Water) ---
        self.setup_background_globe()
        # ----------------------------------------
        
        # Create Sphere for HEP (Active Simulation)
        # Generate custom mesh matching the grid
        self.md = self.generate_grid_mesh(radius=10.05) # Slightly above background (radius 10)
        
        # smooth=False for hard edges, shader=None for full brightness (no lighting)
        self.sphere_item = gl.GLMeshItem(meshdata=self.md, smooth=False, shader=None, glOptions='opaque')
        self.gl_view.addItem(self.sphere_item)
        
        # Create Scatter for Agents
        # glOptions='opaque' to ensure no blending/transparency artifacts
        self.scatter_3d = gl.GLScatterPlotItem(pos=np.zeros((1,3)), size=5, color=(1, 0, 0, 1), pxMode=True, glOptions='opaque')
        self.gl_view.addItem(self.scatter_3d)
        
        # Clusters Overlay (3D)
        # Re-use mesh data structure but with slightly larger radius to prevent Z-fighting
        self.md_clusters = self.generate_grid_mesh(radius=10.08) # Slightly above HEP (which is 10.05)
        
        # Translucent for alpha blending
        self.cluster_sphere = gl.GLMeshItem(meshdata=self.md_clusters, smooth=False, shader=None, glOptions='translucent')
        self.cluster_sphere.setVisible(False)
        self.gl_view.addItem(self.cluster_sphere)
        
        self.setup_pop_colors()


    def update_view_settings(self, settings):
        self.view_settings = settings
        print("Settings updated:", self.view_settings)
        
        # Sync UI Elements
        if 'show_clusters' in self.view_settings:
            self.cb_show_clusters.setChecked(self.view_settings['show_clusters'])

        # Refresh Background Colors
        self.refresh_background_colors()
             
        # Trigger visual update for active HEP
        # We can just force map_hep_to_colors to reuse the last data if we stored it, or wait for next frame.
        # But we didn't store active HEP values yet.
        # Let's force a single update cycle if running, or pull data if paused.
        self.update_visualization()

    def refresh_background_colors(self, first_load=False):
        if self.bg_vals_flat is None or self.bg_faces is None or self.bg_verts is None:
            return

        # Map to colors using Settings
        # Background Globe:
        # <=0 (Water) -> bg_water
        # >0 (Land)   -> bg_land
        
        vals = self.bg_vals_flat
        colors = np.zeros((len(vals), 4))
        
        mask_water = vals <= 0.01
        mask_land = vals > 0.01
        
        colors[mask_water] = self.view_settings.get('bg_water', (0,0,0,1))
        colors[mask_land] = self.view_settings.get('bg_land', (0.5,0.5,0.5,1))
        
        # Repeat for 2 faces per cell
        face_colors = np.repeat(colors, 2, axis=0)
        
        md = gl.MeshData(vertexes=self.bg_verts, faces=self.bg_faces, faceColors=face_colors)
        
        if first_load:
            self.bg_sphere = gl.GLMeshItem(meshdata=md, smooth=False, shader=None, glOptions='opaque')
            self.gl_view.addItem(self.bg_sphere)
            print("Background globe loaded.")
        else:
            if self.bg_sphere:
                self.bg_sphere.setMeshData(meshdata=md)

    def setup_background_globe(self):
        # Load Earth HEP for background
        hep_path = os.path.join(os.path.dirname(__file__), '..', 'input', 'hep', 'background.nc')
        if not os.path.exists(hep_path):
            print("Global HEP file not found. Skipping background globe.")
            return

        try:
            print("Loading background globe...")
            with netCDF4.Dataset(hep_path, 'r') as nc:
                # Read variables
                lats = nc.variables['lat'][:]
                lons = nc.variables['lon'][:]
                # AccHEP is (time, lat, lon)
                vals = nc.variables['AccHEP'][0, :, :] 
                
                # Downsample step for performance
                step = 10 # 0.15 * 10 = 1.5 deg resolution
                
                sub_lats = lats[::step]
                sub_lons = lons[::step]
                sub_vals = vals[::step, ::step]
                
                nlat = len(sub_lats)
                nlon = len(sub_lons)
                
                # Generate Vertices (lat/lon grid)
                lon_grid, lat_grid = np.meshgrid(sub_lons, sub_lats, indexing='ij')
                self.bg_verts = self.latlon_to_cartesian(lon_grid.flatten(), lat_grid.flatten(), radius=10.0)
                
                # Generate Faces
                faces = []
                # We iterate over cells (nlon-1, nlat-1)
                for i in range(nlon - 1):
                    for j in range(nlat - 1):
                        v00 = i * nlat + j
                        v10 = (i + 1) * nlat + j
                        v01 = i * nlat + (j + 1)
                        v11 = (i + 1) * nlat + (j + 1)
                        
                        faces.append([v00, v10, v01])
                        faces.append([v10, v11, v01])
                        
                # --- FIX: WRAPPING (New Zealand Gap) ---
                if abs((sub_lons[-1] - sub_lons[0]) - 360) > 1.0: 
                     # If we wrap, we need extra faces connecting last col to first col?
                     # My previous wrapping logic appended data to arrays. 
                     # But I need to do that BEFORE generating meshgrid/faces OR handle it here.
                     
                     # Re-apply the logic I wrote before: Append to sub_lons/vals
                     pass
                     # Wait, I cannot modify sub_lons easily after meshgrid.
                     # I should restart the logic.
                
                # RESTART LOGIC WITH WRAPPING PRE-CALCULATION
                if abs((sub_lons[-1] - sub_lons[0]) - 360) > 1.0:
                     sub_lons = np.append(sub_lons, sub_lons[0] + 360)
                     first_col = sub_vals[:, 0:1]
                     sub_vals = np.append(sub_vals, first_col, axis=1)
                     nlon += 1
                     
                # Re-Generate Vertices with wrapped data
                lon_grid, lat_grid = np.meshgrid(sub_lons, sub_lats, indexing='ij')
                self.bg_verts = self.latlon_to_cartesian(lon_grid.flatten(), lat_grid.flatten(), radius=10.0)
                
                # Re-Generate Faces
                faces = []
                for i in range(nlon - 1):
                    for j in range(nlat - 1):
                        v00 = i * nlat + j
                        v10 = (i + 1) * nlat + j
                        v01 = i * nlat + (j + 1)
                        v11 = (i + 1) * nlat + (j + 1)
                        
                        faces.append([v00, v10, v01])
                        faces.append([v10, v11, v01])

                self.bg_faces = np.array(faces)
                
                # Data for Colors
                cell_vals = sub_vals[:nlat-1, :nlon-1] 
                cell_vals_flat = cell_vals.T.flatten() 
                
                # Remap for Background Logic
                # Use 1.0 for Land to ensure it passes the > 0.01 threshold in refresh_background_colors
                self.bg_vals_flat = np.where(cell_vals_flat <= 0.01, -1.0, 1.0)
                
                # Render
                self.refresh_background_colors(first_load=True)
                
        except Exception as e:
            print(f"Error loading background globe: {e}")

    def setup_pop_colors(self):
        # Population Colors (Darker: Red, Violet, etc.)
        self.pop_brushes = [
            pg.mkBrush(200, 0, 0, 200),    # Pop 1: Dark Red
            pg.mkBrush(138, 43, 226, 200), # Pop 2: Blue Violet
            pg.mkBrush(0, 100, 0, 200),    # Pop 3: Dark Green
            pg.mkBrush(0, 0, 139, 200),    # Pop 4: Dark Blue
            pg.mkBrush(139, 0, 139, 200)   # Pop 5: Dark Magenta
        ]
        # For 3D (RGBA 0-1)
        self.pop_colors_3d = [
            (0.8, 0.0, 0.0, 1.0),
            (0.54, 0.17, 0.88, 1.0),
            (0.0, 0.4, 0.0, 1.0),
            (0.0, 0.0, 0.55, 1.0),
            (0.55, 0.0, 0.55, 1.0)
        ]

    def generate_grid_mesh(self, radius=10):
        # Generate vertices for the grid
        # Grid has dimensions (dlon, dlat) cells.
        # Vertices will be (dlon + 1, dlat + 1).
        
        # Create coordinate arrays
        lons = np.linspace(self.lon_0, self.lon_0 + self.dlon_hep * self.delta_lon, self.dlon + 1)
        lats = np.linspace(self.lat_0, self.lat_0 + self.dlat_hep * self.delta_lat, self.dlat + 1)
        
        # Meshgrid
        lon_grid, lat_grid = np.meshgrid(lons, lats, indexing='ij')
        
        # Convert to Cartesian
        # Flatten for vertex list
        lon_flat = lon_grid.flatten()
        lat_flat = lat_grid.flatten()
        
        verts = self.latlon_to_cartesian(lon_flat, lat_flat, radius)
        
        # Generate Faces
        # Each cell (i, j) corresponds to 2 triangles.
        # Vertices indices:
        # (i, j)     -> idx = i * (dlat + 1) + j
        # (i+1, j)   -> idx = (i+1) * (dlat + 1) + j
        # (i, j+1)   -> idx = i * (dlat + 1) + (j+1)
        # (i+1, j+1) -> idx = (i+1) * (dlat + 1) + (j+1)
        
        faces = []
        
        # We iterate over cells
        for i in range(self.dlon):
            for j in range(self.dlat):
                # Vertex indices
                v00 = i * (self.dlat + 1) + j
                v10 = (i + 1) * (self.dlat + 1) + j
                v01 = i * (self.dlat + 1) + (j + 1)
                v11 = (i + 1) * (self.dlat + 1) + (j + 1)
                
                # Triangle 1: (v00, v10, v01)
                faces.append([v00, v10, v01])
                
                # Triangle 2: (v10, v11, v01)
                faces.append([v10, v11, v01])
                
        faces = np.array(faces)
        
        # Create MeshData
        md = gl.MeshData(vertexes=verts, faces=faces)
        return md

    def map_hep_to_colors(self, values):
        # Active HEP color mapping
        # < 0: Water -> hep_water
        # >= 0: Land -> Gradient hep_low -> hep_high
        
        colors = np.zeros((len(values), 4))
        colors[:, 3] = 1.0 # Alpha
        
        mask_water = values < -0.01
        mask_land = values >= -0.01
        
        # Get colors from settings (Defaults if missing)
        c_water = self.view_settings.get('hep_water', (0.1, 0.2, 0.8, 1.0))
        c_low = self.view_settings.get('hep_low', (0.5, 0.5, 0.5, 1.0))
        c_high = self.view_settings.get('hep_high', (0.2, 0.8, 0.2, 1.0))
        
        # Water
        colors[mask_water] = c_water
        
        # Land Gradient
        t = values[mask_land]
        t = np.clip(t, 0, 1) # Ensure 0..1
        
        # Interpolate RGB
        for i in range(3):
             colors[mask_land, i] = c_low[i] + t * (c_high[i] - c_low[i])
        
        return colors

    def update_simulation(self):
        if not self.running:
            return

        if self.debug_mode:
            # In debug mode, we only update visualization (e.g. if user panned camera or resizing)
            # We do NOT step simulation automatically.
            # However, to keep UI responsive and 'alive', we might want to trigger update_viz 
            # if we had a manual step. But manual_step calls update_visualization directly.
            # So here we can just do nothing or process events.
            return

        # Step Simulation
        for _ in range(self.steps_per_frame):
            self.t += 1
            mod_python_interface.step_simulation(self.t)

        # Update Visualization
        self.update_visualization()

    def update_visualization(self):
        # 1. Get HEP Data
        t_hep_index = 1 
        hep_data = mod_python_interface.get_simulation_hep(t_hep_index, self.dlon, self.dlat, self.npops)
        
        # 2. Get Agents Data (Extended)
        count = mod_python_interface.get_agent_count()
        
        # Initialize containers for data
        x = np.zeros(0)
        y = np.zeros(0)
        pop = np.zeros(0, dtype=int)
        age = np.zeros(0, dtype=int)
        gender = np.zeros(0, dtype=int)
        resources = np.zeros(0, dtype=int)
        children = np.zeros(0, dtype=int)
        is_pregnant = np.zeros(0, dtype=int)
        avg_resources = np.zeros(0, dtype=float)
        
        if count > 0:
            # Call extended interface (Now 9 args + count)
            x, y, pop, age, gender, resources, children, is_pregnant, avg_resources = mod_python_interface.get_simulation_agents(count)
        
        # 3. Update Plots (if config set)
        if self.t % self.plot_config.get('update_freq', 10) == 0:
             self.update_analysis_plots(count, x, y, pop, age, gender, resources, children, is_pregnant, avg_resources)
        
        # 4. Viz Update
        # Debugging Print clearly
        # print("Viz Update. Settings:", self.view_settings) 
        
        show_agents = self.view_settings.get('show_agents', True)
        show_debug = self.view_settings.get('show_debug', False)
        
        if show_debug:
            try:
                # Fetch Debug Stats
                natural, starv, oob, confl, rnd, gxgy_out, update_pos, move = mod_python_interface.get_debug_stats()
                txt = f"<b>DEBUG COUNTERS</b><br>" \
                      f"<b>Deaths</b><br>" \
                      f"Natural: {natural}<br>" \
                      f"Starvation: {starv}<br>" \
                      f"Out of Bounds: {oob}<br>" \
                      f"Conflict: {confl}<br>" \
                      f"Random: {rnd}<br><br>" \
                      f"<b>Movement/Perf</b><br>" \
                      f"GXGY Out: {gxgy_out}<br>" \
                      f"Update Pos: {update_pos}<br>" \
                      f"Move Calls: {move}"
                self.debug_label.setText(txt)
                self.debug_label.adjustSize()
                self.debug_label.show()
                self.debug_label.raise_() # Ensure top
                
            except Exception as e:
                print(f"Error fetching debug stats: {e}")
                self.debug_label.setText(f"Error: {e}")
                self.debug_label.show()
        else:
            self.debug_label.hide()
            # print("Debug hidden")

        if self.view_mode == '2d':
            self.img_hep.setImage(hep_data[:, :, 0]) 
            
            # Update Clusters 2D
            if self.cb_show_clusters.isChecked():
                self.img_clusters.setVisible(True)
                # Fetch Clusters
                # get_cell_cluster_map(dlon, dlat) -> returns (dlon, dlat) array
                cluster_map = mod_python_interface.get_cell_cluster_map(self.dlon, self.dlat)
                
                # Generate RGBA
                # Map > 0 to unique color + Alpha
                # Map <= 0 to Transparent
                
                # Create a consistent color palette for clusters
                # We can use hashing or a fixed large palette
                # For simplicity, random colors seeded by cluster ID
                
                cluster_colors = np.zeros((self.dlon, self.dlat, 4), dtype=np.ubyte)
                
                valid_mask = cluster_map > 0
                if np.any(valid_mask):
                    ids = cluster_map[valid_mask]
                    
                    # Generate colors: R, G, B based on ID
                    # Simple hashing: 
                    # R = (ID * 123) % 255
                    # G = (ID * 456) % 255
                    # B = (ID * 789) % 255
                    
                    r = (ids * 50) % 255
                    g = (ids * 80) % 255
                    b = (ids * 110) % 255
                    
                    # Avoid too dark?
                    r = np.clip(r + 50, 0, 255)
                    g = np.clip(g + 50, 0, 255)
                    b = np.clip(b + 50, 0, 255)

                    cluster_colors[valid_mask, 0] = r
                    cluster_colors[valid_mask, 1] = g
                    cluster_colors[valid_mask, 2] = b
                    cluster_colors[valid_mask, 3] = 128 # 50% Alpha
                
                self.img_clusters.setImage(cluster_colors, levels=None)
            else:
                self.img_clusters.setVisible(False)
            
            if count > 0 and show_agents:
                # Map populations to brushes
                brushes = []
                for p in pop:
                    idx = (p - 1) % len(self.pop_brushes)
                    brushes.append(self.pop_brushes[idx])
                
                self.scatter_agents.setData(x=x, y=y, brush=brushes)
            else:
                 self.scatter_agents.setData(x=[], y=[])
                
        elif self.view_mode == '3d':
            # Ensure context is current before updating GL items
            self.gl_view.makeCurrent()
            
            # Update Sphere Colors
            sim_vals = hep_data[:, :, 0].flatten() 
            
            # Map to colors
            cell_colors = self.map_hep_to_colors(sim_vals) # (N_cells, 4)
            
            # Duplicate for 2 triangles per cell
            face_colors = np.repeat(cell_colors, 2, axis=0)
            
            # Update MeshData directly with FACE colors
            self.md.setFaceColors(face_colors)
            self.sphere_item.setMeshData(meshdata=self.md)
            
            # Update Clusters 3D
            if self.cb_show_clusters.isChecked():
                self.cluster_sphere.setVisible(True)
                cluster_map = mod_python_interface.get_cell_cluster_map(self.dlon, self.dlat)
                flat_map = cluster_map.flatten()
                
                # Colors
                c_colors = np.zeros((len(flat_map), 4))
                
                valid_mask = flat_map > 0
                if np.any(valid_mask):
                    ids = flat_map[valid_mask]
                    
                    r = ((ids * 50) % 255) / 255.0
                    g = ((ids * 80) % 255) / 255.0
                    b = ((ids * 110) % 255) / 255.0
                    
                    r = np.clip(r + 0.2, 0.0, 1.0)
                    g = np.clip(g + 0.2, 0.0, 1.0)
                    b = np.clip(b + 0.2, 0.0, 1.0)
                    
                    c_colors[valid_mask, 0] = r
                    c_colors[valid_mask, 1] = g
                    c_colors[valid_mask, 2] = b
                    c_colors[valid_mask, 3] = 0.5 # 50% Alpha

                # Repeat for faces
                c_face_colors = np.repeat(c_colors, 2, axis=0)
                
                self.md_clusters.setFaceColors(c_face_colors)
                self.cluster_sphere.setMeshData(meshdata=self.md_clusters)
            else:
                self.cluster_sphere.setVisible(False)
            
            # Update Agents
            if count > 0 and show_agents:
                # Convert (lon, lat) to (x, y, z)
                pos = self.latlon_to_cartesian(x, y, radius=10.1) # Slightly above surface
                
                # Colors
                colors = np.zeros((count, 4))
                for i in range(count):
                    idx = (pop[i] - 1) % len(self.pop_colors_3d)
                    colors[i] = self.pop_colors_3d[idx]
                    
                self.scatter_3d.setData(pos=pos, color=colors, size=5, pxMode=True)
            else:
                self.scatter_3d.setData(pos=np.zeros((0,3)))

        self.setWindowTitle(f"HEP Simulation ({self.view_mode.upper()}) - Step: {self.t} - Agents: {count}")

    def update_analysis_plots(self, count, x, y, pop, age, gender, resources, children, is_pregnant, avg_resources):
        if not self.active_plots:
            return

        import numpy as np

        # Helper to apply filter
        def get_mask(pdef):
            mask = np.ones(count, dtype=bool)
            if 'filter_var' in pdef and pdef['filter_var']:
                var = pdef['filter_var']
                val = pdef.get('filter_val', 0)
                
                # Retrieve data array by name
                data = None
                if var == 'population': data = pop
                elif var == 'age': data = age
                elif var == 'gender': data = gender
                elif var == 'resources': data = resources
                elif var == 'children': data = children
                
                if data is not None:
                     # Allow simple equality or range? For now equality or specific logic
                     if var == 'population': mask = (data == int(val))
                     else: mask = (data == int(val)) # Basic equality
            return mask

        # Helper to get variable data
        def get_data(var_name):
            if var_name == 'age': return age
            elif var_name == 'resources': return resources
            elif var_name == 'children': return children
            elif var_name == 'population': return pop
            elif var_name == 'is_pregnant': return is_pregnant
            elif var_name == 'avg_resources': return avg_resources
            elif var_name == 'gender': return gender
            return None

        for item in self.active_plots:
            pdef = item['def']
            ptype = pdef['type']
            
            # Apply Filter
            mask = get_mask(pdef)
            filtered_count = np.sum(mask)
            
            if ptype == 'timeseries' or ptype == 'count':
                var_name = pdef['variable']
                val = 0
                
                data = get_data(var_name)
                
                if data is not None and filtered_count > 0:
                    d = data[mask]
                    
                    if ptype == 'timeseries':
                         agg = pdef.get('aggregation', 'mean')
                         if agg == 'mean': val = np.mean(d)
                         elif agg == 'sum': val = np.sum(d)
                         elif agg == 'min': val = np.min(d)
                         elif agg == 'max': val = np.max(d)
                         
                    elif ptype == 'count':
                         op = pdef.get('operator', '==')
                         cval = float(pdef.get('condition_val', 0))
                         
                         if op == '==': val = np.sum(d == cval)
                         elif op == '<=': val = np.sum(d <= cval)
                         elif op == '>=': val = np.sum(d >= cval)
                         elif op == '<': val = np.sum(d < cval)
                         elif op == '>': val = np.sum(d > cval)
                         elif op == '!=': val = np.sum(d != cval)
                
                # Update History
                key = f"{pdef['title']}_{var_name}"
                hist = self.plot_data_history[key]
                hist['x'].append(self.t)
                hist['y'].append(val)
                
                # Update Curve
                item['items'][0].setData(x=list(hist['x']), y=list(hist['y']))
                
            elif ptype == 'bucket':
                 var_name = pdef['variable'] # e.g. age
                 buckets = int(pdef.get('buckets', 20))
                 
                 data_all = get_data(var_name)
                 
                 if data_all is not None and filtered_count > 0:
                      d = data_all[mask]
                      g = gender[mask] # 0=F, 1=M
                      
                      # Determine range
                      if var_name == 'age':
                          # Fixed range for Age to prevent "frozen" auto-scaling view
                          min_v, max_v = 0, 100
                      elif var_name == 'population':
                          min_v, max_v = 0, self.npops + 1
                      else:
                          # Dynamic for others
                          min_v = np.min(d)
                          max_v = np.max(d)
                          if max_v == min_v: max_v += 1
                      
                      # Create bins
                      bins = np.linspace(min_v, max_v, buckets+1)
                      
                      # Histogram for Males and Females
                      hist_m, _ = np.histogram(d[g==1], bins=bins)
                      hist_f, _ = np.histogram(d[g==0], bins=bins)
                      
                      # Normalize to percentages
                      hist_m = (hist_m / filtered_count) * 100.0
                      hist_f = (hist_f / filtered_count) * 100.0
                      
                      # X positions (centers)
                      x_centers = (bins[:-1] + bins[1:]) / 2
                      width = (bins[1] - bins[0]) * 0.8
                      
                      bg_item = item['items'][0]
                      
                      x_final = np.concatenate([x_centers, x_centers])
                      h_final = np.concatenate([hist_m, -hist_f]) # Male +, Female -
                      
                      # Brushes: Blue for Male, Red for Female
                      brushes = [pg.mkBrush('b')] * len(hist_m) + [pg.mkBrush('r')] * len(hist_f)
                      
                      bg_item.setOpts(x=x_final, height=h_final, width=width, brushes=brushes)
                      
                      # Fix X View Range for Age to ensure stability
                      if var_name == 'age':
                          item['widget'].setXRange(0, 100, padding=0)

                 else:
                      item['items'][0].setOpts(x=[], height=[])

    def latlon_to_cartesian(self, lon, lat, radius=10):
        # Convert lat/lon to radians
        # Ensure inputs are numpy arrays
        lon = np.array(lon)
        lat = np.array(lat)
        
        phi = np.radians(lat)
        theta = np.radians(lon)
        
        # Cartesian coordinates
        # x = r * cos(lat) * cos(lon)
        # y = r * cos(lat) * sin(lon)
        # z = r * sin(lat)
        
        # Adjust for coordinate system if needed.
        # Usually Z is up.
        
        x = radius * np.cos(phi) * np.cos(theta)
        y = radius * np.cos(phi) * np.sin(theta)
        z = radius * np.sin(phi)
        
        return np.column_stack((x, y, z))

    def keyPressEvent(self, event):
        if event.key() == QtCore.Qt.Key_Q or event.key() == QtCore.Qt.Key_Escape:
            self.close()
        else:
            super().keyPressEvent(event)

    def closeEvent(self, event):
        print("Closing simulation window...")
        self.running = False
        self.timer.stop()
        event.accept()

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    window = SimulationWindow()
    window.show()
    sys.exit(app.exec_())
