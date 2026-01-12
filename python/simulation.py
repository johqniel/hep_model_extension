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
        self.resize(1200, 800)

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
        
        # Store background data to allowing recoloring
        self.bg_faces = None
        self.bg_verts = None
        self.bg_vals_flat = None
        self.bg_sphere = None

        # Central Widget and Layout
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.layout = QtWidgets.QHBoxLayout(self.central_widget)

        # Graphics Layout Widget or GL View Widget will be added directly

        # Initialize Simulation
        if not skip_init:
            print("Initializing simulation...")
            mod_python_interface.init_simulation()
        else:
            print("Skipping initialization (assumed already initialized).")
        
        # Get Grid Dimensions
        self.dlon, self.dlat, self.npops = mod_python_interface.get_grid_dims()
        print(f"Grid Dimensions: {self.dlon}x{self.dlat}, Pops: {self.npops}")

        # Get Simulation Config
        self.lon_0, self.lat_0, self.delta_lon, self.delta_lat, self.dlon_hep, self.dlat_hep = mod_python_interface.get_simulation_config()
        print(f"Config: lon_0={self.lon_0}, lat_0={self.lat_0}, dlon={self.delta_lon}, dlat={self.delta_lat}")

        # Setup Plots
        self.view_mode = view_mode
        self.setup_plots()

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

    def setup_plots(self):
        if self.view_mode == '2d':
            self.setup_2d_view()
        elif self.view_mode == '3d':
            self.setup_3d_view()
            
    def setup_2d_view(self):
        self.glw = pg.GraphicsLayoutWidget()
        self.layout.addWidget(self.glw)
        
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
        
        # Population Colors
        self.setup_pop_colors()

    def setup_3d_view(self):
        self.gl_view = gl.GLViewWidget()
        # Set as central widget directly to avoid layout issues
        self.setCentralWidget(self.gl_view)
        
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
        
        self.setup_pop_colors()

    def update_view_settings(self, settings):
        self.view_settings = settings
        print("Settings updated:", self.view_settings)
        
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

    def update_view_settings(self, settings):
        self.view_settings = settings
        print("Settings updated:", self.view_settings)
        
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
        hep_path = os.path.join(os.path.dirname(__file__), '..', 'input', 'hep', 'generated', 'earth_hep.nc')
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
        
        # 2. Get Agents Data
        count = mod_python_interface.get_agent_count()
        
        if self.view_mode == '2d':
            self.img_hep.setImage(hep_data[:, :, 0]) 
            
            if count > 0:
                x, y, pop = mod_python_interface.get_simulation_agents(count)
                
                # Map populations to brushes
                brushes = []
                for p in pop:
                    idx = (p - 1) % len(self.pop_brushes)
                    brushes.append(self.pop_brushes[idx])
                
                self.scatter_agents.setData(x=x, y=y, brush=brushes)
                
        elif self.view_mode == '3d':
            # Ensure context is current before updating GL items
            self.gl_view.makeCurrent()
            
            # Update Sphere Colors
            # Update Sphere Colors
            # hep_data is (dlon, dlat, 1)
            # We have 2 faces per cell.
            # Order of faces in generate_grid_mesh:
            # Loop i (0..dlon-1), Loop j (0..dlat-1) -> 2 faces
            
            # Flatten hep_data to match face order
            # hep_data[i, j] corresponds to faces 2*(i*dlat + j) and 2*(i*dlat + j) + 1
            
            # Flatten data column-wise (inner loop is j/lat)?
            # Our loops: for i: for j:
            # So we need to flatten such that j varies fastest.
            # hep_data is (dlon, dlat, 1).
            # hep_data[i, j]
            
            sim_vals = hep_data[:, :, 0].flatten() # Default is C-style (last index varies fastest)? No, numpy default is C (row-major).
            # Wait, hep_data shape is (dlon, dlat).
            # flatten() will do: (0,0), (0,1), ... (0, dlat-1), (1,0)...
            # This matches our loop order: for i: for j:
            # So sim_vals[k] corresponds to cell k.
            
            # Map to colors
            cell_colors = self.map_hep_to_colors(sim_vals) # (N_cells, 4)
            
            # Duplicate for 2 triangles per cell
            # We need (N_cells * 2, 4)
            face_colors = np.repeat(cell_colors, 2, axis=0)
            
            # Update MeshData directly with FACE colors
            self.md.setFaceColors(face_colors)
            self.sphere_item.setMeshData(meshdata=self.md)
            
            # Update Agents
            
            # Update Agents
            if count > 0:
                x, y, pop = mod_python_interface.get_simulation_agents(count)
                
                # Convert (lon, lat) to (x, y, z)
                # Assuming x is lon, y is lat
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
