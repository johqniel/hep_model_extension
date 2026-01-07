import sys
import os
import json
import numpy as np
import pyqtgraph as pg
from PyQt5 import QtWidgets, QtCore, QtGui

# Add parent directory to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError:
    mod_python_interface = None

class SpawnPointEditor(QtWidgets.QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        # self.setWindowTitle("Spawn Point Editor") # Not needed for widget
        # self.resize(1000, 800) # Managed by parent layout

        self.layout = QtWidgets.QHBoxLayout(self) # Set layout directly on self

        # Left Panel: Controls
        self.left_panel = QtWidgets.QWidget()
        self.left_layout = QtWidgets.QVBoxLayout(self.left_panel)
        self.layout.addWidget(self.left_panel, 1)

        # Removed Load HEP button - handled by parent context
        # self.btn_load_hep = QtWidgets.QPushButton("Load HEP Map")
        # self.btn_load_hep.clicked.connect(self.load_hep_map)
        # self.left_layout.addWidget(self.btn_load_hep)

        self.lbl_mode = QtWidgets.QLabel("Mode: View")
        self.left_layout.addWidget(self.lbl_mode)

        self.btn_add_point = QtWidgets.QPushButton("Add Spawn Point")
        self.btn_add_point.setCheckable(True)
        self.btn_add_point.clicked.connect(self.toggle_add_mode)
        self.left_layout.addWidget(self.btn_add_point)

        self.list_points = QtWidgets.QListWidget()
        self.list_points.setSelectionMode(QtWidgets.QAbstractItemView.ExtendedSelection)
        self.list_points.itemSelectionChanged.connect(self.update_visualization)
        self.left_layout.addWidget(self.list_points)

        self.btn_remove_point = QtWidgets.QPushButton("Remove Selected")
        self.btn_remove_point.clicked.connect(self.remove_point)
        self.left_layout.addWidget(self.btn_remove_point)

        self.btn_save = QtWidgets.QPushButton("Save Points")
        self.btn_save.clicked.connect(self.save_points)
        self.left_layout.addWidget(self.btn_save)

        self.btn_load = QtWidgets.QPushButton("Load Points")
        self.btn_load.clicked.connect(self.load_points)
        self.left_layout.addWidget(self.btn_load)

        # Module Configuration
        self.left_layout.addSpacing(20)
        self.left_layout.addWidget(QtWidgets.QLabel("<b>Module Configuration</b>"))
        
        self.combo_modules = QtWidgets.QComboBox()
        self.available_modules = {
            "Natural Deaths": 1,
            "Births": 2,
            "Move": 3,
            "Update Age": 4,
            "Find Mate": 5,
            "Distribute Ressources": 6,
            "Resource Mortality": 7
        }
        self.combo_modules.addItems(self.available_modules.keys())
        self.left_layout.addWidget(self.combo_modules)
        
        self.btn_add_module = QtWidgets.QPushButton("Add Module")
        self.btn_add_module.clicked.connect(self.add_module)
        self.left_layout.addWidget(self.btn_add_module)
        
        self.list_modules = QtWidgets.QListWidget()
        self.list_modules.setDragDropMode(QtWidgets.QAbstractItemView.InternalMove)
        self.left_layout.addWidget(self.list_modules)
        
        self.btn_remove_module = QtWidgets.QPushButton("Remove Module")
        self.btn_remove_module.clicked.connect(self.remove_module)
        self.left_layout.addWidget(self.btn_remove_module)

        # Default Configuration
        self.add_module_by_name("Natural Deaths")
        self.add_module_by_name("Births")
        self.add_module_by_name("Move")
        self.add_module_by_name("Update Age")



        # Right Panel: Map
        self.glw = pg.GraphicsLayoutWidget()
        self.layout.addWidget(self.glw, 3)
        
        self.plot = self.glw.addPlot(title="Spawn Points")
        # Aspect ratio will be set when HEP is loaded
        self.img_hep = pg.ImageItem()
        self.plot.addItem(self.img_hep)
        
        # Spawn points visualization
        self.scatter = pg.ScatterPlotItem()
        self.plot.addItem(self.scatter)
        
        # Interaction state
        self.adding_point = False
        self.drag_start = None
        self.current_circle = None
        
        # Data
        self.spawn_points = [] # List of dicts: {x, y, spread, count, pop}
        self.hep_data = None
        self.hep_transform = None
        
        # Context (Config and HEP paths)
        self.config_path = None
        self.hep_paths = []
        
        # Connect events
        self.proxy = pg.SignalProxy(self.plot.scene().sigMouseMoved, rateLimit=60, slot=self.on_mouse_move)
        self.plot.scene().sigMouseClicked.connect(self.on_mouse_click)

    def set_hep_context(self, config_path, hep_paths):
        """
        Sets the configuration and HEP paths for the editor.
        Loads the HEP map for visualization using netCDF4 (no Fortran init).
        """
        self.config_path = config_path
        self.hep_paths = hep_paths
        
        if not self.config_path or not self.hep_paths:
            return

        try:
            import netCDF4
            
            # Load the first HEP file
            hep_file = self.hep_paths[0]
            print(f"SpawnEditor: Loading HEP from {hep_file}...")
            
            with netCDF4.Dataset(hep_file, 'r') as nc:
                # Read dimensions
                lats = nc.variables['lat'][:]
                lons = nc.variables['lon'][:]
                
                # Calculate grid parameters
                self.lat_0 = lats[0]
                self.lon_0 = lons[0]
                self.dlat_hep = len(lats)
                self.dlon_hep = len(lons)
                
                if len(lats) > 1:
                    self.delta_lat = lats[1] - lats[0]
                else:
                    self.delta_lat = 1.0 # Default or error
                    
                if len(lons) > 1:
                    self.delta_lon = lons[1] - lons[0]
                else:
                    self.delta_lon = 1.0
                
                # Read HEP Data (Time 0)
                # Assuming variable is 'AccHEP' or similar. Let's try to find it.
                hep_var_name = None
                for v in nc.variables:
                    if 'hep' in v.lower():
                        hep_var_name = v
                        break
                
                if not hep_var_name:
                    # Fallback to looking for 3D variable
                    for v in nc.variables:
                        if nc.variables[v].ndim == 3:
                            hep_var_name = v
                            break
                            
                if hep_var_name:
                    # Read first time step (index 0)
                    # Shape is usually (time, lat, lon) or (time, lon, lat)?
                    # Fortran reads it as (lon, lat, time) but netCDF python usually (time, lat, lon)
                    # Let's check dimensions.
                    # In mod_read_inputs.f95: start = (/ 1, 1, 1 /), count = (/ dlon_hep, dlat_hep, n_timesteps /)
                    # So likely (lon, lat, time) in the file if Fortran reads it directly?
                    # Or Fortran reorders?
                    # Usually NetCDF is (time, lat, lon).
                    # Let's assume (time, lat, lon) or (lat, lon, time).
                    
                    # Get variable object
                    var_obj = nc.variables[hep_var_name]
                    data = var_obj[:]
                    dims = var_obj.dimensions
                    
                    # Check for time dimension
                    time_idx = -1
                    for i, d in enumerate(dims):
                        if 'time' in d.lower() or 't' == d.lower():
                            time_idx = i
                            break
                    
                    if time_idx != -1:
                        # Slice at time 0
                        # We need to handle generic slicing
                        slices = [slice(None)] * data.ndim
                        slices[time_idx] = 0
                        self.hep_data = data[tuple(slices)]
                    else:
                        # Fallback heuristic
                        if data.ndim == 3:
                            # If (time, lat, lon) -> take time 0
                            if data.shape[0] < data.shape[1] and data.shape[0] < data.shape[2]:
                                 self.hep_data = data[0, :, :]
                            # If (lon, lat, time)
                            elif data.shape[2] < data.shape[0] and data.shape[2] < data.shape[1]:
                                 self.hep_data = data[:, :, 0]
                            # If time is the largest dimension (likely the case here)
                            elif data.shape[0] > data.shape[1] and data.shape[0] > data.shape[2]:
                                 self.hep_data = data[0, :, :]
                            else:
                                 # Ambiguous, take first slice of first dim
                                 self.hep_data = data[0, :, :]
                        elif data.ndim == 2:
                            self.hep_data = data
                    
                    # Ensure shape matches (lon, lat) for ImageItem (x, y)
                    # We expect (lat, lon) from netCDF usually.
                    # If shape is (dlat, dlon), transpose to (dlon, dlat)
                    if self.hep_data.shape == (self.dlat_hep, self.dlon_hep):
                        self.hep_data = self.hep_data.T
                    elif self.hep_data.shape == (self.dlon_hep, self.dlat_hep):
                        pass # Already correct
                    else:
                        print(f"Error: HEP data shape {self.hep_data.shape} does not match grid dimensions ({self.dlon_hep}, {self.dlat_hep}).")
                        return

                else:
                    print("Error: Could not find HEP variable.")
                    return

                # Apply Watermask if available
                if 'watermask' in nc.variables:
                    mask = nc.variables['watermask'][:]
                    # Mask shape is usually (lat, lon) -> (110, 113)
                    # Need to transpose to (lon, lat) -> (113, 110) to match hep_data
                    if mask.shape[0] == self.dlat_hep and mask.shape[1] == self.dlon_hep:
                        mask = mask.T
                    
                    # Apply mask: if mask == 0 (water), set hep_data to -1
                    # Assuming watermask is 1 for land, 0 for water?
                    # Or 0 for land, 1 for water?
                    # Usually 1 is land.
                    # Let's assume 0 is water.
                    if self.hep_data.shape == mask.shape:
                        self.hep_data[mask == 0] = -1
                    else:
                        print(f"Warning: Watermask shape {mask.shape} does not match HEP data shape {self.hep_data.shape}")

            # Set Image
            if self.hep_data is not None:
                self.img_hep.setImage(self.hep_data)
                
                # Set Transform
                tr = QtGui.QTransform()
                tr.translate(self.lon_0, self.lat_0)
                tr.scale(self.delta_lon, self.delta_lat)
                self.img_hep.setTransform(tr)
                
                # Set Ranges
                self.plot.setXRange(self.lon_0, self.lon_0 + self.dlon_hep * self.delta_lon)
                self.plot.setYRange(self.lat_0, self.lat_0 + self.dlat_hep * self.delta_lat)
                
                # Colormap (Blue to Green)
                pos = np.array([0.0, 0.5, 1.0])
                color = np.array([
                    [0, 0, 255, 255],      # Blue
                    [253, 253, 150, 255],  # Yellow
                    [119, 221, 119, 255]   # Green
                ], dtype=np.ubyte)
                cmap = pg.ColorMap(pos, color)
                self.img_hep.setLookupTable(cmap.getLookupTable(0.0, 1.0, 256))
                self.img_hep.setLevels([-1, 1])
                
                # Enforce square grid cells
                if self.delta_lat != 0:
                    ratio = self.delta_lon / self.delta_lat
                    self.plot.setAspectLocked(True, ratio=ratio)
                else:
                    self.plot.setAspectLocked(True)
                
                self.lbl_mode.setText(f"Loaded HEP from {os.path.basename(hep_file)}")
                
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to load HEP context: {e}")
            import traceback
            traceback.print_exc()

    def toggle_add_mode(self):
        if self.btn_add_point.isChecked():
            self.lbl_mode.setText("Mode: Add Point (Click Center & Drag Radius)")
            self.adding_point = True
        else:
            self.lbl_mode.setText("Mode: View")
            self.adding_point = False
            self.drag_start = None
            if self.current_circle:
                self.plot.removeItem(self.current_circle)
                self.current_circle = None

    def on_mouse_click(self, event):
        if self.adding_point:
            # Map to view coordinates
            pos = event.scenePos()
            if not self.plot.sceneBoundingRect().contains(pos):
                return
            
            mouse_point = self.plot.vb.mapSceneToView(pos)
            
            if event.button() == QtCore.Qt.LeftButton:
                if not self.drag_start:
                    # First click: Set Center
                    self.drag_start = mouse_point
                    
                    # Create visual feedback
                    self.current_circle = QtWidgets.QGraphicsEllipseItem(0, 0, 0, 0)
                    self.current_circle.setPen(pg.mkPen('r', width=2))
                    self.current_circle.setBrush(pg.mkBrush(255, 0, 0, 50))
                    self.plot.addItem(self.current_circle)
                    
                    # Update circle position (centered)
                    self.update_circle(self.drag_start, 0)
                    
                else:
                    # Second click (finish drag)
                    radius = ((mouse_point.x() - self.drag_start.x())**2 + (mouse_point.y() - self.drag_start.y())**2)**0.5
                    self.finish_add_point(self.drag_start, radius)
                    self.drag_start = None
                    if self.current_circle:
                        self.plot.removeItem(self.current_circle)
                        self.current_circle = None
        else:
            # Selection Mode
            pos = event.scenePos()
            if not self.plot.sceneBoundingRect().contains(pos):
                return
            
            # Check for clicks on existing points
            # We check in screen/scene coordinates for better UX
            clicked_point_idx = -1
            min_dist = 15 # pixels
            
            for i, p in enumerate(self.spawn_points):
                # Map point to scene
                pt_view = QtCore.QPointF(p['x'], p['y'])
                pt_scene = self.plot.vb.mapViewToScene(pt_view)
                
                dist = ((pt_scene.x() - pos.x())**2 + (pt_scene.y() - pos.y())**2)**0.5
                if dist < min_dist:
                    min_dist = dist
                    clicked_point_idx = i
            
            if clicked_point_idx != -1:
                # Toggle or Select
                item = self.list_points.item(clicked_point_idx)
                if event.modifiers() & QtCore.Qt.ControlModifier:
                    # Toggle
                    item.setSelected(not item.isSelected())
                else:
                    # Select only this one (unless already selected and ctrl not pressed? standard behavior is select only)
                    self.list_points.clearSelection()
                    item.setSelected(True)
                    self.list_points.scrollToItem(item)
            else:
                # Clicked empty space
                if not (event.modifiers() & QtCore.Qt.ControlModifier):
                    self.list_points.clearSelection()

    def on_mouse_move(self, event):
        if not self.adding_point or not self.drag_start:
            return
            
        pos = event[0]
        mouse_point = self.plot.vb.mapSceneToView(pos)
        
        radius = ((mouse_point.x() - self.drag_start.x())**2 + (mouse_point.y() - self.drag_start.y())**2)**0.5
        self.update_circle(self.drag_start, radius)

    def update_circle(self, center, radius):
        if not self.current_circle:
            return
        # QGraphicsEllipseItem takes (x, y, w, h) of bounding rectangle
        # x, y is top-left corner
        self.current_circle.setRect(center.x() - radius, center.y() - radius, 2 * radius, 2 * radius)

    def finish_add_point(self, center, radius):
        # Ask for number of agents
        count, ok = QtWidgets.QInputDialog.getInt(self, "Agent Count", "Number of agents:", 100, 1, 1000000)
        if not ok:
            return
            
        # Ask for population (optional, default to 1 for now or add a selector)
        pop, ok = QtWidgets.QInputDialog.getInt(self, "Population", "Population ID:", 1, 1, 5) # Assuming max 5 pops
        if not ok:
            pop = 1
            
        point_data = {
            'x': center.x(),
            'y': center.y(),
            'spread': radius,
            'count': count,
            'pop': pop
        }
        self.spawn_points.append(point_data)
        self.update_list()
        self.update_visualization()
        
        # Reset mode if desired, or keep adding
        # self.toggle_add_mode() # Uncomment to stop adding after one

    def update_list(self):
        self.list_points.clear()
        for i, p in enumerate(self.spawn_points):
            item_text = f"Pop {p['pop']}: ({p['x']:.1f}, {p['y']:.1f}) R={p['spread']:.1f} N={p['count']}"
            self.list_points.addItem(item_text)

    def remove_point(self):
        selected_items = self.list_points.selectedItems()
        if not selected_items:
            return
            
        # Get indices to remove (in descending order to avoid shifting issues)
        indices = []
        for item in selected_items:
            indices.append(self.list_points.row(item))
        
        indices.sort(reverse=True)
        
        for idx in indices:
            if 0 <= idx < len(self.spawn_points):
                del self.spawn_points[idx]
                
        self.update_list()
        self.update_visualization()

    def keyPressEvent(self, event):
        if event.key() == QtCore.Qt.Key_Delete:
            self.remove_point()
        else:
            super().keyPressEvent(event)

    def update_visualization(self):
        # Clear old items
        self.scatter.clear()
        
        # Get selected indices
        selected_rows = set()
        for item in self.list_points.selectedItems():
            selected_rows.add(self.list_points.row(item))
        
        spots = []
        for i, p in enumerate(self.spawn_points):
            # Draw center
            if i in selected_rows:
                brush = 'y' # Yellow for selected
                pen = {'color': 'w', 'width': 2}
            else:
                brush = 'r' # Red for normal
                pen = {'color': 'w', 'width': 1}
                
            spots.append({'pos': (p['x'], p['y']), 'size': 10, 'pen': pen, 'brush': brush})
            
        self.scatter.setData(spots)
        
        # Remove old circles (except current interaction one)
        for item in self.plot.items[:]:
            if isinstance(item, QtWidgets.QGraphicsEllipseItem) and item is not self.current_circle:
                self.plot.removeItem(item)
                
        # Add circles for all points
        for i, p in enumerate(self.spawn_points):
            circle = QtWidgets.QGraphicsEllipseItem(p['x'] - p['spread'], p['y'] - p['spread'], 2 * p['spread'], 2 * p['spread'])
            
            if i in selected_rows:
                circle.setPen(pg.mkPen('y', style=QtCore.Qt.SolidLine, width=2))
            else:
                circle.setPen(pg.mkPen('w', style=QtCore.Qt.DashLine))
                
            self.plot.addItem(circle)

    def save_points(self):
        if not self.spawn_points:
            QtWidgets.QMessageBox.warning(self, "Warning", "No points to save.")
            return
            
        fname, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Save Spawn Points", "", "JSON Files (*.json)")
        if not fname:
            return
            
        try:
            with open(fname, 'w') as f:
                json.dump(self.spawn_points, f, indent=4)
            QtWidgets.QMessageBox.information(self, "Success", "Spawn points saved.")
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to save points: {e}")

    def load_points(self):
        fname, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Load Spawn Points", "", "JSON Files (*.json)")
        if not fname:
            return
            
        try:
            with open(fname, 'r') as f:
                self.spawn_points = json.load(f)
            self.update_list()
            self.update_visualization()
            QtWidgets.QMessageBox.information(self, "Success", "Spawn points loaded.")
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to load points: {e}")

    def run_simulation(self):
        if not self.spawn_points:
            QtWidgets.QMessageBox.warning(self, "Warning", "No spawn points defined.")
            return

        if not mod_python_interface:
            QtWidgets.QMessageBox.critical(self, "Error", "mod_python_interface not available")
            return

        # Prepare data for Fortran
        # We need to organize data by population
        # Fortran expects arrays of size (ns, npops)
        # But here we have a dynamic list of points.
        # We need to fit this into the fixed structure or just pass what we have.
        # The set_spawn_configuration takes (ns, x_ini, y_ini, spread, counts, npops)
        # We should probably group by population and find the max number of sources per pop.
        
        # Actually, let's just treat each point as a source.
        # We need to find the max number of sources across all populations to define 'ns'.
        
        points_by_pop = {}
        for p in self.spawn_points:
            pop = p['pop']
            if pop not in points_by_pop:
                points_by_pop[pop] = []
            points_by_pop[pop].append(p)
            
        max_sources = 0
        for pop in points_by_pop:
            max_sources = max(max_sources, len(points_by_pop[pop]))
            
        if max_sources == 0:
            return

        ns = max_sources
        # npops is already known from init, but let's be safe
        npops = self.npops if hasattr(self, 'npops') else 5 # Default fallback
        
    def get_spawn_points(self):
        return self.spawn_points

    def set_spawn_points(self, points):
        self.spawn_points = points
        self.update_list()
        self.update_visualization()

    def add_module(self):
        name = self.combo_modules.currentText()
        self.add_module_by_name(name)

    def add_module_by_name(self, name):
        if name in self.available_modules:
            self.list_modules.addItem(name)

    def remove_module(self):
        for item in self.list_modules.selectedItems():
            self.list_modules.takeItem(self.list_modules.row(item))

    def get_module_configuration(self):
        modules = []
        for i in range(self.list_modules.count()):
            name = self.list_modules.item(i).text()
            if name in self.available_modules:
                modules.append(self.available_modules[name])
        return modules

    def get_module_names(self):
        names = []
        for i in range(self.list_modules.count()):
            names.append(self.list_modules.item(i).text())
        return names

    def set_module_configuration(self, names):
        self.list_modules.clear()
        for name in names:
            self.add_module_by_name(name)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = SpawnPointEditor()
    window.show()
    sys.exit(app.exec_())
