import sys
import os
import numpy as np
import pyqtgraph as pg
from pyqtgraph.Qt import QtCore, QtWidgets, QtGui
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
    def __init__(self, skip_init=False):
        super().__init__()
        self.setWindowTitle("HEP Simulation")
        self.resize(1200, 800)

        # Central Widget and Layout
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.layout = QtWidgets.QHBoxLayout(self.central_widget)

        # Graphics Layout Widget
        self.glw = pg.GraphicsLayoutWidget()
        self.layout.addWidget(self.glw)

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
        self.setup_plots()

        # Simulation State
        self.t = 0
        self.running = True
        self.steps_per_frame = 1

        # Timer
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.update_simulation)
        self.timer.start(0) # Run as fast as possible

    def setup_plots(self):
        # HEP Plot (Heatmap)
        self.plot_hep = self.glw.addPlot(title="HEP Density")
        
        # Enforce square grid cells
        # We want 1 unit of grid width (delta_lon) to visually equal 1 unit of grid height (delta_lat)
        # Ratio = delta_lon / delta_lat
        if self.delta_lat != 0:
            ratio = self.delta_lon / self.delta_lat
            self.plot_hep.setAspectLocked(True, ratio=ratio)
        else:
            self.plot_hep.setAspectLocked(True)

        self.img_hep = pg.ImageItem()
        self.plot_hep.addItem(self.img_hep)
        
        # Set Image Transform to map grid to physical coordinates
        # Scale by delta_lon, delta_lat
        # Translate to lon_0, lat_0
        tr = QtGui.QTransform()
        tr.translate(self.lon_0, self.lat_0)
        tr.scale(self.delta_lon, self.delta_lat)
        self.img_hep.setTransform(tr)
        
        # Colormap for HEP
        # Blue (-1), Pastel Yellow (0), Pastel Green (1)
        # Pastel Yellow: (253, 253, 150)
        # Pastel Green: (119, 221, 119)
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
        
        # Population Colors (Darker: Red, Violet, etc.)
        self.pop_brushes = [
            pg.mkBrush(200, 0, 0, 200),    # Pop 1: Dark Red
            pg.mkBrush(138, 43, 226, 200), # Pop 2: Blue Violet
            pg.mkBrush(0, 100, 0, 200),    # Pop 3: Dark Green
            pg.mkBrush(0, 0, 139, 200),    # Pop 4: Dark Blue
            pg.mkBrush(139, 0, 139, 200)   # Pop 5: Dark Magenta
        ]

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
        self.img_hep.setImage(hep_data[:, :, 0]) 

        # 2. Get Agents Data
        count = mod_python_interface.get_agent_count()
        
        if count > 0:
            x, y, pop = mod_python_interface.get_simulation_agents(count)
            
            # Map populations to brushes
            brushes = []
            for p in pop:
                idx = (p - 1) % len(self.pop_brushes)
                brushes.append(self.pop_brushes[idx])
            
            self.scatter_agents.setData(x=x, y=y, brush=brushes)
            
        self.setWindowTitle(f"HEP Simulation - Step: {self.t} - Agents: {count}")

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
