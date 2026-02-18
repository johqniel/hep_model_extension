import sys
import os
import numpy as np
import time
from PyQt5 import QtCore

# Try to import PIL for GIF generation
try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False

# Add parent directory to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    import mod_python_interface
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError:
    mod_python_interface = None

class HeadlessSimulationThread(QtCore.QThread):
    progress_update = QtCore.pyqtSignal(int, int, int, float) # progress %, current_tick, agent_count, time_elapsed
    finished = QtCore.pyqtSignal(str) # output filename
    error = QtCore.pyqtSignal(str)

    def __init__(self, start_year, end_year, config_path, hep_paths, module_config, spawn_points, npops, output_path):
        super().__init__()
        self.start_year = start_year
        self.end_year = end_year
        self.config_path = config_path
        self.hep_paths = hep_paths
        self.module_config = module_config
        self.spawn_points = spawn_points
        self.npops = npops
        self.output_path = output_path
        
        # Constants (should match mod_experiment_config.f95)
        # ... (rest of init)
        self.TICKS_PER_YEAR = 100 
        
        self.running = True

    def run(self):
        if not mod_python_interface:
            self.error.emit("mod_python_interface not available")
            return

        try:
            # ... (setup code)
            # 1. Setup Simulation
            mod_python_interface.set_simulation_config_path(self.config_path)
            mod_python_interface.set_custom_hep_paths(self.hep_paths, len(self.hep_paths))
            
            if self.module_config:
                mod_python_interface.set_active_modules(np.array(self.module_config, dtype=np.int32), len(self.module_config))
            else:
                mod_python_interface.set_active_modules(np.array([], dtype=np.int32), 0)

            # Handle Spawn Points
            if self.spawn_points:
                self.setup_spawn_points()
            else:
                mod_python_interface.init_simulation()

            # 2. Calculate Ticks
            duration_years = self.end_year - self.start_year
            if duration_years <= 0:
                self.error.emit("End time must be greater than start time.")
                return

            total_ticks = int(duration_years * self.TICKS_PER_YEAR)
            
            # 3. Run Loop
            frames = []
            start_time = time.time()
            
            # Get grid dims for image generation
            dlon, dlat, npops = mod_python_interface.get_grid_dims()
            
            for t in range(1, total_ticks + 1):
                if not self.running:
                    break
                
                # Step
                mod_python_interface.step_simulation(t)
                
                # Update Progress every 10 ticks or so
                if t % 10 == 0:
                    count = mod_python_interface.get_agent_count()
                    elapsed = time.time() - start_time
                    progress = int((t / total_ticks) * 100)
                    self.progress_update.emit(progress, t, count, elapsed)
                
                # Capture Frame
                capture_interval = max(1, total_ticks // 200)
                
                if t % capture_interval == 0 and PIL_AVAILABLE:
                    frame = self.capture_frame(dlon, dlat, npops)
                    frames.append(frame)

            # 4. Save Video
            if PIL_AVAILABLE and frames:
                output_file = self.output_path if self.output_path else "simulation_output.gif"
                # Ensure directory exists
                out_dir = os.path.dirname(os.path.abspath(output_file))
                if not os.path.exists(out_dir) and out_dir:
                    os.makedirs(out_dir)
                    
                # Save as GIF
                frames[0].save(output_file, save_all=True, append_images=frames[1:], duration=50, loop=0)
                self.finished.emit(os.path.abspath(output_file))
            else:
                self.finished.emit("No video generated (PIL missing or no frames)")

        except Exception as e:
            import traceback
            traceback.print_exc()
            self.error.emit(str(e))

    def setup_spawn_points(self):
        # Logic copied from application.py
        points_by_pop = {}
        for p in self.spawn_points:
            pop = p['pop']
            if pop not in points_by_pop:
                points_by_pop[pop] = []
            points_by_pop[pop].append(p)
            
        max_sources = 0
        for pop in points_by_pop:
            max_sources = max(max_sources, len(points_by_pop[pop]))
            
        ns = max_sources
        
        x_ini = np.zeros((ns, self.npops), dtype=np.float64)
        y_ini = np.zeros((ns, self.npops), dtype=np.float64)
        spread = np.zeros((ns, self.npops), dtype=np.float64)
        counts = np.zeros((ns, self.npops), dtype=np.int32)
        
        for pop, points in points_by_pop.items():
            pop_idx = pop - 1 
            if pop_idx < self.npops:
                for i, p in enumerate(points):
                    x_ini[i, pop_idx] = p['x']
                    y_ini[i, pop_idx] = p['y']
                    spread[i, pop_idx] = p['spread']
                    counts[i, pop_idx] = p['count']
        
        mod_python_interface.init_simulation(True)
        mod_python_interface.set_spawn_configuration(x_ini, y_ini, spread, counts)
        mod_python_interface.regenerate_agents()

    def capture_frame(self, dlon, dlat, npops):
        # 1. Get HEP Data
        # We need to map this to an image.
        # HEP is (dlon, dlat, npops). We just take the first pop or sum?
        # application.py uses hep_data[:, :, 0]
        
        # We need to pass t_hep. In step_simulation we update it.
        # But get_simulation_hep takes t_hep index.
        # Let's just pass 1 as in application.py
        
        hep_data = mod_python_interface.get_simulation_hep(1, dlon, dlat, npops)
        # hep_data shape: (dlon, dlat, npops)
        
        # Normalize HEP to 0-255
        # Range is -1 to 1.
        # -1 -> Blue, 0 -> Yellow, 1 -> Green
        # Let's make a simple RGB image.
        
        grid = hep_data[:, :, 0] # (dlon, dlat)
        
        # Transpose to (dlat, dlon) for image (y, x)
        grid = grid.T
        
        # Create RGB array
        h, w = grid.shape
        rgb = np.zeros((h, w, 3), dtype=np.uint8)
        
        # Simple colormap
        # -1 (Water) -> Blue (0, 0, 255)
        # 0 (Land low) -> Yellow (255, 255, 150)
        # 1 (Land high) -> Green (100, 255, 100)
        
        # Mask
        water = grid < -0.5
        land = ~water
        
        rgb[water] = [0, 0, 255]
        
        # For land, interpolate? Or just simple threshold
        # Let's just do simple mapping for speed
        # Map -0.5 to 1.0 -> 0 to 255
        # Actually application.py uses a lookup table.
        # Let's just use Yellow for everything > -0.5 for now to keep it simple, 
        # or Green if > 0.5
        
        rgb[land] = [253, 253, 150] # Yellow
        rgb[grid > 0.5] = [119, 221, 119] # Green
        
        # Overlay Agents
        # We need agent positions.
        count = mod_python_interface.get_agent_count()
        if count > 0:
            x, y, pop = mod_python_interface.get_simulation_agents(count)
            # x, y are in grid coordinates (lon, lat)
            # We need to map to pixel coordinates.
            # Grid is (dlon, dlat).
            # Image is (h, w) = (dlat, dlon).
            # So x -> col (0..w), y -> row (0..h)
            
            # We need to subtract lon_0, lat_0 and divide by delta?
            # mod_python_interface.get_simulation_agents returns x, y in physical coords?
            # application.py: scatter_agents.setData(x=x, y=y)
            # And the plot has a transform: translate(lon_0, lat_0), scale(delta_lon, delta_lat)
            # So x, y are physical.
            
            lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep = mod_python_interface.get_simulation_config()
            
            # Map physical to grid index
            # grid_x = (x - lon_0) / delta_lon
            # grid_y = (y - lat_0) / delta_lat
            
            if delta_lon > 0 and delta_lat > 0:
                gx = ((x - lon_0) / delta_lon).astype(int)
                gy = ((y - lat_0) / delta_lat).astype(int)
                
                # Clip
                gx = np.clip(gx, 0, w - 1)
                gy = np.clip(gy, 0, h - 1)
                
                # Draw agents as red dots
                # Note: Image is (row, col) = (y, x)
                # So rgb[gy, gx]
                
                # We can't vector assign easily with duplicates, but loop is slow.
                # Let's just set them.
                for i in range(len(gx)):
                    if 0 <= gy[i] < h and 0 <= gx[i] < w:
                        rgb[gy[i], gx[i]] = [255, 0, 0] # Red
        
        # Flip Y?
        # Usually map origin is bottom-left, image origin is top-left.
        # If lat increases upwards, we need to flip the image rows.
        rgb = np.flipud(rgb)
        
        return Image.fromarray(rgb)

    def stop(self):
        self.running = False
