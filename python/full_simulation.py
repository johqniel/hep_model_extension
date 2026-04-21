import sys
import os
import numpy as np
import time
from PyQt5 import QtCore, QtWidgets
import queue
import netCDF4
from dataclasses import dataclass
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

@dataclass
class DeadAgentPackage:
    tick: int
    id: np.ndarray
    x: np.ndarray
    y: np.ndarray
    pop: np.ndarray
    age: np.ndarray
    gender: np.ndarray
    resources: np.ndarray
    children: np.ndarray
    death_tick: np.ndarray

class DataWriterThread(QtCore.QThread):
    package_saved = QtCore.pyqtSignal(int)
    
    def __init__(self, output_nc_path, dlon, dlat, npops):
        super().__init__()
        self.output_nc_path = output_nc_path
        self.dlon = dlon
        self.dlat = dlat
        self.npops = npops
        self.data_queue = queue.Queue(maxsize=1000) # prevent memory explosion
        self.running = True

    def run(self):
        try:
            # Open NetCDF dataset
            ds = netCDF4.Dataset(self.output_nc_path, 'w', format='NETCDF4')
            
            # Create dimensions
            ds.createDimension('time', None) # unlimited time dimension
            ds.createDimension('lon', self.dlon)
            ds.createDimension('lat', self.dlat)
            ds.createDimension('pop', self.npops)
            
            # Create variables
            var_tick = ds.createVariable('tick', 'i4', ('time',))
            var_hep = ds.createVariable('hep', 'f4', ('time', 'lon', 'lat', 'pop'), zlib=True, complevel=4)
            var_density = ds.createVariable('human_density', 'f8', ('time', 'lon', 'lat'), zlib=True, complevel=4)
            
            time_idx = 0
            while self.running or not self.data_queue.empty():
                try:
                    # Timeout so we check self.running periodically
                    item = self.data_queue.get(timeout=1.0)
                    if item is None: # Sentinel
                        break
                        
                    tick = item['tick']
                    hep = item['hep']
                    density = item['density']
                    
                    var_tick[time_idx] = tick
                    var_hep[time_idx, :, :, :] = hep
                    var_density[time_idx, :, :] = density
                    
                    time_idx += 1
                    self.data_queue.task_done()
                    self.package_saved.emit(1) # Emit 1 for incremental progress
                    
                except queue.Empty:
                    continue
                    
        except Exception as e:
            print(f"DataWriterThread error: {e}")
        finally:
            if 'ds' in locals():
                ds.close()

    def stop(self):
        self.running = False
        self.data_queue.put(None) # push sentinel


class DeadAgentWriterThread(QtCore.QThread):
    """Background thread that writes dead agent data to a NetCDF file.
    
    The dead agent data is ungridded (1D arrays of agent properties),
    so it uses its own file with an unlimited 'agent' dimension.
    """
    package_saved = QtCore.pyqtSignal(int)
    
    def __init__(self, output_nc_path):
        super().__init__()
        self.output_nc_path = output_nc_path
        self.data_queue = queue.Queue(maxsize=1000)
        self.running = True

    def run(self):
        try:
            ds = netCDF4.Dataset(self.output_nc_path, 'w', format='NETCDF4')
            
            # Unlimited dimension for agents
            ds.createDimension('agent', None)
            
            # Variables
            var_id       = ds.createVariable('agent_id',    'i4', ('agent',), zlib=True, complevel=4)
            var_x        = ds.createVariable('pos_x',       'f8', ('agent',), zlib=True, complevel=4)
            var_y        = ds.createVariable('pos_y',       'f8', ('agent',), zlib=True, complevel=4)
            var_pop      = ds.createVariable('population',  'i4', ('agent',), zlib=True, complevel=4)
            var_age      = ds.createVariable('age_ticks',   'i4', ('agent',), zlib=True, complevel=4)
            var_gender   = ds.createVariable('gender',      'i4', ('agent',), zlib=True, complevel=4)
            var_res      = ds.createVariable('resources',   'i4', ('agent',), zlib=True, complevel=4)
            var_children = ds.createVariable('children',    'i4', ('agent',), zlib=True, complevel=4)
            var_tick     = ds.createVariable('death_tick',   'i4', ('agent',), zlib=True, complevel=4)
            
            ds.description = 'Dead agent archive from full simulation run'
            
            agent_idx = 0
            packages = 0
            while self.running or not self.data_queue.empty():
                try:
                    item = self.data_queue.get(timeout=1.0)
                    if item is None:  # Sentinel
                        break
                    
                    if not isinstance(item, DeadAgentPackage):
                        print(f"Warning: Unexpected item type in DeadAgentWriterThread: {type(item)}")
                        self.data_queue.task_done()
                        continue

                    tick = item.tick
                    ids = item.id
                    xs = item.x
                    ys = item.y
                    pops = item.pop
                    ages = item.age
                    genders = item.gender
                    resources = item.resources
                    children = item.children
                    death_ticks = item.death_tick
                    
                    n = len(ids)
                    if n > 0:
                        end_idx = agent_idx + n
                        var_id[agent_idx:end_idx]       = ids
                        var_x[agent_idx:end_idx]        = xs
                        var_y[agent_idx:end_idx]        = ys
                        var_pop[agent_idx:end_idx]      = pops
                        var_age[agent_idx:end_idx]      = ages
                        var_gender[agent_idx:end_idx]   = genders
                        var_res[agent_idx:end_idx]      = resources
                        var_children[agent_idx:end_idx] = children
                        var_tick[agent_idx:end_idx]     = death_ticks
                        agent_idx = end_idx
                    
                    packages += 1
                    self.data_queue.task_done()
                    self.package_saved.emit(1) # Emit 1 for incremental progress
                    
                except queue.Empty:
                    continue
                    
        except Exception as e:
            print(f"DeadAgentWriterThread error: {e}")
            import traceback
            traceback.print_exc()
        finally:
            if 'ds' in locals():
                ds.close()
                print(f"Dead agent NetCDF closed. Total agents written: {agent_idx}")

    def stop(self):
        self.running = False
        self.data_queue.put(None)


class FullSimulationWindow(QtWidgets.QMainWindow):
    def __init__(self, start_year, end_year, save_interval, output_path, store_dead_agents=False):
        super().__init__()
        self.setWindowTitle("Full Simulation Progress")
        self.resize(500, 200)
        
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.layout = QtWidgets.QVBoxLayout(self.central_widget)
        
        self.lbl_progress = QtWidgets.QLabel("Initializing...")
        self.layout.addWidget(self.lbl_progress)
        
        self.progress_bar = QtWidgets.QProgressBar()
        self.layout.addWidget(self.progress_bar)
        
        self.lbl_status = QtWidgets.QLabel("")
        self.layout.addWidget(self.lbl_status)
        
        # Save Progress
        self.lbl_save_status = QtWidgets.QLabel("Saved: 0 / 0 packages")
        self.layout.addWidget(self.lbl_save_status)
        
        self.save_progress_bar = QtWidgets.QProgressBar()
        self.save_progress_bar.setValue(0)
        self.layout.addWidget(self.save_progress_bar)
        
        # Buttons
        self.btn_layout = QtWidgets.QHBoxLayout()
        
        self.btn_abort = QtWidgets.QPushButton("Abort")
        self.btn_abort.setStyleSheet("background-color: red; color: white; font-weight: bold;")
        self.btn_abort.clicked.connect(self.abort_simulation)
        self.btn_layout.addWidget(self.btn_abort)
        
        self.btn_close = QtWidgets.QPushButton("Close Window")
        self.btn_close.setEnabled(False)
        self.btn_close.clicked.connect(self.close)
        self.btn_layout.addWidget(self.btn_close)
        
        self.layout.addLayout(self.btn_layout)
        
        # Determine paths
        base_name = os.path.splitext(output_path)[0] if output_path else "simulation_output"
        gif_path = base_name + ".gif"
        nc_path = base_name + ".nc"
        dead_nc_path = base_name + "_dead_agents.nc"
        
        self.sim_thread = HeadlessSimulationThread(start_year, end_year, save_interval, gif_path, nc_path, 
                                                   store_dead_agents=store_dead_agents, dead_nc_path=dead_nc_path)
        self.sim_thread.progress_update.connect(self.on_sim_progress)
        self.sim_thread.finished.connect(self.on_sim_finished)
        self.sim_thread.error.connect(self.on_sim_error)
        
        self.sim_thread.package_pushed.connect(self.on_package_pushed)
        self.sim_thread.package_saved.connect(self.on_package_saved)
        self.sim_thread.queue_update.connect(self.on_queue_update)
        
        self.data_q_size = -1
        self.dead_q_size = -1
        self.sim_thread.start()
        
        self.packages_pushed = 0
        self.packages_saved = 0
        
    def on_sim_progress(self, progress, tick, agents, elapsed):
        self.progress_bar.setValue(progress)
        self.lbl_status.setText(f"Tick: {tick} | Agents: {agents} | Time: {elapsed:.1f}s")
        
    def abort_simulation(self):
        self.btn_abort.setEnabled(False)
        self.lbl_progress.setText("Aborting Simulation...")
        if self.sim_thread.isRunning():
            self.sim_thread.stop()
            
    def on_package_pushed(self, total):
        self.packages_pushed = total
        self.save_progress_bar.setMaximum(total)
        self.update_save_label()
        
    def on_package_saved(self, count):
        self.packages_saved += count
        self.save_progress_bar.setValue(self.packages_saved)
        self.update_save_label()
        
    def on_queue_update(self, data_q, dead_q):
        self.data_q_size = data_q
        self.dead_q_size = dead_q
        self.update_save_label()
        
    def update_save_label(self):
        txt = f"Saved: {self.packages_saved} / {self.packages_pushed} packages"
        if hasattr(self, 'data_q_size') and self.data_q_size >= 0:
            txt += f" | Grid Q: {self.data_q_size}/1000"
        if hasattr(self, 'dead_q_size') and self.dead_q_size >= 0:
            txt += f" | Dead Q: {self.dead_q_size}/1000"
        self.lbl_save_status.setText(txt)
        
    def on_sim_finished(self, out_msg):
        self.lbl_progress.setText("Simulation Complete")
        self.progress_bar.setValue(100)
        self.btn_abort.hide()
        self.btn_close.setEnabled(True)
        QtWidgets.QMessageBox.information(self, "Success", f"Simulation finished.\n{out_msg}")
        
    def on_sim_error(self, error_msg):
        self.lbl_progress.setText("Error")
        self.btn_abort.hide()
        self.btn_close.setEnabled(True)
        QtWidgets.QMessageBox.critical(self, "Error", f"Simulation failed: {error_msg}")
        
    def closeEvent(self, event):
        if self.sim_thread.isRunning():
            self.sim_thread.stop()
            self.sim_thread.wait()
        event.accept()


class HeadlessSimulationThread(QtCore.QThread):
    progress_update = QtCore.pyqtSignal(int, int, int, float) # progress %, current_tick, agent_count, time_elapsed
    finished = QtCore.pyqtSignal(str) # output filename
    error = QtCore.pyqtSignal(str)
    package_pushed = QtCore.pyqtSignal(int)
    package_saved = QtCore.pyqtSignal(int)
    queue_update = QtCore.pyqtSignal(int, int) # data_writer qsize, dead_agent_writer qsize

    def __init__(self, start_year, end_year, save_interval, output_path, nc_path, 
                 store_dead_agents=False, dead_nc_path=None):
        super().__init__()
        self.start_year = start_year
        self.end_year = end_year
        self.save_interval = save_interval
        self.output_path = output_path
        self.nc_path = nc_path
        self.store_dead_agents = store_dead_agents
        self.dead_nc_path = dead_nc_path
        
        # Constants
        self.TICKS_PER_YEAR = 100 
        
        self.running = True

    def run(self):
        if not mod_python_interface:
            self.error.emit("mod_python_interface not available")
            return

        try:
            # 1. Memory has already been perfectly initialized in application.py prepare_simulation
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
            
            data_writer = None
            dead_agent_writer = None
            packages_total = 0
            
            if self.save_interval > 0 and self.nc_path is not None:
                out_dir = os.path.dirname(os.path.abspath(self.nc_path))
                if out_dir and not os.path.exists(out_dir):
                    os.makedirs(out_dir)
                data_writer = DataWriterThread(self.nc_path, dlon, dlat, npops)
                data_writer.package_saved.connect(self.package_saved.emit)
                data_writer.start()
            
            # Set up dead agent archiving if requested
            if self.store_dead_agents:
                mod_python_interface.set_forget_dead_agents(False)
                if self.dead_nc_path:
                    out_dir = os.path.dirname(os.path.abspath(self.dead_nc_path))
                    if out_dir and not os.path.exists(out_dir):
                        os.makedirs(out_dir)
                    dead_agent_writer = DeadAgentWriterThread(self.dead_nc_path)
                    dead_agent_writer.package_saved.connect(self.package_saved.emit)
                    dead_agent_writer.start()
                    print(f"Dead agent archiving enabled. Output: {self.dead_nc_path}")
            else:
                mod_python_interface.set_forget_dead_agents(True)

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
                    
                    # Also emit queue sizes for UI
                    q_data = data_writer.data_queue.qsize() if data_writer else -1
                    q_dead = dead_agent_writer.data_queue.qsize() if dead_agent_writer else -1
                    self.queue_update.emit(q_data, q_dead)
                
                # Check for NC data logging
                if self.save_interval > 0 and t % self.save_interval == 0:
                    if data_writer and data_writer.running:
                        try:
                            hep_data = mod_python_interface.get_simulation_hep(1, dlon, dlat, npops)
                            if hasattr(mod_python_interface, 'get_grid_density'):
                                density_data = mod_python_interface.get_grid_density(dlon, dlat)
                            else:
                                density_data = np.zeros((dlon, dlat))
                                
                            data_writer.data_queue.put({
                                'tick': t,
                                'hep': hep_data.copy(),
                                'density': density_data.copy()
                            }, timeout=1.0)
                            packages_total += 1
                            self.package_pushed.emit(packages_total)
                        except queue.Full:
                            print("Warning: DataWriter queue is full. Skipping tick.")
                        except Exception as e:
                            print(f"Error fetching data for NC writer: {e}")
                    
                    # Extract dead agents at the same interval
                    if self.store_dead_agents and dead_agent_writer and dead_agent_writer.running:
                        try:
                            dead_count = mod_python_interface.get_dead_agents_count()
                            if dead_count > 0:
                                dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_res, dead_children, dead_death_tick = \
                                    mod_python_interface.get_dead_simulation_agents(dead_count)
                                
                                package = DeadAgentPackage(
                                    tick=t,
                                    id=dead_id.copy(),
                                    x=dead_x.copy(),
                                    y=dead_y.copy(),
                                    pop=dead_pop.copy(),
                                    age=dead_age.copy(),
                                    gender=dead_gender.copy(),
                                    resources=dead_res.copy(),
                                    children=dead_children.copy(),
                                    death_tick=dead_death_tick.copy()
                                )
                                
                                dead_agent_writer.data_queue.put(package, timeout=1.0)
                                mod_python_interface.clear_dead_agents()
                                
                                # Track progress for dead agents
                                packages_total += 1
                                self.package_pushed.emit(packages_total)

                        except queue.Full:
                            print("Warning: DeadAgentWriter queue is full. Skipping extraction.")
                        except Exception as e:
                            print(f"Error extracting dead agents: {e}")

                # Capture Frame
                capture_interval = max(1, total_ticks // 200)
                
                if t % capture_interval == 0 and PIL_AVAILABLE:
                    frame = self.capture_frame(dlon, dlat, npops)
                    frames.append(frame)

            # 4. Save Video
            if data_writer:
                data_writer.stop()
                data_writer.wait()
            
            # Flush remaining dead agents and close writer
            if self.store_dead_agents and dead_agent_writer:
                # One final extraction for any remaining parked agents
                try:
                    dead_count = mod_python_interface.get_dead_agents_count()
                    if dead_count > 0:
                        dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_res, dead_children, dead_death_tick = \
                            mod_python_interface.get_dead_simulation_agents(dead_count)
                        
                        package = DeadAgentPackage(
                            tick=t if 't' in locals() else -1,
                            id=dead_id.copy(),
                            x=dead_x.copy(),
                            y=dead_y.copy(),
                            pop=dead_pop.copy(),
                            age=dead_age.copy(),
                            gender=dead_gender.copy(),
                            resources=dead_res.copy(),
                            children=dead_children.copy(),
                            death_tick=dead_death_tick.copy()
                        )
                        
                        dead_agent_writer.data_queue.put(package, timeout=1.0)
                        mod_python_interface.clear_dead_agents()
                        
                        packages_total += 1
                        self.package_pushed.emit(packages_total)

                except Exception as e:
                    print(f"Error in final dead agent extraction: {e}")
                
                dead_agent_writer.stop()
                dead_agent_writer.wait()
                # Reset Fortran flag
                mod_python_interface.set_forget_dead_agents(True)

            msg = []
            if PIL_AVAILABLE and frames:
                output_file = self.output_path if self.output_path else "simulation_output.gif"
                # Ensure directory exists
                out_dir = os.path.dirname(os.path.abspath(output_file))
                if not os.path.exists(out_dir) and out_dir:
                    os.makedirs(out_dir)
                    
                # Save as GIF
                frames[0].save(output_file, save_all=True, append_images=frames[1:], duration=50, loop=0)
                msg.append(f"Video saved to: {os.path.abspath(output_file)}")
            else:
                msg.append("No video generated (PIL missing or no frames).")
            
            if self.save_interval > 0 and self.nc_path is not None:
                msg.append(f"Grid data saved to: {os.path.abspath(self.nc_path)}")
            
            if self.store_dead_agents and self.dead_nc_path:
                msg.append(f"Dead agents saved to: {os.path.abspath(self.dead_nc_path)}")
                
            self.finished.emit("\n".join(msg))

        except Exception as e:
            import traceback
            traceback.print_exc()
            self.error.emit(str(e))


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
            # Full unpack (12 values)
            x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead = mod_python_interface.get_simulation_agents(count)
            
            # Write frames
            # We need to map positions to pixel coords if we want a visual output or just dump data?
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
