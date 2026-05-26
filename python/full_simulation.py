import sys
import os
import numpy as np
import time
from PyQt5 import QtCore, QtWidgets
import queue
import netCDF4
import multiprocessing
from dataclasses import dataclass


def show_selectable_error(parent, title, text):
    msg_box = QtWidgets.QMessageBox(parent)
    msg_box.setIcon(QtWidgets.QMessageBox.Critical)
    msg_box.setWindowTitle(title)
    msg_box.setText(text)
    msg_box.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
    msg_box.exec_()
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
        QtWidgets.QApplication.processEvents()
        
    def abort_simulation(self):
        self.btn_abort.setEnabled(False)
        self.lbl_progress.setText("Aborting Simulation...")
        if self.sim_thread.isRunning():
            self.sim_thread.stop()
            
    def on_package_pushed(self, total):
        self.packages_pushed = total
        self.save_progress_bar.setMaximum(total)
        self.update_save_label()
        QtWidgets.QApplication.processEvents()
        
    def on_package_saved(self, count):
        self.packages_saved += count
        self.save_progress_bar.setValue(self.packages_saved)
        self.update_save_label()
        QtWidgets.QApplication.processEvents()
        
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
                data_writer.package_saved.connect(self.package_saved.emit, QtCore.Qt.DirectConnection)
                data_writer.start()
            
            # Set up dead agent archiving if requested
            if self.store_dead_agents:
                mod_python_interface.set_forget_dead_agents(False)
                if self.dead_nc_path:
                    out_dir = os.path.dirname(os.path.abspath(self.dead_nc_path))
                    if out_dir and not os.path.exists(out_dir):
                        os.makedirs(out_dir)
                    dead_agent_writer = DeadAgentWriterThread(self.dead_nc_path)
                    dead_agent_writer.package_saved.connect(self.package_saved.emit, QtCore.Qt.DirectConnection)
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
            x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead, cluster_rank, creativity = mod_python_interface.get_simulation_agents(count)
            
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


def run_simulation_process(run_idx, start_year, end_year, save_interval, config_path, hep_paths, 
                            active_modules, spawn_points, age_distribution, clustering_alg, 
                            kmeans_k, dbscan_eps, dbscan_minpts, current_npops, 
                            store_dead_agents, gif_path, nc_path, dead_nc_path, progress_queue):
    import sys
    import os
    import numpy as np
    import time
    import queue
    import netCDF4
    
    # Try to import PIL for GIF generation
    try:
        from PIL import Image
        pil_available = True
    except ImportError:
        pil_available = False

    try:
        sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
        import mod_python_interface
        if hasattr(mod_python_interface, 'mod_python_interface'):
            mpi = mod_python_interface.mod_python_interface
        else:
            mpi = mod_python_interface
            
        if not mpi:
            progress_queue.put((run_idx, "error", "mod_python_interface not available"))
            return
            
        # 1. Initialize simulation context
        mpi.set_simulation_config_path(config_path.encode('utf-8'))
        
        encoded_hep_paths = [p.encode('utf-8') for p in hep_paths]
        mpi.set_custom_hep_paths(encoded_hep_paths, len(hep_paths))
        
        if active_modules:
            mpi.set_active_modules(np.array(active_modules, dtype=np.int32), len(active_modules))
        else:
            mpi.set_active_modules(np.array([], dtype=np.int32), 0)
            
        # 2. Check for custom spawn points
        npops = current_npops
        if spawn_points:
            points_by_pop = {}
            for p in spawn_points:
                pop = p['pop']
                if pop not in points_by_pop:
                    points_by_pop[pop] = []
                points_by_pop[pop].append(p)
                
            max_sources = max(len(pts) for pts in points_by_pop.values()) if points_by_pop else 0
            ns = max_sources
            
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
            
            mpi.init_sim_step_1()
            mpi.init_sim_step_2_part_1()
            mpi.init_sim_step_2_part_2_arrays_only()
            
            nx = mpi.get_grid_nx()
            for i in range(1, nx + 1, 10):
                end_sub = min(i + 9, nx)
                mpi.init_sim_step_2_part_2_chunk(i, end_sub)
                
            mpi.init_sim_step_2_part_3()
            
            if clustering_alg is not None:
                mpi.set_clustering_algorithm(clustering_alg)
                if clustering_alg == 2:
                    mpi.set_kmeans_clusters(kmeans_k)
                elif clustering_alg == 3:
                    mpi.set_dbscan_eps(dbscan_eps)
                    mpi.set_dbscan_minpts(dbscan_minpts)
            
            mpi.set_spawn_configuration(x_ini, y_ini, spread, counts, ns, npops)
        else:
            mpi.init_sim_step_1()
            mpi.init_sim_step_2_part_1()
            mpi.init_sim_step_2_part_2_arrays_only()
            
            nx = mpi.get_grid_nx()
            for i in range(1, nx + 1, 10):
                end_sub = min(i + 9, nx)
                mpi.init_sim_step_2_part_2_chunk(i, end_sub)
                
            mpi.init_sim_step_2_part_3()
            
            if clustering_alg is not None:
                mpi.set_clustering_algorithm(clustering_alg)
                if clustering_alg == 2:
                    mpi.set_kmeans_clusters(kmeans_k)
                elif clustering_alg == 3:
                    mpi.set_dbscan_eps(dbscan_eps)
                    mpi.set_dbscan_minpts(dbscan_minpts)
                    
        # Step 3: Generate Agents
        mpi.init_sim_step_3(False)
        
        # Step Age Distribution
        if age_distribution is not None:
            mpi.set_age_distribution_interface(age_distribution, len(age_distribution))
            mpi.init_sim_step_apply_age_dist()
            
        # Step 4: Verify
        mpi.init_sim_step_4()
        
        # Step 5: Run loop
        duration_years = end_year - start_year
        total_ticks = int(duration_years * 100) # 100 ticks per year
        
        # Get grid dimensions
        dlon, dlat, npops_actual = mpi.get_grid_dims()
        lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep = mpi.get_simulation_config()
        
        # Initialize Writers
        packages_total = 0
        
        if save_interval > 0 and nc_path is not None:
            out_dir = os.path.dirname(os.path.abspath(nc_path))
            if out_dir and not os.path.exists(out_dir):
                os.makedirs(out_dir)
            # Create NetCDF file in this process
            ds = netCDF4.Dataset(nc_path, 'w', format='NETCDF4')
            ds.createDimension('time', None)
            ds.createDimension('lon', dlon)
            ds.createDimension('lat', dlat)
            ds.createDimension('pop', npops_actual)
            
            var_tick = ds.createVariable('tick', 'i4', ('time',))
            var_hep = ds.createVariable('hep', 'f4', ('time', 'lon', 'lat', 'pop'), zlib=True, complevel=4)
            var_density = ds.createVariable('human_density', 'f8', ('time', 'lon', 'lat'), zlib=True, complevel=4)
            time_idx = 0
            
        if store_dead_agents and dead_nc_path:
            out_dir = os.path.dirname(os.path.abspath(dead_nc_path))
            if out_dir and not os.path.exists(out_dir):
                os.makedirs(out_dir)
            mpi.set_forget_dead_agents(False)
            
            ds_dead = netCDF4.Dataset(dead_nc_path, 'w', format='NETCDF4')
            ds_dead.createDimension('agent', None)
            
            var_id       = ds_dead.createVariable('agent_id',    'i4', ('agent',), zlib=True, complevel=4)
            var_x        = ds_dead.createVariable('pos_x',       'f8', ('agent',), zlib=True, complevel=4)
            var_y        = ds_dead.createVariable('pos_y',       'f8', ('agent',), zlib=True, complevel=4)
            var_pop      = ds_dead.createVariable('population',  'i4', ('agent',), zlib=True, complevel=4)
            var_age      = ds_dead.createVariable('age_ticks',   'i4', ('agent',), zlib=True, complevel=4)
            var_gender   = ds_dead.createVariable('gender',      'i4', ('agent',), zlib=True, complevel=4)
            var_res      = ds_dead.createVariable('resources',   'i4', ('agent',), zlib=True, complevel=4)
            var_children = ds_dead.createVariable('children',    'i4', ('agent',), zlib=True, complevel=4)
            var_tick_d   = ds_dead.createVariable('death_tick',   'i4', ('agent',), zlib=True, complevel=4)
            ds_dead.description = 'Dead agent archive from full simulation run'
            agent_idx = 0
        else:
            mpi.set_forget_dead_agents(True)
            
        frames = []
        start_time = time.time()
        
        def capture_frame_local():
            hep_data = mpi.get_simulation_hep(1, dlon, dlat, npops_actual)
            grid = hep_data[:, :, 0].T
            h, w = grid.shape
            rgb = np.zeros((h, w, 3), dtype=np.uint8)
            water = grid < -0.5
            land = ~water
            rgb[water] = [0, 0, 255]
            rgb[land] = [253, 253, 150]
            rgb[grid > 0.5] = [119, 221, 119]
            
            count = mpi.get_agent_count()
            if count > 0:
                x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, ux, uy, is_dead, cluster_rank, creativity = mpi.get_simulation_agents(count)
                if delta_lon > 0 and delta_lat > 0:
                    gx = ((x - lon_0) / delta_lon).astype(int)
                    gy = ((y - lat_0) / delta_lat).astype(int)
                    gx = np.clip(gx, 0, w - 1)
                    gy = np.clip(gy, 0, h - 1)
                    for i in range(len(gx)):
                        if 0 <= gy[i] < h and 0 <= gx[i] < w:
                            rgb[gy[i], gx[i]] = [255, 0, 0]
            rgb = np.flipud(rgb)
            return Image.fromarray(rgb)

        for t in range(1, total_ticks + 1):
            mpi.step_simulation(t)
            
            if t % 10 == 0:
                count = mpi.get_agent_count()
                elapsed = time.time() - start_time
                progress = int((t / total_ticks) * 100)
                progress_queue.put((run_idx, "progress", progress, t, count, elapsed))
                
            # Log NC data
            if save_interval > 0 and t % save_interval == 0:
                if 'ds' in locals():
                    hep_data = mpi.get_simulation_hep(1, dlon, dlat, npops_actual)
                    if hasattr(mpi, 'get_grid_density'):
                        density_data = mpi.get_grid_density(dlon, dlat)
                    else:
                        density_data = np.zeros((dlon, dlat))
                    var_tick[time_idx] = t
                    var_hep[time_idx, :, :, :] = hep_data
                    var_density[time_idx, :, :] = density_data
                    time_idx += 1
                    packages_total += 1
                    progress_queue.put((run_idx, "package_saved", packages_total))
                    
                if store_dead_agents and 'ds_dead' in locals():
                    dead_count = mpi.get_dead_agents_count()
                    if dead_count > 0:
                        dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_res, dead_children, dead_death_tick = \
                            mpi.get_dead_simulation_agents(dead_count)
                            
                        # Append to NetCDF variables
                        num_new = len(dead_id)
                        var_id[agent_idx:agent_idx+num_new] = dead_id
                        var_x[agent_idx:agent_idx+num_new] = dead_x
                        var_y[agent_idx:agent_idx+num_new] = dead_y
                        var_pop[agent_idx:agent_idx+num_new] = dead_pop
                        var_age[agent_idx:agent_idx+num_new] = dead_age
                        var_gender[agent_idx:agent_idx+num_new] = dead_gender
                        var_res[agent_idx:agent_idx+num_new] = dead_res
                        var_children[agent_idx:agent_idx+num_new] = dead_children
                        var_tick_d[agent_idx:agent_idx+num_new] = dead_death_tick
                        
                        agent_idx += num_new
                        mpi.clear_dead_agents()
                        packages_total += 1
                        progress_queue.put((run_idx, "package_saved", packages_total))

            capture_interval = max(1, total_ticks // 200)
            if t % capture_interval == 0 and pil_available:
                frame = capture_frame_local()
                frames.append(frame)
                
        # Close NetCDF files
        if 'ds' in locals():
            try:
                ds.sync()
            except Exception:
                pass
            try:
                ds.close()
            except Exception as ce:
                print(f"Note: Error closing main NetCDF dataset: {ce}")
            
        if store_dead_agents and 'ds_dead' in locals():
            # One last extraction
            dead_count = mpi.get_dead_agents_count()
            if dead_count > 0:
                dead_id, dead_x, dead_y, dead_pop, dead_age, dead_gender, dead_res, dead_children, dead_death_tick = \
                    mpi.get_dead_simulation_agents(dead_count)
                num_new = len(dead_id)
                var_id[agent_idx:agent_idx+num_new] = dead_id
                var_x[agent_idx:agent_idx+num_new] = dead_x
                var_y[agent_idx:agent_idx+num_new] = dead_y
                var_pop[agent_idx:agent_idx+num_new] = dead_pop
                var_age[agent_idx:agent_idx+num_new] = dead_age
                var_gender[agent_idx:agent_idx+num_new] = dead_gender
                var_res[agent_idx:agent_idx+num_new] = dead_res
                var_children[agent_idx:agent_idx+num_new] = dead_children
                var_tick_d[agent_idx:agent_idx+num_new] = dead_death_tick
                agent_idx += num_new
                mpi.clear_dead_agents()
            try:
                ds_dead.sync()
            except Exception:
                pass
            try:
                ds_dead.close()
            except Exception as ce:
                print(f"Note: Error closing dead agents NetCDF dataset: {ce}")
            mpi.set_forget_dead_agents(True)
            
        # Save GIF
        msg = []
        if pil_available and frames:
            # Ensure directory exists
            out_dir = os.path.dirname(os.path.abspath(gif_path))
            if out_dir and not os.path.exists(out_dir):
                os.makedirs(out_dir)
            frames[0].save(gif_path, save_all=True, append_images=frames[1:], duration=50, loop=0)
            msg.append(f"Video saved to: {os.path.abspath(gif_path)}")
        else:
            msg.append("No video generated.")
            
        if save_interval > 0 and nc_path:
            msg.append(f"Grid data saved to: {os.path.abspath(nc_path)}")
        if store_dead_agents and dead_nc_path:
            msg.append(f"Dead agents saved to: {os.path.abspath(dead_nc_path)}")
            
        progress_queue.put((run_idx, "finished", "\n".join(msg)))
        
    except Exception as e:
        import traceback
        err_msg = f"{str(e)}\n{traceback.format_exc()}"
        progress_queue.put((run_idx, "error", err_msg))


class MultiSimulationWindow(QtWidgets.QMainWindow):
    def __init__(self, num_sims, start_year, end_year, save_interval, output_path, store_dead_agents,
                 config_path, hep_paths, active_modules, spawn_points, age_dist,
                 clustering_alg, kmeans_k, dbscan_eps, dbscan_minpts, current_npops):
        super().__init__()
        self.setWindowTitle("Parallel Simulation Suite Manager")
        self.resize(650, 500)
        
        self.num_sims = num_sims
        self.start_year = start_year
        self.end_year = end_year
        self.save_interval = save_interval
        self.output_path = output_path
        self.store_dead_agents = store_dead_agents
        self.config_path = config_path
        self.hep_paths = hep_paths
        self.active_modules = active_modules
        self.spawn_points = spawn_points
        self.age_dist = age_dist
        self.clustering_alg = clustering_alg
        self.kmeans_k = kmeans_k
        self.dbscan_eps = dbscan_eps
        self.dbscan_minpts = dbscan_minpts
        self.current_npops = current_npops
        
        # Scheduler state
        self.max_parallel = max(1, os.cpu_count() - 1)
        self.active_processes = {}  # run_idx -> Process
        self.run_statuses = {i: "queued" for i in range(1, num_sims + 1)}  # run_idx -> status string
        
        # Central layout
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.main_layout = QtWidgets.QVBoxLayout(self.central_widget)
        
        # Title Header
        self.lbl_title = QtWidgets.QLabel(f"Running {num_sims} Simulations (Max Parallel: {self.max_parallel} processes)")
        self.lbl_title.setStyleSheet("font-size: 14px; font-weight: bold; color: #00FFCC; margin-bottom: 5px;")
        self.main_layout.addWidget(self.lbl_title)
        
        # Scroll area for individual runs
        self.scroll_area = QtWidgets.QScrollArea()
        self.scroll_area.setWidgetResizable(True)
        self.scroll_widget = QtWidgets.QWidget()
        self.scroll_layout = QtWidgets.QVBoxLayout(self.scroll_widget)
        self.scroll_layout.setSpacing(10)
        
        self.progress_bars = {}
        self.status_labels = {}
        self.save_labels = {}
        
        for i in range(1, num_sims + 1):
            group_box = QtWidgets.QGroupBox(f"Simulation Run #{i}")
            group_box.setStyleSheet("QGroupBox { font-weight: bold; border: 1px solid #334444; border-radius: 4px; margin-top: 10px; padding: 10px; } QGroupBox::title { subcontrol-origin: margin; left: 10px; padding: 0 3px; color: #00FFCC; }")
            group_layout = QtWidgets.QVBoxLayout(group_box)
            
            pbar = QtWidgets.QProgressBar()
            pbar.setValue(0)
            pbar.setStyleSheet("QProgressBar { border: 1px solid #334444; border-radius: 3px; text-align: center; color: white; background-color: #112222; } QProgressBar::chunk { background-color: #0088AA; }")
            group_layout.addWidget(pbar)
            self.progress_bars[i] = pbar
            
            status_lbl = QtWidgets.QLabel("Status: Queued")
            status_lbl.setStyleSheet("font-size: 10px; color: #889999;")
            status_lbl.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
            group_layout.addWidget(status_lbl)
            self.status_labels[i] = status_lbl
            
            save_lbl = QtWidgets.QLabel("Packages Saved: 0")
            save_lbl.setStyleSheet("font-size: 10px; color: #889999;")
            save_lbl.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
            group_layout.addWidget(save_lbl)
            self.save_labels[i] = save_lbl
            
            self.scroll_layout.addWidget(group_box)
            
        self.scroll_layout.addStretch()
        self.scroll_area.setWidget(self.scroll_widget)
        self.main_layout.addWidget(self.scroll_area)
        
        # Bottom controls
        self.btn_layout = QtWidgets.QHBoxLayout()
        
        self.btn_abort = QtWidgets.QPushButton("Abort All")
        self.btn_abort.setStyleSheet("background-color: #D32F2F; color: white; font-weight: bold; height: 35px; border-radius: 4px;")
        self.btn_abort.clicked.connect(self.abort_all)
        self.btn_layout.addWidget(self.btn_abort)
        
        self.btn_close = QtWidgets.QPushButton("Close Window")
        self.btn_close.setEnabled(False)
        self.btn_close.setStyleSheet("background-color: #37474F; color: white; font-weight: bold; height: 35px; border-radius: 4px;")
        self.btn_close.clicked.connect(self.close)
        self.btn_layout.addWidget(self.btn_close)
        
        self.main_layout.addLayout(self.btn_layout)
        
        # Shared multiprocessing Queue
        self.progress_queue = multiprocessing.Queue()
        
        # Start QTimer for polling and scheduling
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.update_loop)
        self.timer.start(100)  # poll every 100ms
        
    def update_loop(self):
        # 1. Drain progress queue
        while not self.progress_queue.empty():
            try:
                run_idx, packet_type, *args = self.progress_queue.get_nowait()
                if packet_type == "progress":
                    progress, t, count, elapsed = args
                    self.progress_bars[run_idx].setValue(progress)
                    self.status_labels[run_idx].setText(f"Running | Tick: {t} | Agents: {count} | Time: {elapsed:.1f}s")
                elif packet_type == "package_saved":
                    saved = args[0]
                    self.save_labels[run_idx].setText(f"Packages Saved: {saved}")
                elif packet_type == "finished":
                    msg = args[0]
                    self.progress_bars[run_idx].setValue(100)
                    self.status_labels[run_idx].setText("Finished successfully.")
                    self.save_labels[run_idx].setText("Data and media files saved.")
                    self.run_statuses[run_idx] = "finished"
                    self.active_processes.pop(run_idx, None)
                elif packet_type == "error":
                    err_msg = args[0]
                    self.progress_bars[run_idx].setValue(100)
                    self.status_labels[run_idx].setText(f"Error: {err_msg}")
                    self.run_statuses[run_idx] = "error"
                    self.active_processes.pop(run_idx, None)
                    show_selectable_error(self, f"Simulation Run #{run_idx} Error", err_msg)
            except Exception:
                break
                
        # 2. Schedule waiting tasks
        active_count = len(self.active_processes)
        if active_count < self.max_parallel:
            # Launch the next queued run
            for i in range(1, self.num_sims + 1):
                if self.run_statuses[i] == "queued":
                    self.run_statuses[i] = "running"
                    self.status_labels[i].setText("Initializing...")
                    self.progress_bars[i].setStyleSheet("QProgressBar { border: 1px solid #334444; border-radius: 3px; text-align: center; color: white; background-color: #112222; } QProgressBar::chunk { background-color: #00FFCC; }")
                    
                    # Generate separate filenames for this run
                    base_name = os.path.splitext(self.output_path)[0] if self.output_path else "simulation_output"
                    gif_path = f"{base_name}_run{i}.gif"
                    nc_path = f"{base_name}_run{i}.nc"
                    dead_nc_path = f"{base_name}_run{i}_dead_agents.nc"
                    
                    p = multiprocessing.Process(
                        target=run_simulation_process,
                        args=(
                            i, self.start_year, self.end_year, self.save_interval,
                            self.config_path, self.hep_paths, self.active_modules,
                            self.spawn_points, self.age_dist, self.clustering_alg,
                            self.kmeans_k, self.dbscan_eps, self.dbscan_minpts, self.current_npops,
                            self.store_dead_agents, gif_path, nc_path, dead_nc_path,
                            self.progress_queue
                        )
                    )
                    p.start()
                    self.active_processes[i] = p
                    break  # Launch one process per timer tick to prevent CPU spike
                    
        # 3. Check if all completed
        all_done = all(status in ("finished", "error", "aborted") for status in self.run_statuses.values())
        if all_done:
            self.timer.stop()
            self.btn_abort.setEnabled(False)
            self.btn_close.setEnabled(True)
            self.btn_close.setStyleSheet("background-color: #00E676; color: black; font-weight: bold; height: 35px; border-radius: 4px;")
            QtWidgets.QMessageBox.information(self, "Parallel Suite Complete", "All parallel simulation runs have completed.")
            
    def abort_all(self):
        self.timer.stop()
        self.btn_abort.setEnabled(False)
        for i, p in list(self.active_processes.items()):
            if p.is_alive():
                p.terminate()
                p.join()
            self.run_statuses[i] = "aborted"
            self.status_labels[i].setText("Aborted.")
            self.progress_bars[i].setValue(0)
            self.progress_bars[i].setStyleSheet("QProgressBar { border: 1px solid #334444; border-radius: 3px; text-align: center; color: white; background-color: #112222; } QProgressBar::chunk { background-color: #D32F2F; }")
            
        for i in range(1, self.num_sims + 1):
            if self.run_statuses[i] == "queued":
                self.run_statuses[i] = "aborted"
                self.status_labels[i].setText("Queued run aborted.")
                
        self.active_processes.clear()
        self.btn_close.setEnabled(True)
        self.btn_close.setStyleSheet("background-color: #00E676; color: black; font-weight: bold; height: 35px; border-radius: 4px;")
        QtWidgets.QMessageBox.warning(self, "Aborted", "All active and queued parallel simulations were aborted.")
        
    def closeEvent(self, event):
        self.timer.stop()
        # Ensure child processes are killed
        for i, p in list(self.active_processes.items()):
            if p.is_alive():
                p.terminate()
                p.join()
        event.accept()
