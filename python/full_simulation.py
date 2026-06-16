import sys
import os
import numpy as np
import time
from PyQt5 import QtCore, QtWidgets
import queue
import netCDF4
import multiprocessing
import tempfile
try:
    multiprocessing.set_start_method('spawn')
except RuntimeError:
    pass
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
    def __init__(self, start_year, end_year, save_interval, output_path, store_dead_agents=False, store_grid_data=True):
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
        
        self.sim_thread = HeadlessSimulationThread(start_year, end_year, save_interval, gif_path, 
                                                   nc_path if store_grid_data else None, 
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



def resolve_series_value(series, mpi, npops, t, tick_elapsed_total,
                         x, y, pop, age, gender, resources, children,
                         is_pregnant, avg_resources, ux, uy, is_dead,
                         cluster_rank, creativity, count):
    """Standalone version of SimulationWindow._resolve_series_value for use in subprocesses.

    Mirrors the logic in simulation.py so that plot definitions defined in the
    main-app viewport can be sampled inside run_simulation_process.

    REDUNDANCY NOTE
    ---------------
    This function is a NECESSARY DUPLICATE of SimulationWindow._resolve_series_value
    in simulation.py. It exists because run_simulation_process runs in a detached
    subprocess (spawn context) with no Qt/GUI available, so SimulationWindow cannot
    be instantiated. Any change to metric calculations in simulation.py MUST be
    mirrored here. Future refactor: extract both into a shared plot_utils.py module.
    """
    import numpy as np

    def get_data(var_name):
        if var_name == 'age':          return age
        elif var_name == 'resources':  return resources
        elif var_name == 'children':   return children
        elif var_name == 'population': return pop
        elif var_name == 'is_pregnant': return is_pregnant
        elif var_name == 'avg_resources': return avg_resources
        elif var_name == 'gender':     return gender
        elif var_name == 'ux':         return ux
        elif var_name == 'uy':         return uy
        elif var_name == 'is_dead':    return is_dead
        elif var_name == 'cluster_rank': return cluster_rank
        elif var_name == 'creativity': return creativity
        return None

    source   = series.get('source', 'agents')
    var_name = series['variable']
    agg      = series.get('aggregation', 'mean')

    # --- Global source ---
    if source == 'global':
        if var_name == 'agent_count':
            return float(count)
        elif var_name == 'avg_ms_per_tick':
            return (tick_elapsed_total / t * 1000) if t > 0 else 0.0
        elif var_name in ['k_fertility', 'phi_death_acc', 'phi_birth_acc', 'n_alive_acc', 'avg_creativity']:
            pop_idx = int(series.get('population', 0))
            if pop_idx == -1:
                max_pop, max_n = 1, -1
                for p in range(1, npops + 1):
                    _, _, _, n, _ = mpi.get_dynamic_state_stats(0, p)
                    if n > max_n:
                        max_n = n; max_pop = p
                pop_idx = max_pop
            elif pop_idx == -2:
                total_w, weighted = 0.0, 0.0
                for p in range(1, npops + 1):
                    k_f, p_d, p_b, n_a, a_c = mpi.get_dynamic_state_stats(0, p)
                    w = float(n_a)
                    if w > 0:
                        v = {'k_fertility': float(k_f), 'phi_death_acc': float(p_d),
                             'phi_birth_acc': float(p_b), 'n_alive_acc': w,
                             'avg_creativity': float(a_c)}.get(var_name, 0.0)
                        weighted += w * v; total_w += w
                return (weighted / total_w) if total_w > 0 else 0.0
            k_fert, p_death, p_birth, n_alive, avg_creat = mpi.get_dynamic_state_stats(0, pop_idx)
            return {'k_fertility': float(k_fert), 'phi_death_acc': float(p_death),
                    'phi_birth_acc': float(p_birth), 'n_alive_acc': float(n_alive),
                    'avg_creativity': float(avg_creat)}.get(var_name, 0.0)
        elif var_name in ['death_natural','death_starvation','death_oob','death_conflict','death_random']:
            natural, starv, oob, confl, rnd, _, _, _ = mpi.get_debug_stats()
            return {'death_natural': float(natural), 'death_starvation': float(starv),
                    'death_oob': float(oob), 'death_conflict': float(confl),
                    'death_random': float(rnd)}.get(var_name, 0.0)
        elif var_name.startswith('perf_') and var_name != 'perf_active_modules':
            try:
                _, p_avg, a_avg, c_avg, g_dens, g_flows, g_smooth, g_hep, cl_avg, t_avg = mpi.get_performance_stats()
                perf_map = {
                    'perf_permanent': p_avg, 'perf_active': a_avg, 'perf_compaction': c_avg,
                    'perf_grid_density': g_dens, 'perf_grid_flows': g_flows,
                    'perf_grid_smoothing': g_smooth, 'perf_grid_hep': g_hep,
                    'perf_clustering': cl_avg, 'perf_total': t_avg,
                }
                return perf_map.get(var_name, 0.0) * 1000.0
            except Exception:
                return 0.0
        elif var_name == 'perf_active_modules':
            module_names_map = {
                12: "Reviewed Death", 13: "Reviewed Birth",
                14: "Move Children", 21: "Reviewed Motion",
                22: "Cluster Death (No Interaction)", 23: "Cluster Birth (No Interaction)",
                24: "Creativity (C3)", 25: "Cluster Creativity",
                26: "Creativity Simple (C3)", 27: "Creativity Fast (C3)",
                28: "Cluster Death (Shared MC)", 29: "Cluster Birth (Shared MC)"
            }
            try:
                num_active = mpi.get_active_modules_count()
                if num_active > 0:
                    mod_ids, mod_avgs = mpi.get_active_modules_performance_stats(num_active)
                    return {module_names_map.get(int(m), f"Module {int(m)}"): float(a) * 1000.0
                            for m, a in zip(mod_ids, mod_avgs)}
            except Exception:
                pass
            return {}
        return 0.0

    # --- Clusters source ---
    if source == 'clusters':
        cluster_rank_idx = int(series.get('filter_val', 1))
        if cluster_rank_idx < 1:
            cluster_rank_idx = 1
        if var_name in ['n_agents', 'n_cells', 'MC_cl', 'MC_cl_AV', 'MC_cl_shared', 'hep_sum']:
            try:
                n_clusters = mpi.get_cluster_count()[0]
                if cluster_rank_idx <= n_clusters:
                    pop_idx = int(series.get('population', 0))
                    if pop_idx == -1:
                        max_pop, max_n = 1, -1
                        for p in range(1, npops + 1):
                            iinfo, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            if iinfo[2] > max_n:
                                max_n = iinfo[2]; max_pop = p
                        pop_idx = max_pop
                    elif pop_idx == -2:
                        total_w, weighted = 0.0, 0.0
                        for p in range(1, npops + 1):
                            iinfo_p, rinfo_p = mpi.get_cluster_info(cluster_rank_idx, p)
                            w = float(iinfo_p[2])
                            if w > 0:
                                v = {'n_agents': w, 'n_cells': float(iinfo_p[1]),
                                     'MC_cl': float(rinfo_p[3]), 'MC_cl_AV': float(rinfo_p[4]),
                                     'MC_cl_shared': float(rinfo_p[5]),
                                     'hep_sum': float(rinfo_p[2])}.get(var_name, 0.0)
                                weighted += w * v; total_w += w
                        return (weighted / total_w) if total_w > 0 else 0.0
                    iinfo, rinfo = mpi.get_cluster_info(cluster_rank_idx, pop_idx)
                    return {'n_agents': float(iinfo[2]), 'n_cells': float(iinfo[1]),
                            'MC_cl': float(rinfo[3]), 'MC_cl_AV': float(rinfo[4]),
                            'MC_cl_shared': float(rinfo[5]),
                            'hep_sum': float(rinfo[2])}.get(var_name, 0.0)
            except Exception:
                pass
            return 0.0
        if var_name in ['k_fertility', 'phi_death_acc', 'phi_birth_acc', 'n_alive_acc', 'avg_creativity']:
            pop_idx = int(series.get('population', 0))
            if pop_idx == -1:
                try:
                    n_clusters = mpi.get_cluster_count()[0]
                    if cluster_rank_idx <= n_clusters:
                        max_pop, max_n = 1, -1
                        for p in range(1, npops + 1):
                            iinfo_p, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            if iinfo_p[2] > max_n:
                                max_n = iinfo_p[2]; max_pop = p
                        pop_idx = max_pop
                except Exception:
                    pop_idx = 1
            elif pop_idx == -2:
                try:
                    n_clusters = mpi.get_cluster_count()[0]
                    if cluster_rank_idx <= n_clusters:
                        total_w, weighted = 0.0, 0.0
                        for p in range(1, npops + 1):
                            iinfo_p, _ = mpi.get_cluster_info(cluster_rank_idx, p)
                            w = float(iinfo_p[2])
                            if w > 0:
                                k_f, p_d, p_b, n_a, a_c = mpi.get_dynamic_state_stats(cluster_rank_idx, p)
                                v = {'k_fertility': float(k_f), 'phi_death_acc': float(p_d),
                                     'phi_birth_acc': float(p_b), 'n_alive_acc': w,
                                     'avg_creativity': float(a_c)}.get(var_name, 0.0)
                                weighted += w * v; total_w += w
                        return (weighted / total_w) if total_w > 0 else 0.0
                except Exception:
                    pass
                return 0.0
            k_fert, p_death, p_birth, n_alive, avg_creat = mpi.get_dynamic_state_stats(cluster_rank_idx, pop_idx)
            return {'k_fertility': float(k_fert), 'phi_death_acc': float(p_death),
                    'phi_birth_acc': float(p_birth), 'n_alive_acc': float(n_alive),
                    'avg_creativity': float(avg_creat)}.get(var_name, 0.0)
        return 0.0

    # --- Agents source ---
    series_mask = np.ones(count, dtype=bool)
    fvar = series.get('filter_var')
    fval = series.get('filter_val', 0)
    if fvar and fvar != 'None':
        fdata = get_data(fvar)
        if fdata is not None:
            series_mask = series_mask & (fdata == int(fval))
    data = get_data(var_name)
    fcount = int(np.sum(series_mask))
    if data is not None and fcount > 0:
        d = data[series_mask]
        if agg == 'mean':   return float(np.mean(d))
        elif agg == 'sum':  return float(np.sum(d))
        elif agg == 'min':  return float(np.min(d))
        elif agg == 'max':  return float(np.max(d))
    return 0.0


def run_simulation_process(run_idx, start_year, end_year, save_interval, config_path, hep_paths,
                            active_modules, spawn_points, age_distribution, clustering_alg,
                            kmeans_k, dbscan_eps, dbscan_minpts, current_npops,
                            store_dead_agents, gif_path, nc_path, dead_nc_path, progress_queue,
                            ipc_interval=10, dead_export_interval=500, dead_export_threshold=1000,
                            log_path=None,
                            export_timeseries=False, ts_csv_path=None, plot_config=None,
                            temporal_interbreeding=False, interbreed_start_year=0, interbreed_end_year=0):
    import sys
    import os
    import numpy as np
    import time
    import queue
    import netCDF4

    # Redirect stdout and stderr (both Python and Fortran write through fd 1/2)
    # to a log file as early as possible, so crash output is captured.
    _log_file = None
    if log_path:
        try:
            _log_file = open(log_path, 'w', buffering=1)  # line-buffered so partial lines survive a crash
            os.dup2(_log_file.fileno(), 1)  # redirect fd 1 (stdout)
            os.dup2(_log_file.fileno(), 2)  # redirect fd 2 (stderr)
            # Also redirect Python-level streams so print() matches
            sys.stdout = _log_file
            sys.stderr = _log_file
        except Exception:
            pass  # If redirect fails, continue without it

    # Set up SIGTERM handler for clean aborts
    class TerminateInterrupt(BaseException):
        pass

    def sigterm_handler(signum, frame):
        print("Child Process: SIGTERM received. Raising TerminateInterrupt.", flush=True)
        raise TerminateInterrupt("SIGTERM")

    import signal
    try:
        signal.signal(signal.SIGTERM, sigterm_handler)
    except Exception as se:
        print(f"Could not register SIGTERM handler: {se}")

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
        
        # Enable Fortran internal performance timing so we can query detailed metrics
        if hasattr(mpi, 'set_performance_timing_enabled'):
            mpi.set_performance_timing_enabled(True)
        
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
            
        _frame_queue = None  # set up below if PIL is available
        _all_frames = []     # all PIL frames accumulated by the background thread

        if pil_available:
            import threading
            import queue as _queue_mod

            _frame_queue = _queue_mod.Queue(maxsize=400)  # cap memory: ~200 max frames × 2 buffer

            # Ensure gif output directory exists before starting thread
            _gif_out_dir = os.path.dirname(os.path.abspath(gif_path))
            if _gif_out_dir and not os.path.exists(_gif_out_dir):
                os.makedirs(_gif_out_dir, exist_ok=True)

            def _save_gif_snapshot():
                """Write all accumulated frames to disk as a complete (or partial) GIF.
                Called periodically so that an abort still produces a valid GIF file."""
                if not _all_frames:
                    return
                try:
                    _all_frames[0].save(
                        gif_path,
                        save_all=True,
                        append_images=_all_frames[1:],
                        duration=50,
                        loop=0
                    )
                except Exception as e:
                    print(f"[GIF] Snapshot write error: {e}")

            def _frame_builder_worker():
                """Accumulate frames in RAM and write a periodic snapshot GIF to disk.
                Every FLUSH_EVERY frames the current set is saved — so an abort
                will always leave a valid (partial) GIF on disk."""
                FLUSH_EVERY = 50  # flush to disk every N new frames
                while True:
                    item = _frame_queue.get()
                    if item is None:  # sentinel — loop is done
                        _frame_queue.task_done()
                        break
                    try:
                        img = Image.fromarray(item)
                        _all_frames.append(img)
                        if len(_all_frames) % FLUSH_EVERY == 0:
                            _save_gif_snapshot()
                    except Exception as e:
                        print(f"[GIF] Frame error: {e}")
                    _frame_queue.task_done()

            _frame_thread = threading.Thread(target=_frame_builder_worker, daemon=True)
            _frame_thread.start()

        # Pre-compute the static water / land HEP mask once so we don't re-allocate each frame.
        # We refresh the mask at each capture because HEP values change over simulation time.
        def capture_frame_local():
            """Extract current grid state and push raw RGB array to background thread.

            Optimizations vs the original:
            - Uses get_grid_density (O(dlon*dlat), constant in N) instead of
              get_simulation_agents (O(N), scales with agent count) to draw agent positions.
            - Replaces the per-agent Python loop with a single numpy boolean mask.
            - PIL Image.fromarray is done off the hot path in _frame_builder_worker.
            """
            hep_data = mpi.get_simulation_hep(1, dlon, dlat, npops_actual)
            grid = hep_data[:, :, 0].T  # shape: (dlat, dlon) = (h, w)
            h, w = grid.shape

            # Background / terrain layer
            rgb = np.empty((h, w, 3), dtype=np.uint8)
            water_mask = grid < -0.5
            rgb[water_mask]  = (0,   0,   255)
            rgb[~water_mask] = (253, 253, 150)
            rgb[grid > 0.5]  = (119, 221, 119)

            # Agent layer — use precomputed density grid instead of full agent transfer
            if hasattr(mpi, 'get_grid_density'):
                density = mpi.get_grid_density(dlon, dlat)  # shape: (dlon, dlat)
                # density is (dlon, dlat) — transpose to (dlat, dlon) = (h, w)
                agent_mask = density.T > 0
                rgb[agent_mask] = (255, 0, 0)

            rgb = np.flipud(rgb)

            if _frame_queue is not None:
                try:
                    _frame_queue.put_nowait(rgb)  # non-blocking; drop frame if queue is full
                except Exception:
                    pass  # queue full — skip this frame rather than blocking the simulation


        start_time = time.time()
        interval_fortran = 0.0
        interval_grid = 0.0
        interval_dead = 0.0
        last_t = 0
        cumulative_grid_mb = 0.0
        cumulative_dead_written = 0
        cumulative_fortran_time = 0.0
        capture_interval = max(1, total_ticks // 200)  # constant for the whole run

        # --- Timeseries CSV setup ---
        ts_csv_file = None
        ts_csv_writer = None
        ts_plot_defs = []  # list of (title, series, plot_type)
        if export_timeseries and ts_csv_path and plot_config:
            import csv
            try:
                os.makedirs(os.path.dirname(os.path.abspath(ts_csv_path)), exist_ok=True)
                ts_csv_file = open(ts_csv_path, 'w', newline='', buffering=1)
                ts_csv_writer = csv.writer(ts_csv_file)
                # Build column list from timeseries plots only (bucket plots are not time-scalar)
                header = ['tick', 'year']
                for pdef in plot_config.get('plots', []):
                    ptype = pdef.get('type', 'timeseries')
                    if ptype in ('timeseries', 'count', 'dualaxis'):
                        sa = pdef.get('series_a', {})
                        title = pdef.get('title', 'plot')
                        if ptype == 'dualaxis':
                            sb = pdef.get('series_b', sa)
                            header.append(f"{title}__L__{sa.get('variable', '?')}")
                            header.append(f"{title}__R__{sb.get('variable', '?')}")
                            ts_plot_defs.append((title, sa, sb, ptype))
                        else:
                            header.append(f"{title}__{sa.get('variable', '?')}")
                            ts_plot_defs.append((title, sa, None, ptype))
                ts_csv_writer.writerow(header)
                print(f"[Timeseries CSV] Writing to: {ts_csv_path} ({len(ts_plot_defs)} plots)")
            except Exception as e:
                print(f"[Timeseries CSV] Could not open file: {e}")
                ts_csv_file = None
                ts_csv_writer = None

        # --- Pre-compute temporal interbreeding tick bounds ---
        ticks_per_year = 100
        interbreed_start_tick = int((interbreed_start_year - start_year) * ticks_per_year) if temporal_interbreeding else -1
        interbreed_end_tick   = int((interbreed_end_year   - start_year) * ticks_per_year) if temporal_interbreeding else -1


        for t in range(1, total_ticks + 1):
            t0 = time.time()

            # --- Temporal interbreeding toggle ---
            if temporal_interbreeding and hasattr(mpi, 'set_allow_across_populations'):
                in_window = (interbreed_start_tick <= t <= interbreed_end_tick)
                mpi.set_allow_across_populations(in_window)

            mpi.step_simulation(t)
            dt = time.time() - t0
            interval_fortran += dt
            cumulative_fortran_time += dt
            
            if t % ipc_interval == 0 or t == total_ticks:
                count = mpi.get_agent_count()
                elapsed = time.time() - start_time
                python_used = max(0.0, elapsed - cumulative_fortran_time)
                progress = int((t / total_ticks) * 100)
                
                # Get current VmRSS RAM in MB
                ram_mb = 0.0
                try:
                    with open("/proc/self/status", "r") as f:
                        for line in f:
                            if line.startswith("VmRSS:"):
                                ram_mb = int(line.split()[1]) / 1024.0
                                break
                except Exception:
                    pass
                
                ticks_diff = t - last_t
                if ticks_diff > 0:
                    fortran_ms = (interval_fortran / ticks_diff) * 1000.0
                    grid_ms = (interval_grid / ticks_diff) * 1000.0
                    dead_ms = (interval_dead / ticks_diff) * 1000.0
                else:
                    fortran_ms = 0.0
                    grid_ms = 0.0
                    dead_ms = 0.0
                
                # Retrieve Fortran internal performance stats
                p_ms, a_ms, c_ms = 0.0, 0.0, 0.0
                g_dens_ms, g_flows_ms, g_smooth_ms, g_hep_ms = 0.0, 0.0, 0.0, 0.0
                cl_ms, fortran_total_ms = 0.0, 0.0
                active_modules_ms = {}
                if hasattr(mpi, 'get_performance_stats'):
                    try:
                        _, p_avg, a_avg, c_avg, g_dens_avg, g_flows_avg, g_smooth_avg, g_hep_avg, cl_avg, t_avg = mpi.get_performance_stats()
                        p_ms = p_avg * 1000.0
                        a_ms = a_avg * 1000.0
                        c_ms = c_avg * 1000.0
                        g_dens_ms = g_dens_avg * 1000.0
                        g_flows_ms = g_flows_avg * 1000.0
                        g_smooth_ms = g_smooth_avg * 1000.0
                        g_hep_ms = g_hep_avg * 1000.0
                        cl_ms = cl_avg * 1000.0
                        fortran_total_ms = t_avg * 1000.0
                    except Exception:
                        pass
                
                # Fetch active module performance stats
                if hasattr(mpi, 'get_active_modules_count') and hasattr(mpi, 'get_active_modules_performance_stats'):
                    try:
                        num_active = mpi.get_active_modules_count()
                        if num_active > 0:
                            mod_ids, mod_avgs = mpi.get_active_modules_performance_stats(num_active)
                            for m_id, m_avg in zip(mod_ids, mod_avgs):
                                active_modules_ms[int(m_id)] = m_avg * 1000.0
                    except Exception:
                        pass

                # Reset accumulators
                interval_fortran = 0.0
                interval_grid = 0.0
                interval_dead = 0.0
                last_t = t
                
                progress_queue.put((
                    run_idx, "progress", progress, t, count, elapsed, ram_mb,
                    fortran_ms, grid_ms, dead_ms, cumulative_grid_mb, cumulative_dead_written, python_used,
                    p_ms, a_ms, c_ms, g_dens_ms, g_flows_ms, g_smooth_ms, g_hep_ms, cl_ms, fortran_total_ms,
                    active_modules_ms
                ))

                # --- Timeseries CSV sampling ---
                if ts_csv_writer and ts_plot_defs:
                    try:
                        current_year = start_year + t / 100.0
                        # Fetch agent arrays only if any plot needs them
                        need_agents = any(
                            sa.get('source', 'agents') == 'agents' or
                            (sb is not None and sb.get('source', 'agents') == 'agents')
                            for _, sa, sb, _ in ts_plot_defs
                        )
                        _x = _y = _pop = _age = _gender = _res = _children = np.zeros(0)
                        _is_preg = _avg_res = _ux = _uy = _is_dead = _crank = _creat = np.zeros(0)
                        if need_agents and count > 0:
                            try:
                                (_x, _y, _pop, _age, _gender, _res, _children,
                                 _is_preg, _avg_res, _ux, _uy, _is_dead,
                                 _crank, _creat) = mpi.get_simulation_agents(count)
                            except Exception:
                                pass
                        row = [t, f"{current_year:.2f}"]
                        for _title, _sa, _sb, _ptype in ts_plot_defs:
                            if _ptype == 'dualaxis':
                                vL = resolve_series_value(
                                    _sa, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                vR = resolve_series_value(
                                    _sb, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                # perf_active_modules returns a dict; flatten to sum
                                if isinstance(vL, dict): vL = sum(vL.values())
                                if isinstance(vR, dict): vR = sum(vR.values())
                                row.extend([vL, vR])
                            else:
                                v = resolve_series_value(
                                    _sa, mpi, npops_actual, t, cumulative_fortran_time,
                                    _x, _y, _pop, _age, _gender, _res, _children,
                                    _is_preg, _avg_res, _ux, _uy, _is_dead, _crank, _creat, count)
                                if isinstance(v, dict): v = sum(v.values())
                                row.append(v)
                        ts_csv_writer.writerow(row)
                    except Exception as _csv_err:
                        print(f"[Timeseries CSV] Sampling error at tick {t}: {_csv_err}")
                
            # Log NC data
            t_g0 = time.time()
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
                    cumulative_grid_mb += (hep_data.nbytes + density_data.nbytes) / (1024.0 * 1024.0)
                    progress_queue.put((run_idx, "package_saved", packages_total))
            interval_grid += time.time() - t_g0
                    
            t_d0 = time.time()
            if store_dead_agents and 'ds_dead' in locals():
                if t % dead_export_interval == 0 or t == total_ticks:
                    dead_count = mpi.get_dead_agents_count()
                    if dead_count >= dead_export_threshold or t == total_ticks:
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
                            cumulative_dead_written += num_new
                            mpi.clear_dead_agents()
                            packages_total += 1
                            progress_queue.put((run_idx, "package_saved", packages_total))
            interval_dead += time.time() - t_d0

            if t % capture_interval == 0 and pil_available:
                capture_frame_local()  # pushes to background thread; non-blocking
                
        # Wait for background frame-builder to finish all pending frames
        if _frame_queue is not None:
            _frame_queue.put(None)  # sentinel
            _frame_thread.join(timeout=30)  # wait up to 30s for remaining frames
                
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
            
        # Save GIF (final snapshot with all frames)
        msg = []
        if pil_available and _all_frames:
            _save_gif_snapshot()  # ensure all frames are written
            msg.append(f"Video saved to: {os.path.abspath(gif_path)}")
        else:
            msg.append("No video generated.")
            
        if save_interval > 0 and nc_path:
            msg.append(f"Grid data saved to: {os.path.abspath(nc_path)}")
        if store_dead_agents and dead_nc_path:
            msg.append(f"Dead agents saved to: {os.path.abspath(dead_nc_path)}")

        # Close timeseries CSV
        if ts_csv_file is not None:
            try:
                ts_csv_file.flush()
                ts_csv_file.close()
                msg.append(f"Timeseries CSV saved to: {os.path.abspath(ts_csv_path)}")
                print(f"[Timeseries CSV] Closed: {ts_csv_path}")
            except Exception as _csv_close_err:
                print(f"[Timeseries CSV] Close error: {_csv_close_err}")

        progress_queue.put((run_idx, "finished", "\n".join(msg)))
        
    except TerminateInterrupt:
        print(f"Child Process {run_idx}: clean shutdown requested via SIGTERM. Finalizing data writing...")
        
        # 1. Stop background frame thread
        if '_frame_queue' in locals() and _frame_queue is not None:
            _frame_queue.put(None)  # sentinel
        if '_frame_thread' in locals() and _frame_thread is not None:
            _frame_thread.join(timeout=5)
            
        # 2. Close NetCDF files
        if 'ds' in locals():
            try: ds.sync()
            except Exception: pass
            try: ds.close()
            except Exception: pass
            
        if store_dead_agents and 'ds_dead' in locals():
            try: ds_dead.sync()
            except Exception: pass
            try: ds_dead.close()
            except Exception: pass
            try: mpi.set_forget_dead_agents(True)
            except Exception: pass

        # Close timeseries CSV on abort too
        if 'ts_csv_file' in locals() and ts_csv_file is not None:
            try:
                ts_csv_file.flush()
                ts_csv_file.close()
            except Exception:
                pass

        # 3. Save GIF (final snapshot with all frames)
        msg = []
        if 'pil_available' in locals() and pil_available and '_all_frames' in locals() and _all_frames:
            try:
                _save_gif_snapshot()
                msg.append(f"Video saved to: {os.path.abspath(gif_path)}")
            except Exception as ge:
                print(f"Error saving GIF on abort: {ge}")
                msg.append("No video generated (error during saving).")
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
    def __init__(self, num_sims, start_year, end_year, save_interval, output_path, store_dead_agents, store_grid_data,
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
        self.store_grid_data = store_grid_data
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
        self.last_update_tick = {i: 0 for i in range(1, num_sims + 1)}  # run_idx -> current tick
        self.run_log_paths = {}  # run_idx -> log file path for crash diagnostics
        self._tmp_log_dir = tempfile.mkdtemp(prefix="hep_multi_sim_")
        
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
                    progress, t, count, elapsed = args[:4]
                    self.last_update_tick[run_idx] = t
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
                
        # Check for silent crashes (processes that terminated but did not send a finished/error message)
        for run_idx, p in list(self.active_processes.items()):
            if not p.is_alive():
                exit_code = p.exitcode
                if self.run_statuses.get(run_idx) == "running":
                    err_msg = f"Process terminated abnormally with exit code {exit_code}."
                    if exit_code == -11:
                        err_msg += " (Segmentation Fault)"
                    elif exit_code == -9:
                        err_msg += " (Killed / Out of Memory)"

                    # Read last 30 lines of the subprocess console log
                    log_path = self.run_log_paths.get(run_idx)
                    if log_path and os.path.exists(log_path):
                        try:
                            with open(log_path, "r", errors="replace") as lf:
                                all_lines = lf.readlines()
                            tail = "".join(all_lines[-30:])
                            err_msg += f"\n\n--- Last 30 lines of console output ---\n{tail}"
                        except Exception as e:
                            err_msg += f"\n(Could not read log file: {e})"
                    
                    self.progress_bars[run_idx].setValue(100)
                    self.status_labels[run_idx].setText(f"Crashed: {err_msg}")
                    self.run_statuses[run_idx] = "error"
                    self.active_processes.pop(run_idx, None)
                    show_selectable_error(self, f"Simulation Run #{run_idx} Crashed", err_msg)
                
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
                    log_path = os.path.join(self._tmp_log_dir, f"run_{i}_console.log")
                    self.run_log_paths[i] = log_path
                    
                    p = multiprocessing.Process(
                        target=run_simulation_process,
                        args=(
                            i, self.start_year, self.end_year, self.save_interval,
                            self.config_path, self.hep_paths, self.active_modules,
                            self.spawn_points, self.age_dist, self.clustering_alg,
                            self.kmeans_k, self.dbscan_eps, self.dbscan_minpts, self.current_npops,
                            self.store_dead_agents, gif_path, nc_path if self.store_grid_data else None, dead_nc_path,
                            self.progress_queue, 10, 500, 1000, log_path
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

        # Check if too early to save a GIF
        too_early = False
        total_ticks = (self.end_year - self.start_year) * 100
        capture_interval = max(1, total_ticks // 200)
        
        running_indices = [i for i, p in self.active_processes.items() if p.is_alive()]
        if running_indices:
            for i in running_indices:
                current_tick = self.last_update_tick.get(i, 0)
                if current_tick < capture_interval:
                    too_early = True
                    break

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
        
        if too_early:
            QtWidgets.QMessageBox.warning(
                self, 
                "Aborted Too Early", 
                f"Simulation aborted too early to generate a GIF (requires at least {capture_interval} ticks). No video saved."
            )
        else:
            QtWidgets.QMessageBox.warning(
                self, 
                "Aborted", 
                "All active and queued parallel simulations were aborted. Partial GIFs were saved to the output folder."
            )
        
    def closeEvent(self, event):
        self.timer.stop()
        # Ensure child processes are killed
        for i, p in list(self.active_processes.items()):
            if p.is_alive():
                p.terminate()
                p.join()
        event.accept()
