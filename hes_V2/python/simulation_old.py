from utilities_visualization import calc_rows_cols
import os
import glob
import subprocess
import time
import queue
import threading
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.animation import FuncAnimation
from matplotlib.widgets import Button, TextBox
import matplotlib.colors as mcolors
import copy
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# --- Configuration ---
FORTRAN_EXECUTABLE = './bin/main'
DATA_DIRECTORY = 'data'
UPDATE_INTERVAL_MS = 500

# --- Visual Settings ---

terminal_height = 8
button_height = 1
number_of_terminal_lines = terminal_height

# ==============================================================================
# Load Hep Data (using ncdump since python netcdf libs are broken)

script_dir = os.path.dirname(os.path.abspath(__file__))

def get_netcdf_data(filepath, varname):
    """Reads a variable from a NetCDF file using ncdump."""
    try:
        # Run ncdump -v varname
        cmd = ['ncdump', '-v', varname, filepath]
        output = subprocess.check_output(cmd, text=True)
        
        # Parse output
        # Look for "data:" section
        data_idx = output.find('data:')
        if data_idx == -1:
            print(f"Error: Could not find data section in ncdump output for {varname}")
            return None
            
        # Look for "varname ="
        var_idx = output.find(f"{varname} =", data_idx)
        if var_idx == -1:
             # Try without spaces if it's a dimension variable sometimes
             var_idx = output.find(f"{varname} =", data_idx)
        
        if var_idx == -1:
             print(f"Error: Could not find variable {varname} in ncdump output")
             return None

        # Extract values
        # The values start after "=" and end with ";"
        start_idx = output.find('=', var_idx) + 1
        end_idx = output.find(';', start_idx)
        
        values_str = output[start_idx:end_idx].replace(',', ' ').split()
        values = np.array([float(v) for v in values_str])
        return values
    except Exception as e:
        print(f"Error reading {varname} from {filepath}: {e}")
        return None

def get_netcdf_dim_len(filepath, dimname):
    """Reads a dimension length from a NetCDF file using ncdump -h."""
    try:
        cmd = ['ncdump', '-h', filepath]
        output = subprocess.check_output(cmd, text=True)
        
        # Look for "dimname = len ;"
        # e.g. "lon = 100 ;"
        import re
        match = re.search(fr"\s*{dimname}\s*=\s*(\d+)\s*;", output)
        if match:
            return int(match.group(1))
        return None
    except Exception as e:
        print(f"Error reading dim {dimname} from {filepath}: {e}")
        return None

    except Exception as e:
        print(f"Error reading dim {dimname} from {filepath}: {e}")
        return None

# Parse HEP file path from mod_basic_config.f95
config_path = os.path.join(script_dir, '..', 'src', 'globals', 'mod_basic_config.f95')
hep_file = None

try:
    with open(config_path, 'r') as f:
        content = f.read()
        # Look for the first string in hep_paths array
        # Pattern: hep_paths(npops) = [character(len=256) :: & "path", ...
        # Or just look for the first string after "hep_paths"
        import re
        # Find hep_paths definition
        # Supports:
        # hep_paths(npops) = [character(len=256) :: "path", ...
        # hep_paths(npops) = ["path", ...
        # hep_paths(npops) = (/ "path", ... /)
        match = re.search(r'hep_paths.*?(?:=|=>)\s*(?:\[|\(/).*?["\']([^"\']+)["\']', content, re.DOTALL)
        if match:
            rel_path = match.group(1)
            hep_file = os.path.join(script_dir, '..', rel_path)
            print(f"Found HEP file in config: {hep_file}")
        else:
             print("Warning: Could not parse hep_paths from config. Using default.")
             
        # Check if binary is outdated
        bin_path = os.path.join(script_dir, '..', FORTRAN_EXECUTABLE)
        if os.path.exists(bin_path):
            config_mtime = os.path.getmtime(config_path)
            bin_mtime = os.path.getmtime(bin_path)
            if config_mtime > bin_mtime:
                print("\n" + "="*60)
                print("⚠️  WARNING: Configuration file is newer than the executable!")
                print("   The simulation might not reflect recent config changes.")
                print(f"   Config modified: {time.ctime(config_mtime)}")
                print(f"   Binary modified: {time.ctime(bin_mtime)}")
                print("   Please run 'make clean && make bin/main' to update.")
                print("="*60 + "\n")
        
except Exception as e:
    print(f"Error reading config file: {e}")

if not hep_file:
    hep_file = os.path.join(script_dir, '..', 'input', 'hep', 'gradient', 'gradient_hep_1.nc')

if os.path.exists(hep_file):
    print(f"Loading HEP data from {hep_file}...")
    
    # Read dimensions
    n_lat = get_netcdf_dim_len(hep_file, 'lat')
    n_lon = get_netcdf_dim_len(hep_file, 'lon')
    n_time = get_netcdf_dim_len(hep_file, 'time')
    
    print(f"Dimensions: lat={n_lat}, lon={n_lon}, time={n_time}")
    
    # Read coordinates
    lats = get_netcdf_data(hep_file, 'lat')
    lons = get_netcdf_data(hep_file, 'lon')
    
    # Calculate extent
    if lats is not None and lons is not None:
        delta_lat = lats[1] - lats[0] if len(lats) > 1 else 1.0
        delta_lon = lons[1] - lons[0] if len(lons) > 1 else 1.0
        
        lat_0 = lats[0]
        lon_0 = lons[0]
        
        # Extent: [lon_min, lon_max, lat_min, lat_max]
        extent = [
            float(lon_0 - 0.5 * delta_lon),
            float(lons[-1] + 0.5 * delta_lon),
            float(lat_0 - 0.5 * delta_lat),
            float(lats[-1] + 0.5 * delta_lat)
        ]
    else:
        extent = [0.0, 100.0, 0.0, 100.0] # Fallback
        
    # Read AccHEP data
    # Shape in file: (time, lat, lon)
    # We want to match existing usage: hep_data[:,:,0,current_frame_index // t_hep_delta]
    # Existing usage seems to imply: (lat, lon, pop, time) or similar?
    # Line 279: grid_slice = hep_data[:,:,0,current_frame_index // t_hep_delta]
    # So it expects (dim1, dim2, pop, time)
    # Let's reshape AccHEP to (lat, lon, 1, time)
    
    acc_hep_flat = get_netcdf_data(hep_file, 'AccHEP')
    watermask_flat = get_netcdf_data(hep_file, 'watermask')
    
    if acc_hep_flat is not None:
        # ncdump output is C-order: time, lat, lon
        acc_hep = acc_hep_flat.reshape((n_time, n_lat, n_lon))
        
        # Transpose to (lat, lon, time)
        acc_hep = np.transpose(acc_hep, (1, 2, 0))
        
        # Apply watermask if available
        if watermask_flat is not None:
             # watermask shape: (lat, lon)
             watermask = watermask_flat.reshape((n_lat, n_lon))
             # Transpose to match acc_hep (lat, lon) - actually it's already (lat, lon) if read C-order?
             # Wait, ncdump flattens everything.
             # watermask in file is (lat, lon).
             # So reshape((n_lat, n_lon)) is correct.
             # But we need to ensure orientation matches.
             # acc_hep was (time, lat, lon) -> transposed to (lat, lon, time).
             # So watermask (lat, lon) should match the first two dims.
             
             # Expand watermask to match time dimension for broadcasting
             # watermask_expanded = watermask[:, :, np.newaxis]
             
             # Set water pixels to -1
             # We need to iterate or broadcast.
             # Let's do it efficiently.
             # Where watermask == 0 (assuming 0 is water), set HEP to -1.
             
             # Check if watermask needs transposing?
             # ncdump output order is row-major (C-style) for the flattened array.
             # If variable is (lat, lon), then it fills lon first, then lat.
             # So reshape((n_lat, n_lon)) gives index [lat, lon].
             # This matches acc_hep's [lat, lon, time] after transpose.
             
             for t in range(n_time):
                 acc_hep[:, :, t] = np.where(watermask == 0, -1.0, acc_hep[:, :, t])
                 
        else:
             print("Warning: watermask not found. Using raw HEP data.")

        
        # Add population dimension: (lat, lon, 1, time)
        hep_data = acc_hep[:, :, np.newaxis, :]
        
        # t_hep_delta needs to be set correctly. 
        # In the file, time is 10 steps. 
        # Simulation might have many more steps.
        # We need to map simulation time to HEP time.
        # Let's assume t_hep_delta is derived from config or just set it to something reasonable.
        # Config says delta_t_hep = 2000.
        t_hep_delta = 2000 
        
    else:
        print("Error: Could not read AccHEP data.")
        hep_data = np.zeros((100, 100, 1, 1))

else:
    print(f"Error: HEP file not found at {hep_file}")
    extent = [0.0, 100.0, 0.0, 100.0]
    hep_data = np.zeros((100, 100, 1, 1))


# ==============================================================================
# == FORTAN SIMULATION PARAMETERS ==
FORTRAN_PARAMS = {
    'output_interval': 100,
}
# ==============================================================================

# --- Layout Configuration ---
NUM_ROWS_BESIDE = 2
SMALL_PLOT_COLS_BESIDE = 2
SMALL_PLOT_COLS_BELOW = 4

# ==============================================================================
# ==  MODULAR PLOT CONFIGURATION ==
PLOT_CONFIG = [
    {
        'name': 'age_gender_distribution',
        'type': 'bar_graph',
        'title': 'Age Distribution',
        'bucket_variable': 'age',
        'buckets': [0, 10, 20, 30, 40, 50, 60, 70, 80, 100],
        'grouping_variable': 'gender',
        'grouping_values': ['M', 'F'],
        'pyramid_style': True
    },
    {
        'name': 'population',
        'type': 'curve',
        'title': 'Population Size Over Time',
        'column': 'id',
        'aggregation': 'count',
        'y_label': 'Total Agents'
    },
    {
        'name': 'avg_age',
        'type': 'curve',
        'title': 'Average Age Over Time',
        'column': 'age',
        'aggregation': 'mean',
        'y_label': 'Average Age (Years)'
    },
    {
        'name': 'pregnant_agents',
        'type': 'curve',
        'title': 'Number of Pregnant Agents',
        'aggregation': 'count_if',
        'condition': 'is_pregnant > 0',
        'y_label': 'Pregnant Agents Count'
    },
    {'name': 'plot4', 'type': 'curve', 'title': 'Plot 4', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot5', 'type': 'curve', 'title': 'Plot 5', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot6', 'type': 'curve', 'title': 'Plot 6', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot7', 'type': 'curve', 'title': 'Plot 7', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
]
# ==============================================================================

# --- Global State Variables ---
fortran_process, ani = None, None
plot_objects = {}
current_frame_index = 0
current_sim_params = FORTRAN_PARAMS.copy()
terminal_queue = queue.Queue()
terminal_lines = []
terminal_text_object = None

# --- UI Widget Globals ---
start_button, stop_button = None, None
param_boxes = {}
interval_box = None

# --- Dynamic Plot Setup ---
num_small_plots = len(PLOT_CONFIG)
height_map, width_map = 10, 10
height_small_plots, width_small_plots = 5, 5
num_rows_below, num_cols_beside = calc_rows_cols(num_small_plots)

# Add extra rows for UI elements to the grid calculation
num_ui_rows = 2 
total_rows = num_rows_below + 2 + num_ui_rows
total_cols = num_cols_beside + 2

height_fig = height_map + height_small_plots * num_rows_below + terminal_height + button_height # Add space for UI
width_fig = width_map + width_small_plots * num_cols_beside

fig = plt.figure(figsize=(width_fig, height_fig))
# Define height ratios to make UI rows smaller than plot rows
height_ratios = [height_small_plots] * (total_rows - num_ui_rows) + [terminal_height, button_height] 
gs = gridspec.GridSpec(total_rows, total_cols, hspace=0.8, height_ratios=height_ratios)


ax_map = fig.add_subplot(gs[0:2, 0:2], projection=ccrs.PlateCarree())
# ax_map.coastlines(zorder=3); #ax_map.add_feature(cfeature.BORDERS, linestyle=':')
# ax_map.add_feature(cfeature.LAND, facecolor='lightgray', zorder=0)
# ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue', zorder=0)
ax_map.set_extent(extent, crs=ccrs.PlateCarree())
map_title = ax_map.set_title('Agent positions (Waiting to start)')
scatter = ax_map.scatter([], [], s=5, transform=ccrs.PlateCarree(), zorder=10)

# Create custom colormap for HEP data
# 0-1 gradient (e.g., Greens or similar), -1 is blue
base_cmap = copy.copy(plt.get_cmap('Greens'))
base_cmap.set_under('blue')
hep_cmap = base_cmap

# color bar for hep heatmap

colorbar_added = False

for i, config in enumerate(PLOT_CONFIG):
    if i < num_cols_beside: row, col = 0, i + 2
    elif i < num_cols_beside * 2: row, col = 1, i + 2 - num_cols_beside
    else:
        i_prime = i - (num_cols_beside * 2)
        row, col = 2 + i_prime // total_cols, i_prime % total_cols
    ax = fig.add_subplot(gs[row, col])
    ax.set_title(config['title'])
    plot_type = config.get('type', 'curve')
    if plot_type == 'bar_graph':
        buckets = config['buckets']
        bin_labels = [f"{buckets[j]}-{buckets[j+1]-1}" for j in range(len(buckets)-2)] + [f"{buckets[-2]}+"]
        y_pos = np.arange(len(bin_labels))
        bar_groups = {}
        colors = ['steelblue', 'green', 'orange', 'purple', 'red']
        for idx, group_val in enumerate(config['grouping_values']):
            bars = ax.barh(y_pos, np.zeros(len(y_pos)), align='center', color=colors[idx % len(colors)], edgecolor="black", label=group_val)
            bar_groups[group_val] = bars
        ax.set_yticks(y_pos, labels=bin_labels); ax.invert_yaxis()
        ax.set_xlabel("Percentage (%)"); ax.legend(loc='lower right'); ax.grid(axis='x', linestyle='--', alpha=0.7)
        plot_objects[config['name']] = {'ax': ax, 'type': 'bar_graph', 'config': config, 'bar_groups': bar_groups}
    elif plot_type == 'curve':
        line, = ax.plot([], [], color="blue")
        ax.set_xlabel("Time"); ax.set_ylabel(config['y_label'])
        ax.grid(True); ax.set_ylim(0, 50); ax.set_xlim(0, 50)
        plot_objects[config['name']] = {'ax': ax, 'type': 'curve', 'line': line, 'config': config, 'x_data': [], 'y_data': []}

def enqueue_output(out, queue):
    for line in iter(out.readline, ''):
        queue.put(line)
    out.close()

def start_simulation(params_to_pass):
    global fortran_process, current_frame_index
    print("🚀 Starting Fortran simulation...")
    current_frame_index = 0
    for name, plot_data in plot_objects.items():
        if plot_data.get('type') == 'curve':
            plot_data['x_data'].clear(); plot_data['y_data'].clear()
            plot_data['ax'].set_xlabel(f"Time (x{params_to_pass.get('output_interval', 'N/A')})")
    os.makedirs(DATA_DIRECTORY, exist_ok=True)
    for f in glob.glob(os.path.join(DATA_DIRECTORY, '*.csv')): os.remove(f)
    try:
        cmd = [FORTRAN_EXECUTABLE]
        for key, value in params_to_pass.items():
            cmd.append(f'--{key}'); cmd.append(str(value))
        print(f"Running command: {' '.join(cmd)}")
        fortran_process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)
        output_thread = threading.Thread(target=enqueue_output, args=(fortran_process.stdout, terminal_queue))
        output_thread.daemon = True
        output_thread.start()
        print(f"Fortran process started with PID: {fortran_process.pid}")
    except FileNotFoundError:
        print(f"❌ ERROR: Executable not found at '{FORTRAN_EXECUTABLE}'"); fortran_process = None

def stop_simulation(event):
    global fortran_process, ani
    if fortran_process and fortran_process.poll() is None:
        print("🛑 Stopping Fortran simulation...")
        fortran_process.terminate(); fortran_process = None
        map_title.set_text('Simulation Stopped by User')
    if ani:
        ani.event_source.stop(); ani = None
    if start_button: start_button.set_active(True)
    for key, box in param_boxes.items(): box.set_active(True)
    if interval_box: interval_box.set_active(True)
    fig.canvas.draw_idle()

def update(frame):
    global current_frame_index, terminal_lines
    while not terminal_queue.empty():
        try:
            line = terminal_queue.get_nowait()
            terminal_lines.append(line.strip())
            terminal_lines = terminal_lines[-number_of_terminal_lines:]
        except queue.Empty:
            break
    if terminal_text_object:
        terminal_text_object.set_text('\n'.join(terminal_lines))

    current_frame_index += 1
    timestep_increment = current_sim_params.get('output_interval', 1)
    timestep_to_find = current_frame_index * timestep_increment
    data_file = os.path.join(DATA_DIRECTORY, f'agents_plotting_data_{timestep_to_find}.csv')
    wait_time, max_wait_sec = 0, 60
    while not os.path.exists(data_file):
        #time.sleep(0.1); wait_time += 0.1
        plt.pause(0.01); wait_time += 0.01 # keeps gui responsive (no annoying messages about "not responding")
        if wait_time > max_wait_sec or (fortran_process and fortran_process.poll() is not None):
            print("Simulation finished or timed out."); stop_simulation(None); return
    try:
        required_cols = set()
        for p in PLOT_CONFIG:
            ptype = p.get('type', 'curve')
            if ptype == 'curve':
                if p['aggregation'] in ['mean', 'std']: required_cols.add(p['column'])
                elif p['aggregation'] == 'count_if': required_cols.add(p['condition'].split()[0])
            elif ptype == 'bar_graph':
                required_cols.add(p['bucket_variable']); required_cols.add(p['grouping_variable'])
        base_cols = ['id', 'pos_x', 'pos_y', 'gender', 'age', 'population']
        all_csv_cols = base_cols + sorted(list(required_cols - set(base_cols)))
        df = pd.read_csv(data_file, sep='\s+', skiprows=1, header=None, names=all_csv_cols)
        if df.empty: stop_simulation(None); return
        total_agents = len(df)
        df['age'] = (df['age'] / 52.0).clip(upper=100)
        map_title.set_text(f'Agent positions (t = {timestep_to_find})')
        colors = df['population'].map({1: 'blue', 2: 'green', 3: 'orange'}).fillna('black')
        scatter.set_offsets(df[['pos_x', 'pos_y']].values); scatter.set_color(colors)
        # handle the heatmap overlay

        # handle the heatmap overlay
        # Ensure index is within bounds
        hep_time_idx = min(current_frame_index // t_hep_delta, hep_data.shape[3] - 1)
        grid_slice = hep_data[:,:,0,hep_time_idx]
        
        # Debug print (optional, remove later)
        # print(f"Frame {current_frame_index}, HEP time {hep_time_idx}, Slice min/max: {grid_slice.min()}/{grid_slice.max()}")

        # Update image data instead of creating new imshow every time if possible, 
        # but imshow returns an AxesImage which we can update.
        # For now, let's clear and redraw or just use set_data if we had an object.
        # But we need to handle the extent and transform.
        # Simpler to clear? No, that clears coastlines.
        # Let's try to update the existing image if it exists, or create it.
        
        if not hasattr(update, "hep_img"):
            # Initialize with the first frame or zeros if not ready
            # Use zorder=0 to be the background
            update.hep_img = ax_map.imshow(grid_slice, origin='lower', extent=extent,
                    transform=ccrs.PlateCarree(), cmap=hep_cmap,
                    vmin=0, vmax=1, zorder=0)
        else:
            update.hep_img.set_data(grid_slice)
            # Add ocean once here if needed, but better in setup.
            
        # Ensure scatter is on top
        scatter.set_zorder(10)

        #if colorbar_added == False :
        #    plt.colorbar(img, ax_map=ax_map, orientation='vertical', shrink=0.6, label='Hep Value')
        #    colorbar_added = True



        # handle other plots 
        for name, plot_data in plot_objects.items():
            ax, plot_type, config = plot_data['ax'], plot_data['type'], plot_data.get('config')
            if plot_type == 'bar_graph':
                ax.set_title(f"{config['title']} (N={total_agents})")
                max_pc = 0
                for i, group_val in enumerate(config['grouping_values']):
                    group_df = df[df[config['grouping_variable']] == group_val]
                    counts, _ = np.histogram(group_df[config['bucket_variable']], bins=config['buckets'])
                    percent = (counts / total_agents * 100) if total_agents > 0 else 0
                    max_pc = max(max_pc, np.max(percent) if len(percent) > 0 else 0)
                    sign = -1 if config.get('pyramid_style', False) and i % 2 != 0 else 1
                    bars = plot_data['bar_groups'][group_val]
                    for bar, width in zip(bars, percent): bar.set_width(width * sign)
                xlim = max(10, max_pc + 5)
                ax.set_xlim(-xlim if config.get('pyramid_style', False) else 0, xlim)
            elif plot_type == 'curve':
                value = 0
                if config['aggregation'] == 'count': value = total_agents
                elif config['aggregation'] == 'mean': value = df[config['column']].mean() if not df.empty else 0
                elif config['aggregation'] == 'std': value = df[config['column']].std() if not df.empty else 0
                elif config['aggregation'] == 'count_if': value = len(df.query(config['condition']))
                plot_data['x_data'].append(current_frame_index); plot_data['y_data'].append(value)
                plot_data['line'].set_data(plot_data['x_data'], plot_data['y_data'])
                if value >= ax.get_ylim()[1]: ax.set_ylim(bottom=0, top=max(1, ax.get_ylim()[1] * 2))
                if current_frame_index >= ax.get_xlim()[1]: ax.set_xlim(left=0, right=ax.get_xlim()[1] * 2)
    except Exception as e:
        print(f"Error processing file {data_file}: {e}")
    fig.canvas.draw_idle()

if __name__ == "__main__":
    def start_app(event):
        global UPDATE_INTERVAL_MS, ani, current_sim_params
        params_to_pass = {}
        try:
            for key, box in param_boxes.items():
                val = float(box.text)
                if val.is_integer(): val = int(val)
                params_to_pass[key] = val
            UPDATE_INTERVAL_MS = int(interval_box.text)
            if UPDATE_INTERVAL_MS <= 0:
                print("ERROR: Update rate must be a positive integer."); return
        except ValueError:
            print("ERROR: Invalid input. Please enter valid numbers."); return
        current_sim_params = params_to_pass.copy()
        start_button.set_active(False)
        for key, box in param_boxes.items(): box.set_active(False)
        interval_box.set_active(False)
        fig.canvas.draw_idle()
        start_simulation(params_to_pass)
        if fortran_process:
            ani = FuncAnimation(fig, update, frames=None, interval=UPDATE_INTERVAL_MS, repeat=False, cache_frame_data=False)
            fig.canvas.draw_idle()
        else:
            print("Could not start simulation. Re-enabling controls.")
            stop_simulation(None)

    # --- UI Setup using GridSpec ---
    # Terminal Panel (second to last row, spanning all columns)
    ax_terminal = fig.add_subplot(gs[-2, :])
    ax_terminal.set_xticks([]); ax_terminal.set_yticks([]); ax_terminal.set_facecolor('black')
    terminal_text_object = ax_terminal.text(0.01, 0.95, '--- Simulation output will appear here ---',
                                            ha='left', va='top', color='lightgray',
                                            fontfamily='monospace', fontsize=9)
    
    # Control Panel (last row)
    # Correctly calculate the number of columns needed for the controls
    num_control_items = (len(FORTRAN_PARAMS) * 2) + 2 + 2 # (param labels+boxes) + (interval label+box) + (start+stop buttons)
    gs_controls = gs[-1, :].subgridspec(1, num_control_items, wspace=0.5)

    control_col_index = 0
    for key, value in FORTRAN_PARAMS.items():
        ax_label = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
        ax_label.text(0.95, 0.5, f'{key}:', ha='right', va='center'); ax_label.axis('off')
        
        ax_box = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
        param_boxes[key] = TextBox(ax_box, '', initial=str(value))

    ax_interval_label = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
    ax_interval_label.text(0.95, 0.5, 'Update (ms):', ha='right', va='center'); ax_interval_label.axis('off')
    
    ax_interval_box = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
    interval_box = TextBox(ax_interval_box, '', initial=str(UPDATE_INTERVAL_MS))
    
    ax_start_button = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
    start_button = Button(ax_start_button, 'Start')

    ax_stop_button = fig.add_subplot(gs_controls[0, control_col_index]); control_col_index += 1
    stop_button = Button(ax_stop_button, 'Stop')

    start_button.on_clicked(start_app)
    stop_button.on_clicked(stop_simulation)
    
    plt.tight_layout()
    plt.show()

