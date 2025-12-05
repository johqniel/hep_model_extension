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
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# --- Configuration ---
FORTRAN_EXECUTABLE = './bin/main_new'
DATA_DIRECTORY = 'data'
UPDATE_INTERVAL_MS = 500

# --- Visual Settings ---

terminal_height = 8
button_height = 1
number_of_terminal_lines = terminal_height

# --- visualize hep settings ---
script_dir = os.path.dirname(os.path.abspath(__file__))
hep_data_dir = os.path.join(script_dir, '..', 'hep_control')
dims_path = os.path.join(hep_data_dir, 'hep_dims.txt')
bin_path = os.path.join(hep_data_dir, 'hep.bin')
t_hep_delta = 2000 # time steps between Hep data outputs should also be read in from hep_dims.txt

# ==============================================================================
# Load Hep Data:

# === Load dimensions from Fortran text file ===
with open(dims_path) as f:
    parts = f.read().split()
# First 4 are integers, rest are floats
n, m, num_pop, t_hep = map(int, parts[:4])
lon_hep_one, lon_hep_deux, lat_hep_one, lat_hep_deux = map(float, parts[4:])

# === calculate grid spacing and extends ===
delta_lon = lon_hep_deux - lon_hep_one
delta_lat = lat_hep_deux - lat_hep_one
lon_0 = lon_hep_one - 0.5 * delta_lon
lat_0 = lat_hep_one - 0.5 * delta_lat

extent = [
    lon_0,
    lon_0 + m * delta_lon,
    lat_0,
    lat_0 + n * delta_lat,
]

# === Read binary data ===
hep_data = np.fromfile(bin_path, dtype=np.float64)
hep_data = hep_data.reshape((n, m, num_pop, t_hep), order='F')


# flip data and rotate to match map orientation
hep_data = np.rot90(hep_data, k=3, axes=(0,1))
hep_data = np.flip(hep_data, axis=1)


# ==============================================================================
# == FORTAN SIMULATION PARAMETERS ==
FORTRAN_PARAMS = {
    'output_interval': 1000,
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
ax_map.coastlines(); #ax_map.add_feature(cfeature.BORDERS, linestyle=':')
ax_map.add_feature(cfeature.LAND, facecolor='lightgray'); ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')
ax_map.set_extent(extent)
map_title = ax_map.set_title('Agent positions (Waiting to start)')
scatter = ax_map.scatter([], [], s=5, transform=ccrs.PlateCarree())

#add color bar for hep heatmap

#img = ax_map.imshow(hep_data[:,:,0,0], origin='lower', extent=extent,
#    transform=ccrs.PlateCarree(), cmap='Greens',
#    vmin=0, vmax=1, alpha=0.8)
#plt.colorbar(img, ax_map=ax_map, orientation='vertical', shrink=0.6, label='Hep Value')


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

        grid_slice = hep_data[:,:,0,current_frame_index // t_hep_delta]

        img = ax_map.imshow(grid_slice, origin='lower', extent=extent,
                transform=ccrs.PlateCarree(), cmap='Greens',
                vmin=0, vmax=1, alpha=0.8)
        # i want ocean color to be on top of heatmap :()
        ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')

        if current_frame_index == 1 :
            plt.colorbar(img, ax_map=ax_map, orientation='vertical', shrink=0.6, label='Hep Value')



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

