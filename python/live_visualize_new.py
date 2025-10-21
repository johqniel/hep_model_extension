from utilities_visualization import calc_rows_cols
import os
import glob
import subprocess
import time
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.animation import FuncAnimation
from matplotlib.widgets import Button
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# --- Configuration ---
FORTRAN_EXECUTABLE = './bin/main_agb'
DATA_DIRECTORY = 'data'
TIMESTEP_INCREMENT = 1000
UPDATE_INTERVAL_MS = TIMESTEP_INCREMENT / 2

# --- Layout Configuration ---
NUM_ROWS_BESIDE = 2
SMALL_PLOT_COLS_BESIDE = 2
SMALL_PLOT_COLS_BELOW = 4

# ==============================================================================
# ==  MODULAR PLOT CONFIGURATION ==
# To add a new curve plot, just add a new dictionary to this list.
#
# 'name': A unique internal identifier.
# 'type': curve or bar_graph
# 'title': The title that will appear on the plot.
#
# if type == curve: 
#   'column': The column name from your Fortran CSV output.
#   'aggregation': How to calculate the value for the y-axis.
#                'count' -> total number of agents.
#                'mean'  -> the average of the specified column.
#                'std'   -> the standard deviation of the column.
#               'count_if' -> count agents where a condition is met.
#   'y_label': The label for the y-axis.
#
# if type == bar_graph:
#        'bucket_variable':   The var used to create the histogram buckets
#        'buckets':           The histogram buckets
#        'grouping_variable': The var used to group bars in two colors (e.g. gender)
#        'grouping_values':   In case of gender, ['M', 'F'] to define the groups and their order
#        'pyramid_style':     Make the second group's bars go left
PLOT_CONFIG = [
    {
        'name': 'age_gender_distribution',
        'type': 'bar_graph',
        'title': 'Age Distribution',
        'bucket_variable': 'age',
        'buckets': [0, 10, 20, 30, 40, 50, 60, 70, 80, 100],
        'grouping_variable': 'gender',
        'grouping_values': ['M', 'F'], # Defines the groups and their order
        'pyramid_style': True # Make the second group's bars go left
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
    {'name': 'plot4', 'title': 'Plot 4', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot5', 'title': 'Plot 5', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot6', 'title': 'Plot 6', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot7', 'title': 'Plot 7', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
]
# ==============================================================================

# --- Global State Variables ---
fortran_process, ani = None, None
plot_objects = {} # Unified dictionary to hold all plot objects
current_frame_index = 0

# --- Dynamic Plot Setup ---
num_small_plots = len(PLOT_CONFIG)

height_map = 10
width_map = 10
height_small_plots = 5
width_small_plots = 5

num_rows_below, num_cols_beside = calc_rows_cols(num_small_plots)

height_fig = height_map + height_small_plots * num_rows_below
width_fig = width_map + width_small_plots * num_cols_beside

#num_plots_beside = min(num_small_plots, NUM_ROWS_BESIDE * SMALL_PLOT_COLS_BESIDE)
#plots_config_beside = PLOT_CONFIG[:num_plots_beside]
#num_plots_below = num_small_plots - num_plots_beside
#plots_config_below = PLOT_CONFIG[num_plots_beside:]
#has_bottom_row = num_plots_below > 0
#num_rows_below = (num_plots_below + SMALL_PLOT_COLS_BELOW - 1) // SMALL_PLOT_COLS_BELOW if has_bottom_row else 0
#fig_height = (NUM_ROWS_BESIDE * 3) + (num_rows_below * 3.5)

fig = plt.figure(figsize=(width_fig, height_fig))

gs = gridspec.GridSpec(num_rows_below + 2, num_cols_beside + 2, hspace = 0.5)
print("GridSpec shape: ", gs.get_geometry())
#if not has_bottom_row:
#    gs_main = gridspec.GridSpec(1, 2, width_ratios=[3, 2], wspace=0.3)
#    gs_top = gs_main; gs_bottom = None
#else:
#    gs_main = gridspec.GridSpec(2, 1, height_ratios=[NUM_ROWS_BESIDE, num_rows_below], hspace=0.5)
#    gs_top = gs_main[0].subgridspec(1, 2, width_ratios=[3, 2], wspace=0.2)
#    gs_bottom = gs_main[1].subgridspec(num_rows_below, SMALL_PLOT_COLS_BELOW, hspace=0.7, wspace=0.3)

# 1. Map Plot
ax_map = fig.add_subplot(gs[0:2, 0:2], projection=ccrs.PlateCarree())
ax_map.coastlines(); ax_map.add_feature(cfeature.BORDERS, linestyle=':')
ax_map.add_feature(cfeature.LAND, facecolor='lightgray'); ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')
ax_map.set_extent([-10, 30, 35, 70])
map_title = ax_map.set_title('Agent positions (Frame 0)')
scatter = ax_map.scatter([], [], s=5, transform=ccrs.PlateCarree())

# 2. Grid of small plots
#gs_right = gs_top[0, 1].subgridspec(NUM_ROWS_BESIDE, SMALL_PLOT_COLS_BESIDE, hspace=0.7, wspace=0.3)
#all_plot_grids = [(gs_right, plots_config_beside, SMALL_PLOT_COLS_BESIDE)]
#if gs_bottom: all_plot_grids.append((gs_bottom, plots_config_below, SMALL_PLOT_COLS_BELOW))

#for grid_spec, configs, num_cols in all_plot_grids:
#print("num plots: ", num_small_plots)
#print("num cols: ", num_cols_beside + 2, " num rows: ", num_rows_below + 2)

for i, config in enumerate(PLOT_CONFIG):
    #print("placing plot: ", i, " - ", config['name'])
    #print("num_cols_beside: ", num_cols_beside)
    if i < num_cols_beside:
        row, col = 0, i + 2
    elif i < num_cols_beside * 2:
        row, col = 1, i + 2 - num_cols_beside
    else: 
        i_prime = i - (num_cols_beside * 2)
        row, col = 2 + i_prime // (num_cols_beside + 2), i_prime % (num_cols_beside + 2) 

    ax = fig.add_subplot(gs[row, col])
    ax.set_title(config['title'])
    plot_type = config.get('type', 'curve')
        
    if plot_type == 'bar_graph':
        buckets = config['buckets']
        bin_labels = [f"{buckets[i]}-{buckets[i+1]-1}" for i in range(len(buckets)-2)] + [f"{buckets[-2]}+"]
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
        ax.set_xlabel(f"Time (x{TIMESTEP_INCREMENT})"); ax.set_ylabel(config['y_label'])
        ax.grid(True); ax.set_ylim(0, 10); ax.set_xlim(0, 50)
        plot_objects[config['name']] = {'ax': ax, 'type': 'curve', 'line': line, 'config': config, 'x_data': [], 'y_data': []}

    print(f"Placed plot '{config['name']}' at grid position ({row}, {col})")


# --- Functions (start_simulation, stop_simulation are unchanged) ---
def start_simulation():
    global fortran_process
    print("🚀 Starting Fortran simulation...")
    os.makedirs(DATA_DIRECTORY, exist_ok=True)
    for f in glob.glob(os.path.join(DATA_DIRECTORY, '*.csv')): os.remove(f)
    try:
        fortran_process = subprocess.Popen([FORTRAN_EXECUTABLE, str(TIMESTEP_INCREMENT)])
        print(f"Fortran process started with PID: {fortran_process.pid}")
    except FileNotFoundError:
        print(f"❌ ERROR: Executable not found at '{FORTRAN_EXECUTABLE}'"); fortran_process = None

def stop_simulation(event):
    global fortran_process, ani
    if fortran_process and fortran_process.poll() is None:
        print("🛑 Stopping Fortran simulation...")
        fortran_process.terminate(); fortran_process = None
        map_title.set_text('Simulation Stopped by User'); fig.canvas.draw_idle()
    if ani: ani.event_source.stop()

def update(frame):
    global current_frame_index
    current_frame_index += 1
    timestep_to_find = current_frame_index * TIMESTEP_INCREMENT
    data_file = os.path.join(DATA_DIRECTORY, f'agents_plotting_data_{timestep_to_find}.csv')

    wait_time, max_wait_sec = 0, 60
    while not os.path.exists(data_file):
        time.sleep(0.1); wait_time += 0.1
        if wait_time > max_wait_sec or (fortran_process and fortran_process.poll() is not None):
            stop_simulation(None); return

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

# --- Main Execution ---
if __name__ == "__main__":
    ax_stop_button = fig.add_axes([0.92, 0.01, 0.06, 0.04])
    stop_button = Button(ax_stop_button, 'Stop')
    stop_button.on_clicked(stop_simulation)
    ani = FuncAnimation(fig, update, frames=None, interval=UPDATE_INTERVAL_MS, repeat=False, cache_frame_data=False)
    start_simulation()
    if fortran_process:
        fig.tight_layout(rect=[0, 0.03, 0.9, 1])
        plt.show()
    else:
        print("Could not start the simulation. Exiting.")

