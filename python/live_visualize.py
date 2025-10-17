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
TIMESTEP_INCREMENT = 100
UPDATE_INTERVAL_MS = TIMESTEP_INCREMENT / 2

# --- NEW: Layout Configuration ---
# You can now control the grid layout with these variables.
NUM_ROWS_BESIDE = 2      # How many rows of small plots to show next to the map.
SMALL_PLOT_COLS_BESIDE = 2 # How many columns for the grid next to the map.
SMALL_PLOT_COLS_BELOW = 4  # How many columns for the new grid that appears below.

# ==============================================================================
# ==  MODULAR PLOT CONFIGURATION ==
# To add a new curve plot, just add a new dictionary to this list.
#
# 'name': A unique internal identifier.
# 'title': The title that will appear on the plot.
# 'column': The column name from your Fortran CSV output.
# 'aggregation': How to calculate the value for the y-axis.
#                'count' -> total number of agents.
#                'mean'  -> the average of the specified column.
#                'std'   -> the standard deviation of the column.
#               'count_if' -> count agents where a condition is met.
# 'y_label': The label for the y-axis.
#
PLOT_CONFIG = [
    {
        'name': 'population',
        'title': 'Population Size Over Time',
        'column': 'id',
        'aggregation': 'count',
        'y_label': 'Total Agents'
    },
    {
        'name': 'avg_age',
        'title': 'Average Age Over Time',
        'column': 'age',
        'aggregation': 'mean',
        'y_label': 'Average Age (Years)'
    },
    {
        'name': 'pregnant_agents',
        'title': 'Number of Pregnant Agents',
        'aggregation': 'count_if',
        'condition': 'is_pregnant > 0',
        'y_label': 'Pregnant Agents Count'
    },
    # Add 4 more plots to see the bottom row appear!
    {'name': 'plot4', 'title': 'Plot 4', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot5', 'title': 'Plot 5', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot6', 'title': 'Plot 6', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
    {'name': 'plot7', 'title': 'Plot 7', 'aggregation': 'count', 'column': 'id', 'y_label': '...'},
]
# ==============================================================================

# --- Global State Variables ---
fortran_process, ani = None, None
curve_plots, plot_axes = {}, {}
male_bars, female_bars = None, None
current_frame_index = 0

# -- Dynamic Plot Setup new ---


# --- Dynamic Plot Setup ---
ALL_SMALL_PLOTS_CONFIG = [{'name': 'age_pyramid', 'title': 'Age Distribution'}] + PLOT_CONFIG
num_small_plots = len(ALL_SMALL_PLOTS_CONFIG)

# --- NEW: DYNAMIC GRID CREATION LOGIC ---
num_plots_beside = min(num_small_plots, NUM_ROWS_BESIDE * SMALL_PLOT_COLS_BESIDE)
plots_config_beside = ALL_SMALL_PLOTS_CONFIG[:num_plots_beside]

num_plots_below = num_small_plots - num_plots_beside
plots_config_below = ALL_SMALL_PLOTS_CONFIG[num_plots_beside:]

# Calculate grid dimensions
has_bottom_row = num_plots_below > 0
num_rows_below = (num_plots_below + SMALL_PLOT_COLS_BELOW - 1) // SMALL_PLOT_COLS_BELOW if has_bottom_row else 0

# Adjust figure size based on layout
fig_height = (NUM_ROWS_BESIDE * 3) + (num_rows_below * 3.5)
fig = plt.figure(figsize=(16, max(8, fig_height)))

# Create main GridSpec
if not has_bottom_row:
    # Simple case: only map and right-side plots
    gs_main = gridspec.GridSpec(1, 2, width_ratios=[3, 2], wspace=0.3)
    gs_top = gs_main
    gs_bottom = None
else:
    # Complex case: top section (map+right) and a bottom section
    gs_main = gridspec.GridSpec(2, 1, height_ratios=[NUM_ROWS_BESIDE, num_rows_below], hspace=0.5)
    gs_top = gs_main[0].subgridspec(1, 2, width_ratios=[3, 2], wspace=0.2)
    gs_bottom = gs_main[1].subgridspec(num_rows_below, SMALL_PLOT_COLS_BELOW, hspace=0.7, wspace=0.3)

# 1. Map Plot (always in the top-left part of the grid)
ax_map = fig.add_subplot(gs_top[0, 0], projection=ccrs.PlateCarree())
ax_map.coastlines(); ax_map.add_feature(cfeature.BORDERS, linestyle=':')
ax_map.add_feature(cfeature.LAND, facecolor='lightgray'); ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')
ax_map.set_extent([-10, 30, 35, 70])
map_title = ax_map.set_title('Agent positions (Frame 0)')
scatter = ax_map.scatter([], [], s=5, transform=ccrs.PlateCarree())

# 2. Grid of small plots beside the map
gs_right = gs_top[0, 1].subgridspec(NUM_ROWS_BESIDE, SMALL_PLOT_COLS_BESIDE, hspace=0.7, wspace=0.3)
all_plot_grids = [(gs_right, plots_config_beside, SMALL_PLOT_COLS_BESIDE)]
if gs_bottom:
    all_plot_grids.append((gs_bottom, plots_config_below, SMALL_PLOT_COLS_BELOW))

# This loop creates all small plots in their correct grids
for grid_spec, configs, num_cols in all_plot_grids:
    for i, config in enumerate(configs):
        row, col = i // num_cols, i % num_cols
        ax = fig.add_subplot(grid_spec[row, col])
        plot_axes[config['name']] = ax
        ax.set_title(config['title'])

        if config['name'] == 'age_pyramid':
            age_bins = [0, 10, 20, 30, 40, 50, 60, 70, 80, 100]
            bin_labels = [f"{b}-{b+9}" for b in age_bins[:-1]]+["80+"]
            y_pos = np.arange(len(bin_labels))
            male_bars = ax.barh(y_pos, np.zeros(len(y_pos)), align='center', color="steelblue", edgecolor="black", label="Male")
            female_bars = ax.barh(y_pos, np.zeros(len(y_pos)), align='center', color="green", edgecolor="black", label="Female")
            ax.set_yticks(y_pos, labels=bin_labels); ax.invert_yaxis()
            ax.set_xlabel("Percentage (%)"); ax.legend(loc='lower right'); ax.grid(axis='x', linestyle='--', alpha=0.7)
        else: # Curve plot
            line, = ax.plot([], [], color="blue")
            ax.set_xlabel(f"Time (x{TIMESTEP_INCREMENT})"); ax.set_ylabel(config['y_label'])
            ax.grid(True); ax.set_ylim(0, 10); ax.set_xlim(0, 50)
            curve_plots[config['name']] = {'line': line, 'config': config, 'x_data': [], 'y_data': []}

# --- Functions (start_simulation, stop_simulation are unchanged) ---
def start_simulation():
    global fortran_process
    print("🚀 Starting Fortran simulation...")
    os.makedirs(DATA_DIRECTORY, exist_ok=True)
    for f in glob.glob(os.path.join(DATA_DIRECTORY, 'agents_plotting_data_*.csv')): os.remove(f)
    try:
        fortran_process = subprocess.Popen([FORTRAN_EXECUTABLE, str(TIMESTEP_INCREMENT)])
        print(f"Fortran process started with PID: {fortran_process.pid}")
    except FileNotFoundError:
        print(f"❌ ERROR: Fortran executable not found at '{FORTRAN_EXECUTABLE}'"); fortran_process = None

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

    # --- DEBUGGING ENHANCEMENTS ---
    print(f"Waiting for file: '{data_file}'...")
    wait_time, max_wait_sec = 0, 60  # Increased timeout to 60 seconds
    last_print_time = time.time()

    while not os.path.exists(data_file):
        time.sleep(0.1)
        wait_time += 0.1

        # Print a waiting message every 3 seconds to show it's working
        if time.time() - last_print_time > 3:
            print(f"   ...still waiting ({int(wait_time)}s elapsed)...")
            last_print_time = time.time()
            
        if wait_time > max_wait_sec or (fortran_process and fortran_process.poll() is not None):
            print("\nSimulation finished or timed out waiting for the next data file.")
            
            # List files in directory to help debug filename mismatches
            try:
                print(f"Checking contents of '{DATA_DIRECTORY}' directory...")
                files_in_dir = os.listdir(DATA_DIRECTORY)
                if not files_in_dir:
                    print("   -> The data directory is empty.")
                else:
                    print("   -> Found files:")
                    for f in files_in_dir:
                        print(f"      - '{f}'")
                    print("\nCheck if the filenames above match what the script is waiting for.")
                    print("A common issue is extra spaces in Fortran-generated filenames.")
            except FileNotFoundError:
                print(f"   -> The data directory '{DATA_DIRECTORY}' does not exist.")
            
            stop_simulation(None)
            return

    try:
        print(f"Processing data for timestep {timestep_to_find}...") # Your print statement
        
        required_cols = set()
        for p in PLOT_CONFIG:
            if p['aggregation'] in ['mean', 'std']: required_cols.add(p['column'])
            elif p['aggregation'] == 'count_if': 
                col_name = p['condition'].split()[0]
                required_cols.add(col_name)
        
        base_cols = ['id', 'pos_x', 'pos_y', 'gender', 'age', 'population']
        all_csv_cols = base_cols + sorted(list(required_cols - set(base_cols)))
        df = pd.read_csv(data_file, sep='\s+', skiprows=1, header=None, names=all_csv_cols)
        
        if df.empty: stop_simulation(None); return

        total_agents = len(df)
        df['age'] = (df['age'] / 52.0).clip(upper=100)
        
        map_title.set_text(f'Agent positions (t = {timestep_to_find})')
        colors = df['population'].map({1: 'blue', 2: 'green', 3: 'orange'}).fillna('black')
        scatter.set_offsets(df[['pos_x', 'pos_y']].values); scatter.set_color(colors)
        
        ax_age = plot_axes['age_pyramid']; age_bins = [0, 10, 20, 30, 40, 50, 60, 70, 80, 100]
        males = df[df['gender'] == 'M']['age']; females = df[df['gender'] == 'F']['age']
        male_counts, _ = np.histogram(males, bins=age_bins); female_counts, _ = np.histogram(females, bins=age_bins)
        male_rel = (male_counts/total_agents*100) if total_agents>0 else 0; female_rel = (female_counts/total_agents*100) if total_agents>0 else 0
        for bar, width in zip(male_bars, male_rel): bar.set_width(width)
        for bar, width in zip(female_bars, -female_rel): bar.set_width(width)
        ax_age.set_title(f"Age Distribution (N={total_agents})")
        max_pc = max(10, np.max(male_rel) if len(male_rel)>0 else 0, np.max(female_rel) if len(female_rel)>0 else 0)
        ax_age.set_xlim(-max_pc-5, max_pc+5)

        for plot_name, plot_data in curve_plots.items():
            config = plot_data['config']; ax = plot_axes[plot_name]; value = 0
            if config['aggregation'] == 'count': value = total_agents
            elif config['aggregation'] == 'mean': value = df[config['column']].mean() if not df.empty else 0
            elif config['aggregation'] == 'std': value = df[config['column']].std() if not df.empty else 0
            elif config['aggregation'] == 'count_if': 
                value = len(df.query(config['condition']))
            
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

