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
    # == EXAMPLE: Count agents based on a condition ==
    {
        'name': 'pregnant_agents',
        'title': 'Number of Pregnant Agents',
        'aggregation': 'count_if',        # Use the new aggregation type
        'condition_column': 'is_pregnant',# The column to check from your Fortran output
        'condition_value': 1,             # The value to count (e.g., 1 for true)
        'y_label': 'Pregnant Agents Count'
    },
]
# ==============================================================================

# --- Global State Variables ---
fortran_process = None
current_frame_index = 0
ani = None
curve_plots = {}

# --- Dynamic Plot Setup ---
num_curve_plots = len(PLOT_CONFIG)
num_right_rows = 1 + num_curve_plots
fig = plt.figure(figsize=(16, 4 * num_right_rows))
gs = gridspec.GridSpec(num_right_rows, 2, width_ratios=[2, 1], height_ratios=[2] + [1]*num_curve_plots)
fig.subplots_adjust(hspace=0.5, wspace=0.3)

# 1. Map Plot
ax_map = fig.add_subplot(gs[:, 0], projection=ccrs.PlateCarree())
ax_map.coastlines()
ax_map.add_feature(cfeature.BORDERS, linestyle=':')
ax_map.add_feature(cfeature.LAND, facecolor='lightgray')
ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')
ax_map.set_extent([-10, 30, 35, 70])
map_title = ax_map.set_title('Agent positions (Frame 0)')
scatter = ax_map.scatter([], [], s=5, transform=ccrs.PlateCarree())

# 2. Age Pyramid
ax_age = fig.add_subplot(gs[0, 1])
age_bins = [0, 10, 20, 30, 40, 50, 60, 70, 80, 100]
bin_labels = [f"{age_bins[i]}-{age_bins[i+1]}" for i in range(len(age_bins)-1)]
bin_labels[-1] = "80+"
y_pos = np.arange(len(bin_labels))
male_bars = ax_age.barh(y_pos, np.zeros(len(y_pos)), align='center', color="steelblue", edgecolor="black", label="Male")
female_bars = ax_age.barh(y_pos, np.zeros(len(y_pos)), align='center', color="green", edgecolor="black", label="Female")
ax_age.set_yticks(y_pos, labels=bin_labels)
ax_age.invert_yaxis()
ax_age.set_xlabel("Percentage of agents (%)")
ax_age.set_title("Age Distribution (N=0)")
ax_age.legend(loc='lower right')
ax_age.grid(axis='x', linestyle='--', alpha=0.7)

# 3. Modular Curve Plots
for i, config in enumerate(PLOT_CONFIG):
    ax = fig.add_subplot(gs[i + 1, 1])
    line, = ax.plot([], [], color="blue")
    ax.set_title(config['title'])
    ax.set_xlabel(f"Time Step (x{TIMESTEP_INCREMENT})")
    ax.set_ylabel(config['y_label'])
    ax.grid(True)
    ax.set_ylim(0, 10)
    ax.set_xlim(0, 50)
    curve_plots[config['name']] = {
        'ax': ax, 'line': line, 'config': config, 'x_data': [], 'y_data': []
    }

# --- Functions (start_simulation, stop_simulation are unchanged) ---
def start_simulation():
    global fortran_process
    print("🚀 Starting Fortran simulation...")
    os.makedirs(DATA_DIRECTORY, exist_ok=True)
    files = glob.glob(os.path.join(DATA_DIRECTORY, 'agents_plotting_data_*.csv'))
    for f in files: os.remove(f)
    try:
        fortran_process = subprocess.Popen([FORTRAN_EXECUTABLE, str(TIMESTEP_INCREMENT)])
        print(f"Fortran process started with PID: {fortran_process.pid}")
    except FileNotFoundError:
        print(f"❌ ERROR: Fortran executable not found at '{FORTRAN_EXECUTABLE}'")
        fortran_process = None

def stop_simulation(event):
    global fortran_process, ani
    if fortran_process and fortran_process.poll() is None:
        print("🛑 Stopping Fortran simulation...")
        fortran_process.terminate()
        fortran_process = None
        map_title.set_text('Simulation Stopped by User')
        fig.canvas.draw_idle()
    if ani: ani.event_source.stop()

def update(frame):
    global current_frame_index
    current_frame_index += 1
    timestep_to_find = current_frame_index * TIMESTEP_INCREMENT
    data_file = os.path.join(DATA_DIRECTORY, f'agents_plotting_data_{timestep_to_find}.csv')

    wait_time, max_wait_sec = 0, 10
    while not os.path.exists(data_file):
        time.sleep(0.1)
        wait_time += 0.1
        if wait_time > max_wait_sec or (fortran_process and fortran_process.poll() is not None):
            print("Simulation finished or timed out waiting for new data file.")
            stop_simulation(None)
            return

    try:
        # --- NEW: Smartly collect all required columns from the config ---
        required_cols = set()
        for p in PLOT_CONFIG:
            if p['aggregation'] in ['mean', 'std']:
                required_cols.add(p['column'])
            elif p['aggregation'] == 'count_if':
                required_cols.add(p['condition_column'])
        
        base_cols = ['id', 'pos_x', 'pos_y', 'gender', 'age', 'population']
        all_csv_cols = base_cols + sorted(list(required_cols - set(base_cols)))
        
        df = pd.read_csv(data_file, sep='\s+', skiprows=1, header=None, names=all_csv_cols)
        
        if df.empty:
            stop_simulation(None); return

        total_agents = len(df)
        df['age'] = (df['age'] / 52.0).clip(upper=100)
        
        # --- Update Static Plots (Map, Age Pyramid) ---
        map_title.set_text(f'Agent positions (t = {timestep_to_find})')
        colors = df['population'].map({1: 'blue', 2: 'green', 3: 'orange'}).fillna('black')
        scatter.set_offsets(df[['pos_x', 'pos_y']].values)
        scatter.set_color(colors)
        
        males = df[df['gender'] == 'M']['age']
        females = df[df['gender'] == 'F']['age']
        male_counts, _ = np.histogram(males, bins=age_bins)
        female_counts, _ = np.histogram(females, bins=age_bins)
        male_rel = (male_counts / total_agents * 100) if total_agents > 0 else 0
        female_rel = (female_counts / total_agents * 100) if total_agents > 0 else 0
        for bar, width in zip(male_bars, male_rel): bar.set_width(width)
        for bar, width in zip(female_bars, -female_rel): bar.set_width(width)
        ax_age.set_title(f"Age Distribution (N={total_agents})")
        max_percent = max(10, np.max(male_rel) if len(male_rel)>0 else 0, np.max(female_rel) if len(female_rel)>0 else 0)
        ax_age.set_xlim(-max_percent-5, max_percent+5)

        # --- MODULAR CURVE PLOT UPDATE ---
        for plot_name, plot_data in curve_plots.items():
            config = plot_data['config']
            ax = plot_data['ax']
            value = 0
            
            # --- NEW: Handle the 'count_if' aggregation ---
            if config['aggregation'] == 'count':
                value = total_agents
            elif config['aggregation'] == 'mean':
                value = df[config['column']].mean()
            elif config['aggregation'] == 'std':
                value = df[config['column']].std()
            elif config['aggregation'] == 'count_if':
                value = len(df[df[config['condition_column']] == config['condition_value']])
            
            plot_data['x_data'].append(current_frame_index)
            plot_data['y_data'].append(value)
            plot_data['line'].set_data(plot_data['x_data'], plot_data['y_data'])
            
            current_ymax = ax.get_ylim()[1]
            if value >= current_ymax:
                ax.set_ylim(bottom=0, top=max(1, current_ymax * 2))
            
            current_xmax = ax.get_xlim()[1]
            if current_frame_index >= current_xmax:
                ax.set_xlim(left=0, right=current_xmax * 2)

    except Exception as e:
        print(f"Error processing file {data_file}: {e}")

# --- Main Execution ---
if __name__ == "__main__":
    ax_stop_button = fig.add_axes([0.9, 0.01, 0.08, 0.04])
    stop_button = Button(ax_stop_button, 'Stop')
    stop_button.on_clicked(stop_simulation)
    ani = FuncAnimation(fig, update, frames=None, interval=UPDATE_INTERVAL_MS, repeat=False, cache_frame_data=False)
    start_simulation()
    if fortran_process:
        plt.tight_layout(rect=[0, 0.03, 0.9, 1])
        plt.show()
    else:
        print("Could not start the simulation. Exiting.")