import os
import glob
import re
import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature

csv_folder = 'data'
output_folder = 'animation_output'
os.makedirs(output_folder, exist_ok=True)

# Pattern for files like agent_plotting_data_10.csv, etc.
pattern = os.path.join(csv_folder, 'agents_plotting_data_*.csv')

def extract_t(filename):
    match = re.search(r'agents_plotting_data_(\d+)\.csv$', filename)
    return int(match.group(1)) if match else -1

csv_files = glob.glob(pattern)
csv_files = sorted(csv_files, key=extract_t)

for i, csv_file in enumerate(csv_files):
    # Read CSV with your whitespace + skiprows setup
    df = pd.read_csv(csv_file, delim_whitespace=True, skiprows=1, 
                     names=['id', 'pos_x', 'pos_y', 'gender', 'age', 'population'])
    
    plt.figure(figsize=(10, 10))
    ax = plt.axes(projection=ccrs.PlateCarree())

    # Add map features
    ax.coastlines()
    ax.add_feature(cfeature.BORDERS)
    ax.add_feature(cfeature.LAND, facecolor='lightgray')
    ax.add_feature(cfeature.OCEAN, facecolor='lightblue')

    # Create a color map for populations
    color_map = {1: 'blue', 2: 'green', 3: 'orange'}

    #   Map population to colors, default to black if population is unexpected
    colors = df['population'].map(color_map).fillna('black')

    # Plot agents
    ax.scatter(df['pos_x'], df['pos_y'], color=colors, s=2, transform=ccrs.PlateCarree())

    # Set extent for Europe (adjust if needed)
    ax.set_extent([-10, 30, 35, 70])

    plt.title(f'Agent Positions over Europe (Frame {i+1})')
    plt.tight_layout()

    # Save frame
    out_path = os.path.join(output_folder, f'frame_{i:03d}.png')
    plt.savefig(out_path)
    plt.close()
    print(f'✅ Saved {out_path}')