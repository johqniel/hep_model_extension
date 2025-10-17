import os
import glob
import re
import pandas as pd
import numpy as np
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

max_expected_population = 2000
population_counts = []
#for csv_file in enumerate(csv_files):
#    df = pd.read_csv(csv_file, delim_whitespace=True, skiprows=1, 
#                     names=['id', 'pos_x', 'pos_y', 'gender', 'age', 'population'])
#    max_expected_population = max(max_expected_population,len(df))




for i, csv_file in enumerate(csv_files):
    # Read CSV with your whitespace + skiprows setup
    df = pd.read_csv(csv_file, sep='\s+', skiprows=1, 
                     names=['id', 'pos_x', 'pos_y', 'gender', 'age', 'population'])

    # Convert age from weeks to years (float)

    df['age'] = df['age'] / 52.0
    df['age'] = df['age'].clip(upper=100)     # humans above 100 should come into same bucket in histogram.
    
    # Geschlechter trennen: 
    male_ages   = df.loc[df['gender'] == 'M', 'age']
    female_ages = df.loc[df['gender'] == 'F', 'age']

    total_agents = len(df)
    population_counts.append(total_agents)


    # Two subplots, location of agents and age demografics
    fig = plt.figure(figsize=(16,8))
    gs = fig.add_gridspec(2,2,width_ratios=[2,1],height_ratios=[1,1],hspace=0.5,wspace=0.3)

    ax_map = fig.add_subplot(gs[:,0], projection=ccrs.PlateCarree())  # Map nimmt ganze linke Spalte
    ax_age = fig.add_subplot(gs[0,1])   # oben rechts
    ax_pop = fig.add_subplot(gs[1,1])   # unten rechts


# First Subplot

        #plt.figure(figsize=(10, 10))
        #ax = plt.axes(projection=ccrs.PlateCarree())

    # Add map features
    ax_map.coastlines()
    ax_map.add_feature(cfeature.BORDERS)
    ax_map.add_feature(cfeature.LAND, facecolor='lightgray')
    ax_map.add_feature(cfeature.OCEAN, facecolor='lightblue')

    # Create a color map for populations
    color_map = {1: 'blue', 2: 'green', 3: 'orange'}

    #   Map population to colors, default to black if population is unexpected
    colors = df['population'].map(color_map).fillna('black')

    # Plot agents
    ax_map.scatter(df['pos_x'], df['pos_y'], color=colors, s=2, transform=ccrs.PlateCarree())

    # Set extent for Europe (adjust if needed)
    ax_map.set_extent([-10, 30, 35, 70])

    ax_map.set_title(f'Agent positions (Frame {i+1})')

        #plt.title(f'Agent Positions over Europe (Frame {i+1})')

# Second subplot: Age demographics ---
    # Define age bins (customize as needed)
    bins = [0, 10, 20, 30, 40, 50, 60, 70, 80, 100]

    # Histogramme berechnen
    male_counts, edges = np.histogram(male_ages, bins=bins)
    female_counts, _   = np.histogram(female_ages, bins=bins)

    total_agents = len(df)
    male_rel   = male_counts / total_agents * 100
    female_rel = female_counts / total_agents * 100

    # Bar plot (manual so we can label bins)
    bin_labels = [f"{int(edges[i])}–{int(edges[i+1])}" for i in range(len(edges)-1)]
    bin_labels[-1] = "80+"  # last bin label

    # Plot: Männer positiv (oben), Frauen negativ (unten)
    ax_age.bar(bin_labels, male_rel, color="steelblue", edgecolor="black", label="Male")
    ax_age.bar(bin_labels, -female_rel, color="green", edgecolor="black", label="Female")

    ax_age.set_ylim(-20, 20)

    ax_age.legend(loc='lower right')

    # Axis labels
    ax_age.set_xlabel("Age (years)")
    ax_age.set_ylabel("Percentage of agents (%)")

    # Title includes total agents
    ax_age.set_title(f"Age Distribution (N={total_agents})")


    # y-Ticks als positive Werte anzeigen
    yticks = ax_age.get_yticks()
    ax_age.set_yticklabels([f"{abs(int(y))}%" for y in yticks])

# Third subplot: population curve:
    ax_pop.set_xlim(0, len(csv_files))           # Anzahl Frames auf x-Achse
    ax_pop.set_ylim(0, max_expected_population)  # feste maximale Agentenzahl
    ax_pop.set_ylabel("Total Agents")
    ax_pop.set_title("Population Size Over Time")

    # initiale Kurve
    line, = ax_pop.plot([], [], color="blue")

    xdata = list(range(len(population_counts)))
    ydata = population_counts

    line.set_data(xdata,ydata)

    # Rotate x labels for readability
    plt.setp(ax_age.get_xticklabels(), rotation=45, ha="right")
    
    plt.tight_layout(pad = 20.0)

    # Save frame
    out_path = os.path.join(output_folder, f'frame_{i:03d}.png')
    plt.savefig(out_path)
    plt.close()
    print(f'✅ Saved {out_path}')