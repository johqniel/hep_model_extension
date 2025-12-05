import os
import glob
import re
import pandas as pd
import matplotlib.pyplot as plt
from PIL import Image

# === SETUP ===
csv_folder = 'data'
output_folder = 'animation_output'
os.makedirs(output_folder, exist_ok=True)

# === FILE MATCHING ===
pattern = os.path.join(csv_folder, 'agents_plotting_data_*.csv')

def read_mixed_csv(filename):
    with open(filename, 'r') as f:
        header = f.readline().strip().split(',')         # Read header line
    df = pd.read_csv(filename, skiprows=1, sep=r'\s+', names=header)
    return df

def extract_t(filename):
    match = re.search(r'agents_plotting_data_(\d+)\.csv$', filename)
    return int(match.group(1)) if match else -1

csv_files = sorted(glob.glob(pattern), key=extract_t)

if not csv_files:
    print("No CSV files found.")
    exit(1)

# === DYNAMIC LIMITS FROM LAST FILE ===
last_file = csv_files[-1]
df_last = read_mixed_csv(last_file)

x_min, x_max = df_last['pos_x'].min(), df_last['pos_x'].max()
y_min, y_max = df_last['pos_y'].min(), df_last['pos_y'].max()

x_margin = 2  # add some padding
y_margin = 2

x_lim = (x_min - x_margin, x_max + x_margin)
y_lim = (y_min - y_margin, y_max + y_margin)

# === PLOTTING & IMAGE CREATION ===
image_files = []

for i, csv_file in enumerate(csv_files):
    df = read_mixed_csv(csv_file)

    plt.figure(figsize=(8, 8))
    plt.scatter(df['pos_x'], df['pos_y'], color='black', s=2)
    plt.xlim(*x_lim)
    plt.ylim(*y_lim)
    plt.xlabel('X Position')
    plt.ylabel('Y Position')
    plt.title(f'Agent Distribution Frame {i+1}')
    plt.gca().set_facecolor('white')
    #plt.axis('equal')
    plt.tight_layout()

    img_path = os.path.join(output_folder, f'frame_{i:03d}.png')
    plt.savefig(img_path)
    plt.close()
    image_files.append(img_path)
    print(f'✅ Frame {i+1} saved as: {img_path}')

# === CREATE ANIMATION ===
if not image_files:
    print("No images saved. Cannot create animation.")
    exit(1)

gif_output = os.path.join(output_folder, 'agents_animation.gif')

frames = [Image.open(img) for img in image_files]
frames[0].save(gif_output, format='GIF',
               append_images=frames[1:], save_all=True,
               duration=20, loop=0)

print(f'✅ Animation saved as: {gif_output}')