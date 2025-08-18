import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # needed for 3D
import glob
import os
import imageio.v2 as imageio  # use v2 API to avoid warnings

# Input/output folders
input_folder = "hep_control"
output_folder = "hep_animation_output"
os.makedirs(output_folder, exist_ok=True)

output_gif = os.path.join(output_folder, "hep_control.gif")

# Collect CSV files
csv_files = sorted(glob.glob(os.path.join(input_folder, "*.csv")))
frames = []

for idx, csv_file in enumerate(csv_files):
    if os.path.getsize(csv_file) == 0:
        print(f"Skipping empty file: {csv_file}")
        continue

    data = np.loadtxt(csv_file, delimiter=",")

    # Ensure data is 2D
    data = np.atleast_2d(data)

    nx, ny = data.shape
    X, Y = np.meshgrid(np.arange(ny), np.arange(nx))

    # Plot
    fig = plt.figure(figsize=(6,5))
    ax = fig.add_subplot(111, projection="3d")
    surf = ax.plot_surface(X, Y, data, cmap="viridis")

    ax.set_title(os.path.basename(csv_file))
    ax.set_xlabel("Y index")
    ax.set_ylabel("X index")
    ax.set_zlabel("Value")

    # Save PNG frame into output folder
    frame_file = os.path.join(output_folder, f"frame_{idx:03d}.png")
    plt.savefig(frame_file, dpi=100, bbox_inches="tight")
    plt.close(fig)

    frames.append(imageio.imread(frame_file))

# Write gif only if frames exist
if frames:
    imageio.mimsave(output_gif, frames, duration=0.5)
    print(f"GIF saved as {output_gif}")
else:
    print("No valid CSVs found, GIF not created.")