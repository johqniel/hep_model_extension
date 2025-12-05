import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import os

# === Determine paths relative to this script ===
script_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(script_dir, '..', 'hep_control')
dims_path = os.path.join(data_dir, 'hep_dims.txt')
bin_path = os.path.join(data_dir, 'hep.bin')

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

# === Select slice to plot ===
pop_index = 0
time_index = 450
grid_slice = hep_data[:, :, pop_index, time_index]

#grid_slice = np.rot90(grid_slice,k=3)
#grid_slice = np.flip(grid_slice,axis = 1)

# === Plotting ===
extent_map = [-10, 30, 35, 70]
fig = plt.figure(figsize=(10, 6))
ax = plt.axes(projection=ccrs.PlateCarree())
ax.set_extent(extent)
ax.coastlines()
ax.add_feature(cfeature.BORDERS, linestyle=':')
ax.add_feature(cfeature.LAND, facecolor='lightgray')
ax.add_feature(cfeature.OCEAN, facecolor='lightblue')

img = ax.imshow(grid_slice, origin='lower', extent=extent,
                transform=ccrs.PlateCarree(), cmap='coolwarm',
                vmin=0, vmax=1, alpha=0.8)

plt.colorbar(img, ax=ax, orientation='vertical', shrink=0.6, label='Hep Value')
plt.title(f'Hep Map: Population {pop_index+1}, Time {time_index+1}')
plt.tight_layout()
plt.show()