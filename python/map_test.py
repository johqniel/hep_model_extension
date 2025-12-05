import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import pandas as pd

df = pd.read_csv('data/agents_plotting_data_10.csv', delim_whitespace=True, skiprows=1, names=['id', 'pos_x', 'pos_y', 'gender', 'age'])

plt.figure(figsize=(10, 10))
ax = plt.axes(projection=ccrs.PlateCarree())

# Add map features
ax.coastlines()
ax.add_feature(cfeature.BORDERS)
ax.add_feature(cfeature.LAND, facecolor='lightgray')
ax.add_feature(cfeature.OCEAN, facecolor='lightblue')

# Plot agents
ax.scatter(df['pos_x'], df['pos_y'], color='black', s=2, transform=ccrs.PlateCarree())

ax.set_extent([-10, 30, 35, 70])  # adjust to your region
plt.title('Agent Positions over Europe')
plt.tight_layout()
plt.show()
