import numpy as np
import os
import sys

def generate_mask():
    try:
        from global_land_mask import globe
    except ImportError:
        print("Error: global-land-mask not installed.")
        print("Please run: pip install global-land-mask")
        sys.exit(1)

    print("Generating Whole Earth Land Mask...")
    
    # Dimensions
    # Resolution 0.15 degrees
    dlon = 0.15
    dlat = 0.15
    
    # Range
    lon_min, lon_max = -180.0, 180.0
    lat_min, lat_max = -90.0, 90.0
    
    nx = int((lon_max - lon_min) / dlon)
    ny = int((lat_max - lat_min) / dlat)
    
    print(f"Grid: {nx} x {ny} (Resolution: {dlon})")
    
    # Generate Coordinates
    # We want center of cells or corners? NetCDF usually defines points.
    # Let's align with the standard typically used: logic similar to Meshgrid.
    
    # Linspace might include endpoint, so be careful.
    # We want nx points.
    lons = np.linspace(lon_min, lon_max - dlon, nx)
    lats = np.linspace(lat_min, lat_max - dlat, ny)
    
    # Create Meshgrid
    # Note: global_land_mask expects lat, lon
    lon_grid, lat_grid = np.meshgrid(lons, lats)
    
    print("Checking land mask...")
    is_land = globe.is_land(lat_grid, lon_grid)
    
    # Convert to binary (0=Water, 1=Land)
    # The simulation HEP might expect 1 for Habitable, 0 for Uninhabitable.
    # Typically Water = 0 (or -1), Land = 1.
    # User said: "0 where there is water and 1 where there is land"
    
    mask = np.zeros((ny, nx), dtype=np.uint8)
    mask[is_land] = 1
    
    # Save to binary file
    output_dir = "data"
    os.makedirs(output_dir, exist_ok=True)
    output_file = os.path.join(output_dir, "earth_mask_2400x1200.bin")
    
    print(f"Saving to {output_file}...")
    mask.tofile(output_file)
    print("Done.")

if __name__ == "__main__":
    generate_mask()
