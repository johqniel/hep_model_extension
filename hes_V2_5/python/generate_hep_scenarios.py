import netCDF4
import numpy as np
import os

def create_nc_file(filename, hep_data, watermask, lat, lon, time):
    """
    Creates a NetCDF file with the given data.
    """
    print(f"Creating {filename}...")
    try:
        with netCDF4.Dataset(filename, 'w', format='NETCDF4') as nc:
            # Dimensions
            nc.createDimension('lon', len(lon))
            nc.createDimension('lat', len(lat))
            nc.createDimension('time', len(time))
            
            # Variables
            v_lat = nc.createVariable('lat', 'f8', ('lat',))
            v_lon = nc.createVariable('lon', 'f8', ('lon',))
            v_time = nc.createVariable('time', 'f8', ('time',))
            v_hep = nc.createVariable('AccHEP', 'f8', ('time', 'lat', 'lon'))
            v_water = nc.createVariable('watermask', 'i4', ('lat', 'lon'))
            
            # Data
            v_lat[:] = lat
            v_lon[:] = lon
            v_time[:] = time
            v_hep[:] = hep_data
            v_water[:] = watermask
            
            print(f"Successfully created {filename}")
            
    except Exception as e:
        print(f"Error creating {filename}: {e}")

def get_watermask(type, shape):
    """
    Generates a watermask based on type.
    1 = Land, 0 = Water
    """
    mask = np.ones(shape, dtype=int) # Default all land
    
    if type == 'circle':
        # Circle in the middle is land, rest is water
        ny, nx = shape
        y, x = np.ogrid[:ny, :nx]
        center_y, center_x = ny / 2, nx / 2
        radius = min(ny, nx) / 3 # 1/3 of dimension
        dist_from_center = np.sqrt((x - center_x)**2 + (y - center_y)**2)
        
        mask = np.zeros(shape, dtype=int) # Start with all water
        mask[dist_from_center <= radius] = 1 # Set circle to land
        
    elif type == 'none':
        pass # Already all land
        
    return mask

def generate_scenarios():
    output_dir = "input/hep/generated"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Reference File
    ref_file = "input/hep/europe/AUR.nc"
    print(f"Using reference file: {ref_file}")
    
    try:
        with netCDF4.Dataset(ref_file, 'r') as nc_ref:
            lat = nc_ref.variables['lat'][:]
            lon = nc_ref.variables['lon'][:]
            time = nc_ref.variables['time'][:]
            
            nx = len(lon)
            ny = len(lat)
            nt = len(time)
            
            print(f"Dimensions: nx={nx}, ny={ny}, nt={nt}")
            
            # --- Scenario 1: Two Valleys (No Water) ---
            # Two high HEP (1.0) areas connected by a path (0.3)
            hep_valleys = np.zeros((nt, ny, nx))
            
            # Valley 1: Center (approx index 25% x, 50% y)
            y_idx, x_idx = np.ogrid[:ny, :nx]
            
            c1_x_idx = int(nx * 0.25)
            c1_y_idx = int(ny * 0.5)
            r1 = int(min(nx, ny) * 0.15)
            mask1 = ((x_idx - c1_x_idx)**2 + (y_idx - c1_y_idx)**2) <= r1**2
            
            # Valley 2: Center (approx index 75% x, 50% y)
            c2_x_idx = int(nx * 0.75)
            c2_y_idx = int(ny * 0.5)
            r2 = int(min(nx, ny) * 0.15)
            mask2 = ((x_idx - c2_x_idx)**2 + (y_idx - c2_y_idx)**2) <= r2**2
            
            # Path: Rectangle connecting centers
            # From x=25% to x=75%, width 10%
            path_y_min = int(ny * 0.45)
            path_y_max = int(ny * 0.55)
            path_mask = (x_idx >= c1_x_idx) & (x_idx <= c2_x_idx) & (y_idx >= path_y_min) & (y_idx <= path_y_max)
            
            # Apply values
            hep_valleys[:, path_mask] = 0.3
            hep_valleys[:, mask1] = 1.0
            hep_valleys[:, mask2] = 1.0
            
            water_none = get_watermask('none', (ny, nx))
            create_nc_file(os.path.join(output_dir, "two_valleys.nc"), hep_valleys, water_none, lat, lon, time)
            
            # --- Scenario 2: Circle Island (Uniform HEP) ---
            # Uniform HEP 0.5, Circle Land Mask
            hep_uniform = np.full((nt, ny, nx), 0.5)
            water_circle = get_watermask('circle', (ny, nx))
            create_nc_file(os.path.join(output_dir, "circle_island.nc"), hep_uniform, water_circle, lat, lon, time)
            
    except Exception as e:
        print(f"Error reading reference file: {e}")

if __name__ == "__main__":
    generate_scenarios()
