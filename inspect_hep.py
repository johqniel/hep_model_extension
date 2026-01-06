import netCDF4
import os
import sys

def inspect_hep(filepath):
    print(f"Inspecting {filepath}")
    try:
        with netCDF4.Dataset(filepath, 'r') as nc:
            print("Dimensions:")
            for d in nc.dimensions:
                print(f"  {d}: {nc.dimensions[d].size}")
            
            print("\nVariables:")
            for v in nc.variables:
                var = nc.variables[v]
                print(f"  {v}: {var.dimensions}, shape={var.shape}")
                
            if 'lat' in nc.variables:
                lats = nc.variables['lat'][:]
                print(f"\nLat range: {lats[0]} to {lats[-1]} (len={len(lats)})")
                if len(lats) > 1:
                    print(f"Lat delta: {lats[1] - lats[0]}")
                    
            if 'lon' in nc.variables:
                lons = nc.variables['lon'][:]
                print(f"\nLon range: {lons[0]} to {lons[-1]} (len={len(lons)})")
                if len(lons) > 1:
                    delta = lons[1] - lons[0]
                    print(f"Lon delta (first): {delta}")
                    # Check uniformity
                    import numpy as np
                    deltas = np.diff(lons)
                    if not np.allclose(deltas, delta, atol=1e-5):
                        print(f"WARNING: Lon is NOT uniform! Min delta: {deltas.min()}, Max delta: {deltas.max()}")
                    else:
                        print("Lon is uniform.")

            if 'AccHEP' in nc.variables:
                data = nc.variables['AccHEP'][:]
                print(f"\nAccHEP range: {data.min()} to {data.max()}")
                print(f"AccHEP shape: {data.shape}")

            if 'watermask' in nc.variables:
                mask = nc.variables['watermask'][:]
                print(f"\nWatermask range: {mask.min()} to {mask.max()}")
                print(f"Watermask shape: {mask.shape}")

    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    # Use a default file if none provided
    default_file = "input/hep/gradient/AUR.nc"
    if len(sys.argv) > 1:
        target = sys.argv[1]
    else:
        target = default_file
        
    if os.path.exists(target):
        inspect_hep(target)
    else:
        print(f"File not found: {target}")
