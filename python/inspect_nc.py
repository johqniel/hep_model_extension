import netCDF4
import sys

def inspect_nc(path):
    print(f"--- Inspecting: {path} ---")
    try:
        with netCDF4.Dataset(path, 'r') as nc:
            print("Dimensions:")
            for dim in nc.dimensions.values():
                print(f"  {dim.name}: {dim.size}")
            
            print("\nVariables:")
            for var in nc.variables.values():
                print(f"  {var.name}: {var.dimensions}, {var.shape}, {var.dtype}")
                
    except Exception as e:
        print(f"Error opening {path}: {e}")
    print("\n")

if __name__ == "__main__":
    inspect_nc("input/hep/generated/two_valleys.nc")
    inspect_nc("input/hep/generated/circle_island.nc")
