import sys
import os
import queue
import time
import netCDF4
import numpy as np

# Adjust imports from python dir
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'python')))

from full_simulation import DataWriterThread

def test_writer():
    output_nc = "test_output.nc"
    if os.path.exists(output_nc):
        os.remove(output_nc)

    dlon = 100
    dlat = 50
    npops = 3

    print("Initializing DataWriterThread...")
    writer = DataWriterThread(output_nc, dlon, dlat, npops)
    writer.start()

    print("Feeding mock data...")
    for t in range(5):
        hep_mock = np.random.rand(dlon, dlat, npops).astype(np.float32)
        density_mock = np.random.rand(dlon, dlat).astype(np.float64)
        
        writer.data_queue.put({
            'tick': t * 10,
            'hep': hep_mock,
            'density': density_mock
        })

    print("Stopping writer...")
    writer.stop()
    writer.wait()

    print("Checking NetCDF file format...")
    ds = netCDF4.Dataset(output_nc, 'r')
    print("Dimensions:", list(ds.dimensions.keys()))
    print("Variables:", list(ds.variables.keys()))
    
    # Assert checks
    assert 'time' in ds.dimensions
    assert ds.dimensions['time'].isunlimited()
    assert ds.dimensions['lon'].size == dlon
    assert ds.dimensions['lat'].size == dlat
    assert ds.dimensions['pop'].size == npops
    
    assert ds.variables['tick'].shape == (5,)
    assert ds.variables['hep'].shape == (5, dlon, dlat, npops)
    assert ds.variables['human_density'].shape == (5, dlon, dlat)
    
    print("All assertions passed!")
    ds.close()

if __name__ == "__main__":
    test_writer()
