#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <netcdf.h>
#include <cstdlib>
#include <cmath>

// Error handling macro
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int main(int argc, char** argv) {
    int ncid, x_dimid, y_dimid, time_dimid;
    int lat_varid, lon_varid, time_varid, hep_varid;
    int dimids[3];
    int retval;

    // Dimensions for Whole Earth at 0.15 degree resolution
    // Lon: 360 / 0.15 = 2400
    // Lat: 180 / 0.15 = 1200
    int NX = 2400;
    int NY = 1200;
    int NTIME = 1;

    std::string filename = "input/hep/generated/earth_hep.nc";
    std::string mask_filename = "data/earth_mask_2400x1200.bin";

    double delta_lon = 0.15;
    double delta_lat = 0.15;
    double lon_0 = -180.0;
    double lat_0 = -90.0;

    std::cout << "Generating " << filename << " with dimensions: " 
              << NX << "x" << NY << " and " << NTIME << " time steps." << std::endl;

    // 1. Read Mask Data
    std::cout << "Reading mask data from " << mask_filename << "..." << std::endl;
    std::ifstream mask_file(mask_filename, std::ios::binary);
    if (!mask_file) {
        std::cerr << "Error: Could not open mask file! Did you run the python script?" << std::endl;
        return 1;
    }

    // Mask is uint8, but we want to store double in HEP for compatibility
    std::vector<uint8_t> mask_data(NX * NY);
    mask_file.read(reinterpret_cast<char*>(mask_data.data()), NX * NY);
    if (!mask_file) {
        std::cerr << "Error: Failed to read all mask data (expected " << NX * NY << " bytes)." << std::endl;
        return 1;
    }
    mask_file.close();

    // 2. Create NetCDF File
    if ((retval = nc_create(filename.c_str(), NC_CLOBBER, &ncid)))
        ERR(retval);

    // Define dimensions
    if ((retval = nc_def_dim(ncid, "lon", NX, &x_dimid)))
        ERR(retval);
    if ((retval = nc_def_dim(ncid, "lat", NY, &y_dimid)))
        ERR(retval);
    if ((retval = nc_def_dim(ncid, "time", NTIME, &time_dimid)))
        ERR(retval);

    // Define variables
    if ((retval = nc_def_var(ncid, "lat", NC_DOUBLE, 1, &y_dimid, &lat_varid)))
        ERR(retval);
    if ((retval = nc_def_var(ncid, "lon", NC_DOUBLE, 1, &x_dimid, &lon_varid)))
        ERR(retval);
    if ((retval = nc_def_var(ncid, "time", NC_DOUBLE, 1, &time_dimid, &time_varid)))
        ERR(retval);
    
    // AccHEP (time, lat, lon)
    dimids[0] = time_dimid;
    dimids[1] = y_dimid;
    dimids[2] = x_dimid;

    if ((retval = nc_def_var(ncid, "AccHEP", NC_DOUBLE, 3, dimids, &hep_varid)))
        ERR(retval);

    // Attributes
    nc_put_att_text(ncid, lat_varid, "units", 13, "degrees_north");
    nc_put_att_text(ncid, lon_varid, "units", 12, "degrees_east");
    nc_put_att_text(ncid, hep_varid, "longname", 36, "Accessible Human Existence Potential");

    // End define mode
    if ((retval = nc_enddef(ncid)))
        ERR(retval);

    // 3. Prepare Data
    std::vector<double> lats(NY);
    std::vector<double> lons(NX);
    std::vector<double> times(NTIME);

    for(int i=0; i<NY; i++) lats[i] = lat_0 + (double)i * delta_lat;
    for(int i=0; i<NX; i++) lons[i] = lon_0 + (double)i * delta_lon;
    for(int i=0; i<NTIME; i++) times[i] = 0.0; // Static time

    // Write Coordinates
    if ((retval = nc_put_var_double(ncid, lat_varid, lats.data())))
        ERR(retval);
    if ((retval = nc_put_var_double(ncid, lon_varid, lons.data())))
        ERR(retval);
    if ((retval = nc_put_var_double(ncid, time_varid, times.data())))
        ERR(retval);

    // Write HEP Data
    // Convert mask (NY, NX) to (1, NY, NX)
    std::vector<double> hep_data(NX * NY * NTIME);
    
    // NetCDF order is (time, lat, lon)
    // Our mask is (lat, lon) -> row major
    // So mask index [y * NX + x] maps directly to flat index.
    
    for (int t = 0; t < NTIME; t++) {
        for (int i = 0; i < NY * NX; i++) {
            hep_data[i] = (double)mask_data[i];
        }
    }

    if ((retval = nc_put_var_double(ncid, hep_varid, hep_data.data())))
        ERR(retval);

    // Close
    if ((retval = nc_close(ncid)))
        ERR(retval);

    std::cout << "Successfully generated " << filename << std::endl;

    return 0;
}
