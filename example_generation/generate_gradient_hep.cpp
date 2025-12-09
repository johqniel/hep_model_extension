#include <iostream>
#include <vector>
#include <string>
#include <netcdf.h>
#include <cstdlib>

// Error handling macro
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int main(int argc, char** argv) {
    int ncid, x_dimid, y_dimid, time_dimid;
    int lat_varid, lon_varid, time_varid, hep_varid;
    int dimids[3];
    int retval;

    // Default dimensions
    int NX = 100;
    int NY = 100;
    int NTIME = 1;
    std::string filename = "gradient_hep.nc";

    // Parse command line arguments
    if (argc > 1) NX = std::atoi(argv[1]);
    if (argc > 2) NY = std::atoi(argv[2]);
    if (argc > 3) NTIME = std::atoi(argv[3]);
    if (argc > 4) filename = argv[4];

    std::cout << "Generating " << filename << " with dimensions: " 
              << NX << "x" << NY << " and " << NTIME << " time steps." << std::endl;

    // Create the file
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
    // lat
    if ((retval = nc_def_var(ncid, "lat", NC_DOUBLE, 1, &y_dimid, &lat_varid)))
        ERR(retval);
    // lon
    if ((retval = nc_def_var(ncid, "lon", NC_DOUBLE, 1, &x_dimid, &lon_varid)))
        ERR(retval);
    // time
    if ((retval = nc_def_var(ncid, "time", NC_DOUBLE, 1, &time_dimid, &time_varid)))
        ERR(retval);
    
    // AccHEP (time, lat, lon) -> Note: NetCDF is typically (time, lat, lon) or (lat, lon) depending on convention.
    // The existing file had: double AccHEP(time, lat, lon)
    dimids[0] = time_dimid;
    dimids[1] = y_dimid;
    dimids[2] = x_dimid;

    if ((retval = nc_def_var(ncid, "AccHEP", NC_DOUBLE, 3, dimids, &hep_varid)))
        ERR(retval);

    // End define mode
    if ((retval = nc_enddef(ncid)))
        ERR(retval);

    // Write coordinates
    std::vector<double> lats(NY);
    std::vector<double> lons(NX);
    std::vector<double> times(NTIME);

    for(int i=0; i<NY; i++) lats[i] = (double)i;
    for(int i=0; i<NX; i++) lons[i] = (double)i;
    for(int i=0; i<NTIME; i++) times[i] = (double)i;

    if ((retval = nc_put_var_double(ncid, lat_varid, lats.data())))
        ERR(retval);
    if ((retval = nc_put_var_double(ncid, lon_varid, lons.data())))
        ERR(retval);
    if ((retval = nc_put_var_double(ncid, time_varid, times.data())))
        ERR(retval);

    // Write Data
    // Gradient: Left (West, index 0) = 1.0, Right (East, index NX-1) = 0.0
    // Value depends only on longitude index.
    std::vector<double> data(NX * NY * NTIME);
    
    for (int t = 0; t < NTIME; t++) {
        for (int y = 0; y < NY; y++) {
            for (int x = 0; x < NX; x++) {
                // Linear interpolation: 1.0 at x=0, 0.0 at x=NX-1
                double val = 1.0 - (double)x / (double)(NX - 1);
                if (NX == 1) val = 1.0; // Avoid division by zero if single point

                // Index mapping for (time, lat, lon) flattened array
                // layout is usually row-major in C, so last dim varies fastest
                // index = t * (NY * NX) + y * (NX) + x
                size_t index = t * NY * NX + y * NX + x;
                data[index] = val;
            }
        }
    }

    if ((retval = nc_put_var_double(ncid, hep_varid, data.data())))
        ERR(retval);

    // Close the file
    if ((retval = nc_close(ncid)))
        ERR(retval);

    std::cout << "Successfully generated " << filename << std::endl;

    return 0;
}
