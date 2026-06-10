#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include <filesystem>
#include <iomanip>
#include <netcdf.h>

namespace fs = std::filesystem;

// Error handling macro
#define ERR(e) { std::cerr << "NetCDF Error at line " << __LINE__ << ": " << nc_strerror(e) << std::endl; return false; }

// Haversine formula for grid cell area approximation (replicated from Fortran)
double area_of_gridcell(int i, int j, const std::vector<double>& lon_in, const std::vector<double>& lat_in, double R) {
    double lat1, lat2, lon1, lon2;
    double pi = 3.14159265358979323846;

    int NX = lon_in.size();
    int NY = lat_in.size();

    // Compute latitude borders
    if (j == 0) {
        lat1 = lat_in[0] - (lat_in[1] - lat_in[0]) / 2.0;
    } else {
        lat1 = (lat_in[j] + lat_in[j-1]) / 2.0;
    }

    if (j == NY - 1) {
        lat2 = lat_in[j] + (lat_in[j] - lat_in[j-1]) / 2.0;
    } else {
        lat2 = (lat_in[j+1] + lat_in[j]) / 2.0;
    }

    // Compute longitude borders
    if (i == 0) {
        lon1 = lon_in[0] - (lon_in[1] - lon_in[0]) / 2.0;
    } else {
        lon1 = (lon_in[i] + lon_in[i-1]) / 2.0;
    }

    if (i == NX - 1) {
        lon2 = lon_in[i] + (lon_in[i] - lon_in[i-1]) / 2.0;
    } else {
        lon2 = (lon_in[i+1] + lon_in[i]) / 2.0;
    }

    // Latitude distance using haversine
    double haversine_lat = pow(sin(std::abs(lat2 - lat1) * pi / (2.0 * 180.0)), 2);
    double dist_lat = 2.0 * R * atan2(sqrt(haversine_lat), sqrt(1.0 - haversine_lat));

    // Longitude distances at both latitudes
    double haversine1 = pow(cos(lat2 * pi / 180.0), 2) * pow(sin(std::abs(lon2 - lon1) * pi / (2.0 * 180.0)), 2);
    double dist_lon1 = 2.0 * R * atan2(sqrt(haversine1), sqrt(1.0 - haversine1));

    double haversine2 = pow(cos(lat1 * pi / 180.0), 2) * pow(sin(std::abs(lon2 - lon1) * pi / (2.0 * 180.0)), 2);
    double dist_lon2 = 2.0 * R * atan2(sqrt(haversine2), sqrt(1.0 - haversine2));

    // Trapezoid-like area approximation
    double term1 = (dist_lon1 + dist_lon2) / 2.0;
    double inside_sqrt = dist_lat * dist_lat - pow((dist_lon1 - dist_lon2) / 2.0, 2);
    double term2 = sqrt(inside_sqrt > 0.0 ? inside_sqrt : 0.0);
    return term1 * term2;
}

// Convert string to lower case
std::string to_lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::tolower(c); });
    return s;
}

struct CapacityResult {
    std::string filepath;
    int nx;
    int ny;
    int ntime;
    double max_capacity;
    int max_time_step;
    double min_capacity;
    double avg_capacity;
    bool success;
};

bool process_hep_file(const std::string& filepath, double NC, bool clean_data, CapacityResult& res) {
    res.filepath = filepath;
    res.success = false;

    int ncid, retval;
    if ((retval = nc_open(filepath.c_str(), NC_NOWRITE, &ncid))) {
        std::cerr << "Error: Could not open file " << filepath << " : " << nc_strerror(retval) << std::endl;
        return false;
    }

    // Get dimension IDs
    int lon_dimid = -1, lat_dimid = -1, time_dimid = -1;
    size_t NX = 0, NY = 0, NTIME = 1;

    int ndims;
    if ((retval = nc_inq_ndims(ncid, &ndims))) ERR(retval);

    for (int d = 0; d < ndims; ++d) {
        char name[NC_MAX_NAME + 1];
        size_t len;
        if ((retval = nc_inq_dim(ncid, d, name, &len))) ERR(retval);
        std::string dim_name = to_lower(name);
        if (dim_name.find("lon") != std::string::npos || dim_name == "x") {
            lon_dimid = d;
            NX = len;
        } else if (dim_name.find("lat") != std::string::npos || dim_name == "y") {
            lat_dimid = d;
            NY = len;
        } else if (dim_name.find("time") != std::string::npos || dim_name == "t") {
            time_dimid = d;
            NTIME = len;
        }
    }

    if (lon_dimid == -1 || lat_dimid == -1) {
        std::cerr << "Error: Could not find lon/lat dimensions in " << filepath << std::endl;
        nc_close(ncid);
        return false;
    }

    res.nx = NX;
    res.ny = NY;
    res.ntime = NTIME;

    // Inquire variable IDs
    int lon_varid = -1, lat_varid = -1;
    if ((retval = nc_inq_varid(ncid, "lon", &lon_varid))) {
        int nvars;
        nc_inq_nvars(ncid, &nvars);
        for (int v = 0; v < nvars; ++v) {
            char name[NC_MAX_NAME + 1];
            nc_inq_varname(ncid, v, name);
            std::string var_name = to_lower(name);
            if (var_name.find("lon") != std::string::npos || var_name == "x") {
                lon_varid = v;
                break;
            }
        }
    }
    if ((retval = nc_inq_varid(ncid, "lat", &lat_varid))) {
        int nvars;
        nc_inq_nvars(ncid, &nvars);
        for (int v = 0; v < nvars; ++v) {
            char name[NC_MAX_NAME + 1];
            nc_inq_varname(ncid, v, name);
            std::string var_name = to_lower(name);
            if (var_name.find("lat") != std::string::npos || var_name == "y") {
                lat_varid = v;
                break;
            }
        }
    }

    if (lon_varid == -1 || lat_varid == -1) {
        std::cerr << "Error: Could not find lon/lat variables in " << filepath << std::endl;
        nc_close(ncid);
        return false;
    }

    // Read coordinate values
    std::vector<double> lons(NX);
    std::vector<double> lats(NY);
    if ((retval = nc_get_var_double(ncid, lon_varid, lons.data()))) ERR(retval);
    if ((retval = nc_get_var_double(ncid, lat_varid, lats.data()))) ERR(retval);

    // Get AccHEP variable
    int hep_varid = -1;
    if ((retval = nc_inq_varid(ncid, "AccHEP", &hep_varid))) {
        std::cerr << "Error: Could not find variable AccHEP in " << filepath << std::endl;
        nc_close(ncid);
        return false;
    }

    int hep_ndims;
    int hep_dimids[8];
    if ((retval = nc_inq_varndims(ncid, hep_varid, &hep_ndims))) ERR(retval);
    if ((retval = nc_inq_vardimid(ncid, hep_varid, hep_dimids))) ERR(retval);

    // Map AccHEP dimensions
    int hep_time_dim_idx = -1;
    int hep_lat_dim_idx = -1;
    int hep_lon_dim_idx = -1;
    size_t hep_dim_len[8];

    for (int i = 0; i < hep_ndims; ++i) {
        char name[NC_MAX_NAME + 1];
        if ((retval = nc_inq_dim(ncid, hep_dimids[i], name, &hep_dim_len[i]))) ERR(retval);
        std::string dim_name = to_lower(name);
        if (dim_name.find("time") != std::string::npos || dim_name == "t") {
            hep_time_dim_idx = i;
        } else if (dim_name.find("lat") != std::string::npos || dim_name == "y") {
            hep_lat_dim_idx = i;
        } else if (dim_name.find("lon") != std::string::npos || dim_name == "x") {
            hep_lon_dim_idx = i;
        }
    }

    if (hep_lat_dim_idx == -1 || hep_lon_dim_idx == -1) {
        std::cerr << "Error: AccHEP variable is missing lat or lon dimensions in " << filepath << std::endl;
        nc_close(ncid);
        return false;
    }

    // Read AccHEP data
    size_t total_size = 1;
    for (int i = 0; i < hep_ndims; ++i) {
        total_size *= hep_dim_len[i];
    }
    std::vector<double> hep_data(total_size);
    if ((retval = nc_get_var_double(ncid, hep_varid, hep_data.data()))) ERR(retval);

    // Read watermask if exists
    int wm_varid = -1;
    bool has_wm = false;
    int wm_ndims = 0;
    int wm_dimids[8];
    size_t wm_dim_len[8];
    std::vector<double> wm_data;

    int wm_lat_dim_idx = -1;
    int wm_lon_dim_idx = -1;

    if (nc_inq_varid(ncid, "watermask", &wm_varid) == NC_NOERR) {
        if (nc_inq_varndims(ncid, wm_varid, &wm_ndims) == NC_NOERR &&
            nc_inq_vardimid(ncid, wm_varid, wm_dimids) == NC_NOERR) {
            
            size_t wm_total_size = 1;
            for (int i = 0; i < wm_ndims; ++i) {
                nc_inq_dim(ncid, wm_dimids[i], nullptr, &wm_dim_len[i]);
                wm_total_size *= wm_dim_len[i];
                char name[NC_MAX_NAME + 1];
                nc_inq_dim(ncid, wm_dimids[i], name, nullptr);
                std::string dim_name = to_lower(name);
                if (dim_name.find("lat") != std::string::npos || dim_name == "y") {
                    wm_lat_dim_idx = i;
                } else if (dim_name.find("lon") != std::string::npos || dim_name == "x") {
                    wm_lon_dim_idx = i;
                }
            }
            wm_data.resize(wm_total_size);
            if (nc_get_var_double(ncid, wm_varid, wm_data.data()) == NC_NOERR) {
                has_wm = true;
            }
        }
    }

    // Calculate grid cell areas
    std::vector<double> areas(NX * NY);
    for (size_t y = 0; y < NY; ++y) {
        for (size_t x = 0; x < NX; ++x) {
            areas[y * NX + x] = area_of_gridcell(x, y, lons, lats, 6371.0);
        }
    }

    // Calculate capacities for each time step
    std::vector<double> capacities(NTIME, 0.0);
    double max_cap = -1.0;
    int max_t = 0;
    double min_cap = 1e20;
    double sum_cap = 0.0;

    for (size_t t = 0; t < NTIME; ++t) {
        double current_capacity = 0.0;
        for (size_t y = 0; y < NY; ++y) {
            for (size_t x = 0; x < NX; ++x) {
                // Fetch HEP value
                size_t flat_idx = 0;
                if (hep_ndims == 3) {
                    size_t coords[3];
                    coords[hep_time_dim_idx] = t;
                    coords[hep_lat_dim_idx] = y;
                    coords[hep_lon_dim_idx] = x;
                    flat_idx = coords[0] * hep_dim_len[1] * hep_dim_len[2] + coords[1] * hep_dim_len[2] + coords[2];
                } else {
                    size_t coords[2];
                    coords[hep_lat_dim_idx] = y;
                    coords[hep_lon_dim_idx] = x;
                    flat_idx = coords[0] * hep_dim_len[1] + coords[1];
                }
                double hep_val = hep_data[flat_idx];

                if (clean_data) {
                    if (hep_val > 1.0e10 || hep_val < 0.0 || std::isnan(hep_val) || std::isinf(hep_val)) {
                        hep_val = 0.0;
                    } else if (hep_val > 1.0) {
                        hep_val = 1.0;
                    }
                }

                // Fetch watermask value
                double wm_val = 1.0;
                if (has_wm) {
                    size_t wm_flat_idx = 0;
                    size_t coords[2];
                    coords[wm_lat_dim_idx] = y;
                    coords[wm_lon_dim_idx] = x;
                    wm_flat_idx = coords[0] * wm_dim_len[1] + coords[1];
                    wm_val = wm_data[wm_flat_idx];
                }

                if (clean_data) {
                    if (wm_val > 1.0e10 || std::isnan(wm_val) || std::isinf(wm_val)) {
                        wm_val = 0.0; // Treat masked as water
                    }
                }

                if (wm_val > 0.0 && hep_val > 0.0) {
                    double area = areas[y * NX + x];
                    current_capacity += hep_val * NC * (area / 100.0);
                }
            }
        }
        capacities[t] = current_capacity;
        if (current_capacity > max_cap) {
            max_cap = current_capacity;
            max_t = t;
        }
        if (current_capacity < min_cap) {
            min_cap = current_capacity;
        }
        sum_cap += current_capacity;
    }

    res.max_capacity = max_cap;
    res.max_time_step = max_t;
    res.min_capacity = min_cap;
    res.avg_capacity = sum_cap / NTIME;
    res.success = true;

    nc_close(ncid);
    return true;
}

int main() {
    double NC = 5.0;
    std::cout << "Starting carrying capacity computation for NC = " << NC << std::endl;
    std::cout << "Scanning input/hep/ directory recursively..." << std::endl;

    std::vector<std::string> files;
    for (const auto& entry : fs::recursive_directory_iterator("input/hep")) {
        if (entry.is_regular_file() && entry.path().extension() == ".nc") {
            files.push_back(entry.path().string());
        }
    }
    std::sort(files.begin(), files.end());

    std::cout << "\n--- COMPUTATION WITH DATA CLEANSING (Capping values > 1.0 to 1.0, FillValues/NaNs/Negatives to 0.0) ---" << std::endl;
    std::cout << std::left << std::setw(50) << "File Path"
              << std::right << std::setw(12) << "Grid"
              << std::right << std::setw(8) << "Steps"
              << std::right << std::setw(16) << "Max Capacity"
              << std::right << std::setw(16) << "Min Capacity"
              << std::right << std::setw(16) << "Avg Capacity"
              << std::endl;
    std::cout << std::string(118, '-') << std::endl;

    for (const auto& file : files) {
        CapacityResult res;
        if (process_hep_file(file, NC, true, res)) {
            std::string grid_str = std::to_string(res.nx) + "x" + std::to_string(res.ny);
            std::cout << std::left << std::setw(50) << file.substr(10) // strip "input/hep/"
                      << std::right << std::setw(12) << grid_str
                      << std::right << std::setw(8) << res.ntime
                      << std::right << std::setw(16) << std::fixed << std::setprecision(1) << res.max_capacity
                      << std::right << std::setw(16) << res.min_capacity
                      << std::right << std::setw(16) << res.avg_capacity
                      << std::endl;
        }
    }
    std::cout << std::string(118, '-') << std::endl;

    return 0;
}
