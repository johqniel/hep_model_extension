#!/bin/bash
# build_fortran.sh
# Compiles the Fortran modules into object files in the build/ directory.

# Stop on error
set -e

echo "Building Fortran modules..."

# Create build directory if it doesn't exist
mkdir -p build

# Compiler settings
FC=gfortran
# -J build: put .mod files in build/
# -I /usr/include: include path
FFLAGS="-O3 -fPIC -J build -I/usr/include"

# Helper function to compile
compile() {
    src_file=$1
    obj_file="build/$(basename "${src_file%.*}").o"
    $FC $FFLAGS -c "$src_file" -o "$obj_file"
}

# 1. Compile dependencies (Order matters!)
# Globals
compile src/globals/mod_constants.f95
compile src/globals/mod_config.f95

# Utilities & Data Structures
compile src/globals/mod_counter.f95
compile src/data_structures/mod_hashmap.f95
compile src/utilities/mod_calculations.f95
compile src/utilities/mod_rnorm.f95
compile src/utilities/mod_functions.f95
compile src/setup/mod_read_inputs.f95
compile src/data_structures/mod_grid_id.f95
compile src/data_structures/mod_agent_world.f95
compile src/utilities/mod_test_utilities.f95

# Birth-Death Target (Dependency for strict)
compile src/simulation_modules/mod_birth_death_target.f95

# Simulation Modules (Refactored)
compile src/simulation_modules/mod_birth_technical.f95
compile src/simulation_modules/mod_birth_death_agb.f95
compile src/simulation_modules/mod_birth_death_strict.f95
compile src/simulation_modules/mod_birth_death_probabilistic.f95
compile src/simulation_modules/mod_birth_death_new.f95
compile src/simulation_modules/mod_test_modules.f95
compile src/simulation_modules/mod_yaping_development.f95
compile src/simulation_modules/mod_reviewed_modules.f95

# Watershed Clustering
compile src/data_structures/mod_watershed.f95
compile src/data_structures/mod_clustering.f95

# Setup & Data Management
compile src/setup/mod_initial_agents.f95
compile src/data_management/mod_export_agents_hash.f95
compile src/data_management/mod_extract_plottable_data.f95

# Analysis
compile src/analyze/mod_analyze.f95

echo "Creating static library libhep.a..."
rm -f build/libhep.a
ar rcs build/libhep.a build/*.o

echo "Fortran modules compiled and library created in build/libhep.a."

echo "Building standalone Fortran binary..."
$FC $FFLAGS src/main_fortran.f95 -o build/main_fortran -Lbuild -lhep -I build -lnetcdff -lnetcdf
