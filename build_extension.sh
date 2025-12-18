#!/bin/bash
# Build the Python extension using f2py with separate compilation

# Clean up
rm -f *.o *.mod *.so

# Compiler flags
FC=gfortran
FFLAGS="-O3 -fPIC -J. -I/usr/include"

# 1. Compile dependencies (Order matters!)
echo "Compiling dependencies..."

$FC $FFLAGS -c src/globals/mod_constants.f95
$FC $FFLAGS -c src/globals/mod_config.f95

$FC $FFLAGS -c src/globals/mod_counter.f95
$FC $FFLAGS -c src/data_structures/mod_hashmap.f95
$FC $FFLAGS -c src/utilities/mod_calculations.f95
$FC $FFLAGS -c src/utilities/mod_rnorm.f95
$FC $FFLAGS -c src/utilities/mod_functions.f95
$FC $FFLAGS -c src/setup/mod_read_inputs.f95
$FC $FFLAGS -c src/data_structures/mod_grid_id.f95
$FC $FFLAGS -c src/data_structures/mod_agent_world.f95
$FC $FFLAGS -c src/utilities/mod_test_utilities.f95
$FC $FFLAGS -c src/simulation_modules/mod_modules_hash.f95
$FC $FFLAGS -c src/setup/mod_setup.f95
$FC $FFLAGS -c src/data_management/mod_export_agents_hash.f95
$FC $FFLAGS -c src/data_management/mod_extract_plottable_data.f95

# 2. Build Python extension linking against the object files
echo "Building Python extension..."

# List of object files
OBJS="mod_constants.o \
mod_config.o \

mod_counter.o \
mod_hashmap.o \
mod_calculations.o \
mod_rnorm.o \
mod_functions.o \
mod_read_inputs.o \
mod_grid_id.o \
mod_agent_world.o \
mod_test_utilities.o \
mod_modules_hash.o \
mod_setup.o \
mod_export_agents_hash.o \
mod_extract_plottable_data.o"

# Run f2py
# We pass the interface source file and the object files
# Also link against netcdff
python3 -m numpy.f2py -c src/interfaces/python_interface.f95 $OBJS -m mod_python_interface -I. --f90flags="-I." -lnetcdff

if [ $? -eq 0 ]; then
    echo "Build successful!"
    mv mod_python_interface*.so python/
    # Clean up intermediate files
    rm -f *.o *.mod
else
    echo "Build failed!"
    exit 1
fi
