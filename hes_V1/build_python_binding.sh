#!/bin/bash
# build_python_binding.sh
# Links object files from build/ and builds the Python extension using f2py.

# Stop on error
set -e

# Switch to build directory
cd build
BUILD_DIR=$(pwd)

echo "Building in $BUILD_DIR..."

# Run f2py
# We reference the source file via relative path
# We link against the static library libhep.a located in the current dir (build/)
# We explicitly pass the source file to wrapper generation
# -L. : Look for libraries in current dir (which is build/)
# -lhep : Link against libhep.a
# -lnetcdff : Link against NetCDF fortran
# -I"$BUILD_DIR": Absolute path to module files

python3 -m numpy.f2py \
    -c ../src/interfaces/python_interface.f95 \
    -m mod_python_interface \
    -I"$BUILD_DIR" \
    --f90flags="-I$BUILD_DIR" \
    -L"$BUILD_DIR" -lhep -lnetcdff

# Find and move the .so file
if ls mod_python_interface*.so 1> /dev/null 2>&1; then
     echo "Moving extension from build/ to python/ directory..."
     mv mod_python_interface*.so ../python/
else
    echo "Error: Extension file not found in $BUILD_DIR!"
    exit 1
fi
