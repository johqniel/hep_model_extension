#!/bin/bash
# build.sh
# Orchestrates the full build process.

# Stop on error
set -e

echo "Starting full build..."

# Clean up artifacts
echo "Cleaning up..."
rm -rf build
# Re-create build directory
mkdir -p build
# Clean old extension
rm -f python/mod_python_interface*.so

# Run the build steps
./build_fortran.sh
./build_python_binding.sh

echo "Build complete!"
