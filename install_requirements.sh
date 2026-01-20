#!/bin/bash
# install_requirements.sh
# Installs necessary system packages and Python dependencies.
# Intended for Debian/Ubuntu systems.

set -e

echo "Installing requirements..."

# Check if running as root or with sudo
if [ "$EUID" -ne 0 ]; then 
  echo "Please run as root or with sudo to install system packages."
  exit 1
fi

echo "Updating package list..."
apt-get update

echo "Installing system dependencies..."
# gfortran: Fortran 95 compiler
# python3-dev: Python development headers
# python3-numpy: NumPy (often easier to install via apt for system python, but pip is also fine)
# libnetcdf-dev, libnetcdff-dev: NetCDF libraries for C and Fortran
apt-get install -y gfortran python3-dev python3-numpy libnetcdf-dev libnetcdff-dev python3-pip

echo "Installing Python dependencies..."
# Install Python packages required for the project
# Using pip to install packages for the current user (if avoiding system-wide pip install is desired, remove --break-system-packages or use venv)
# Assuming run with sudo, we might want to install globally or let user handle venv.
# Here we install globally for simplicity as requested for "copy and run".

pip3 install numpy matplotlib netCDF4 PyQt5 pyopengl pyqtgraph meson ninja --break-system-packages

echo "Installation complete."
