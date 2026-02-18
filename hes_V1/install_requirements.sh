
set -e

echo "Installing requirements..."

echo "Updating package list..."
apt-get update

echo "Installing system dependencies..."

apt-get install -y gfortran python3-dev python3-numpy libnetcdf-dev libnetcdff-dev python3-pip

echo "Installing Python dependencies..."
# Install Python packages required for the project


pip3 install numpy matplotlib netCDF4 PyQt5 pyopengl pyqtgraph meson ninja --break-system-packages

echo "Installation complete."
