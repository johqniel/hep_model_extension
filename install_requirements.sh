#!/bin/bash
# install_requirements.sh
# Installs system and python dependencies for the HEP simulation.
# Usage: ./install_requirements.sh

set -e

# ANSI Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== HEP Simulation Dependency Installer ===${NC}"

# 1. System Dependencies (Debian/Ubuntu)
if [ -f /etc/debian_version ]; then
    echo -e "${YELLOW}Detected Debian/Ubuntu system. Installing system packages...${NC}"
    sudo apt update
    sudo apt install -y \
        gfortran \
        make \
        libnetcdf-dev \
        libnetcdff-dev \
        python3-dev \
        python3-pip \
        python3-venv \
        python3-tk 
    echo -e "${GREEN}System packages installed.${NC}"
else
    echo -e "${RED}Warning: Not specific support for non-Debian systems in this script yet.${NC}"
    echo -e "Please ensure you have the following installed manually:"
    echo "  - gfortran"
    echo "  - make"
    echo "  - libnetcdf (dev headers)"
    echo "  - libnetcdff (Fortran bindings)"
    echo "  - python3 (dev headers)"
    echo ""
    read -p "Press Enter to continue if you have installed these, or Ctrl+C to abort."
fi

# 2. Python Environment
echo -e "${YELLOW}Setting up Python environment...${NC}"

# Check for venv
if [ ! -d "venv" ]; then
    echo "Creating virtual environment 'venv'..."
    python3 -m venv venv
else
    echo "Virtual environment 'venv' already exists."
fi

# Activate venv
source venv/bin/activate

# Upgrade pip
pip install --upgrade pip

# Install Python Packages
echo -e "${YELLOW}Installing Python packages...${NC}"
pip install \
    "numpy<2" \
    PyQt5 \
    pyqtgraph \
    netCDF4 \
    PyOpenGL

echo -e "${GREEN}Python packages installed.${NC}"

# 3. Build Extension
echo -e "${YELLOW}Building Fortran extension...${NC}"
if [ -f "build_extension.sh" ]; then
    chmod +x build_extension.sh
    ./build_extension.sh
else
    echo -e "${RED}Error: build_extension.sh not found!${NC}"
    exit 1
fi

echo -e "${GREEN}=== Installation Complete! ===${NC}"
echo -e "To run the simulation:"
echo -e "  1. source venv/bin/activate"
echo -e "  2. python3 python/simulation.py"
