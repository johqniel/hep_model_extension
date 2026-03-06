How to install everything required to run this version of the HES Model and its interface on your computer: 

1. do to data/hescor/dnoguesk 
   -copy the latest version to where ever you want to run it. 
   -run: cp -r /data/hescor/dnoguesk/hes_V1 /path/to/destination

2. Make sure you have the following installed on your machine:
   -run: apt-get install -y gfortran python3-dev python3-numpy libnetcdf-dev libnetcdff-dev python3-pip
  
3. Go into the newly created folder. 
   - cd /path/to/destination/hes_V1

4. Initialize a virtual machine
   - python3 -m venv vm
  
5. activate the virtual machine
   - source vm/bin/activate
   - (to exit the vm type 'deactivate' in the terminal)

6. Install Python packages on the virtual machine
   - pip3 install numpy matplotlib netCDF4 PyQt5 pyopengl pyqtgraph meson ninja




