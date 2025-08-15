src_dir: src
output_dir: doc
graph: true

preprocess: True

files:
  - setup
  - simulation_modules
  - agent_management
  - data_management
  - grid_management
  - matrix_calculations
  - merge_modules
  - omp
  - utilities

graphviz:
  output_format: svg
  save_dot_files: true