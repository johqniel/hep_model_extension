import sys
import os
import numpy as np
import matplotlib.pyplot as plt

# Add python directory so we can import mod_python_interface
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'python')))

try:
    import mod_python_interface
    # f2py compiles the Fortran module inside an extension module of the same name.
    if hasattr(mod_python_interface, 'mod_python_interface'):
        mod_python_interface = mod_python_interface.mod_python_interface
except ImportError as e:
    print(f"Failed to import mod_python_interface: {e}")
    print("Ensure you have run ./build.sh and the .so file is in the python directory.")
    sys.exit(1)

def main():
    print("Initializing simulation to get base density...")
    # 1. Init
    mod_python_interface.init_sim_step_1()
    
    # Must explicitly set the config path so NetCDF doesn't fail looking for missing files
    mod_python_interface.set_simulation_config_path("input/config/main_fortran_config.nml")
    
    # Must explicitly set the HEP map paths so NetCDF doesn't fail
    full_paths = [
        "input/hep/europe/AUR.nc",
        "input/hep/europe/NEA.nc",
        "input/hep/europe/MIX.nc"
    ]
    mod_python_interface.set_custom_hep_paths(full_paths, len(full_paths))

    mod_python_interface.init_sim_step_2_part_1()
    mod_python_interface.init_sim_step_2_part_2_arrays_only()
    nx, ny, npops = mod_python_interface.get_grid_dims()
    mod_python_interface.init_sim_step_2_part_2_chunk(1, nx)
    mod_python_interface.init_sim_step_2_part_3() # Populate environment data
    mod_python_interface.init_sim_step_3(False) # do not skip gen
    
    # Run 1 tick so density is populated
    mod_python_interface.step_simulation(1)
    print(f"Simulation stepped. Grid NX={nx}, NY={ny}")
    
    # 2. Extract Base Density
    raw_density = mod_python_interface.get_grid_density(nx, ny)
    print(f"Raw density fetched. Max: {np.max(raw_density)}")
    
    try:
        radius_input = input("Enter smoothing radius (default 4): ").strip()
        radius = int(radius_input) if radius_input else 4
    except ValueError:
        print("Invalid radius! Defaulting to 4.")
        radius = 4

    try:
        iter_input = input("Enter number of standard smoothing iterations (default 3): ").strip()
        iterations = int(iter_input) if iter_input else 3
    except ValueError:
        print("Invalid iterations! Defaulting to 3.")
        iterations = 3
        
    print(f"\nProceeding with local_radius={radius}, iterations={iterations}\n")

    std_radius = 4  # Standard smoothing radius set to 4

    # 3. Apply smooth_box_filter globally (Using Standard Radius = 4)
    print(f"Applying smooth_box_filter (radius={std_radius})...")
    smooth_1 = mod_python_interface.apply_smooth_box_filter(raw_density, nx, ny, std_radius)
    print(f"smooth_box_filter returned. Max: {np.max(smooth_1)}")
    
    # 4. Apply local_box_filter iteratively to simulate cell-by-cell smoothing (Using Custom Radius)
    print(f"Applying local_box_filter (radius={radius})...")
    smooth_2 = mod_python_interface.apply_local_box_filter(raw_density, nx, ny, radius)
    print(f"local_box_filter returned. Max: {np.max(smooth_2)}")
    
    # 5. Apply smooth_box_filter multiple times iteratively (Using Standard Radius = 4)
    print(f"Applying smooth_box_filter {iterations} times iteratively (radius={std_radius})...")
    iterative_smooth = raw_density.copy()
    for i in range(iterations):
        iterative_smooth = mod_python_interface.apply_smooth_box_filter(iterative_smooth, nx, ny, std_radius)
    print(f"Iterated smooth_box_filter returned. Max: {np.max(iterative_smooth)}")
    
    # 6. Extract Peaks using Peak Finding Algorithm on ALL surfaces
    print(f"Running find_local_maxima on topologies...")
    threshold = 0.05
    
    def extract_peaks(z_data):
        labels, num_peaks = mod_python_interface.apply_find_local_maxima(z_data, nx, ny, threshold)
        return labels, num_peaks
        
    labels_raw, peaks_raw = extract_peaks(raw_density)
    labels_1, peaks_1 = extract_peaks(smooth_1)
    labels_2, peaks_2 = extract_peaks(smooth_2)
    labels_iter, peaks_iter = extract_peaks(iterative_smooth)
    
    print(f"Algorithm identified peaks! [Raw: {peaks_raw}, S1: {peaks_1}, S2: {peaks_2}, Iter: {peaks_iter}]")
    
    from matplotlib.widgets import CheckButtons

    # 7. Visualize (3D Surfaces using matplotlib to avoid OpenGL crashes)
    fig = plt.figure(figsize=(20, 5))
    fig.canvas.manager.set_window_title("Smoothing Algorithm Comparison")
    
    # Adjust layout to make room for a bottom button
    plt.subplots_adjust(bottom=0.2)
    
    # Create X and Y meshgrids
    x = np.arange(nx)
    y = np.arange(ny)
    X, Y = np.meshgrid(x, y, indexing='ij')

    scatter_collections = []

    def create_subplot(position, title, z_data, labels, num_peaks):
        ax = fig.add_subplot(1, 4, position, projection='3d')
        ax.set_title(title, fontweight='bold', fontsize=12)
        
        # Plot surface
        surf = ax.plot_surface(X, Y, z_data, cmap='viridis', edgecolor='none', alpha=0.8)
        
        # Superimpose Peak nodes natively mapped locally
        if num_peaks > 0:
            peak_x, peak_y = np.where(labels > 0)
            peak_z = z_data[peak_x, peak_y] + (np.max(z_data) * 0.05)
            scatter = ax.scatter(peak_x, peak_y, peak_z, color='red', s=30, label=f'Peaks ({num_peaks})')
            scatter_collections.append(scatter)
            ax.legend(loc='upper right')
            
        ax.set_zbound(0, max(np.max(z_data) * 1.1 + 1e-5, 0.1))
        
        # Adjust viewing angle
        ax.view_init(elev=35, azim=-45)
        
    create_subplot(1, "Raw Density", raw_density, labels_raw, peaks_raw)
    create_subplot(2, f"Standard Filter (r={std_radius})", smooth_1, labels_1, peaks_1)
    create_subplot(3, f"Local Agent Filter (r={radius})", smooth_2, labels_2, peaks_2)
    create_subplot(4, f"Standard Filter x{iterations} (r={std_radius})", iterative_smooth, labels_iter, peaks_iter)
    
    # Add a CheckButton to toggle peaks
    ax_button = plt.axes([0.45, 0.05, 0.1, 0.1])
    check = CheckButtons(ax_button, ['Show Peaks'], [True])
    
    def toggle_peaks(label):
        visible = check.get_status()[0]
        for scatter in scatter_collections:
            scatter.set_visible(visible)
        fig.canvas.draw_idle()
        
    check.on_clicked(toggle_peaks)
    
    # Enable intuitive mouse-wheel zooming across all graphs synchronously
    def on_scroll(event):
        scale_factor = 0.9 if event.button == 'up' else 1.11
        for ax in fig.axes:
            if hasattr(ax, 'get_zlim'):  # Ensure it is a 3D axis, not the button axis
                xlim = ax.get_xlim()
                ylim = ax.get_ylim()
                zlim = ax.get_zlim()
                
                # Scale from the center of the bounding box
                x_center, x_range = sum(xlim) / 2, (xlim[1] - xlim[0]) * scale_factor
                y_center, y_range = sum(ylim) / 2, (ylim[1] - ylim[0]) * scale_factor
                z_center, z_range = sum(zlim) / 2, (zlim[1] - zlim[0]) * scale_factor
                
                ax.set_xlim([x_center - x_range / 2, x_center + x_range / 2])
                ax.set_ylim([y_center - y_range / 2, y_center + y_range / 2])
                ax.set_zlim([z_center - z_range / 2, z_center + z_range / 2])
        fig.canvas.draw_idle()
        
    fig.canvas.mpl_connect('scroll_event', on_scroll)
    
    plt.show()

if __name__ == '__main__':
    main()
