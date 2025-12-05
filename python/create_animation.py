import os
from PIL import Image

# Folder where frames are stored
frames_folder = 'animation_output'
output_gif = 'animation_output/animation.gif'

# Get list of image files sorted by filename
frames = sorted([
    os.path.join(frames_folder, f) for f in os.listdir(frames_folder)
    if f.endswith('.png')
])

# Load images
images = [Image.open(frame).copy() for frame in frames]

# Save as GIF with 25 fps => duration per frame in ms = 1000 / 25 = 40 ms
if images:
    images[0].save(
        output_gif,
        save_all=True,
        append_images=images[1:],
        duration=40,
        loop=0
    )
    print(f'✅ GIF created at {output_gif}')
else:
    print('⚠️ No images found in', frames_folder)