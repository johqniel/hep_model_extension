#!/bin/bash
# Usage: ./run_all.sh program_name

set -e  # Exit on error

if [ -z "$1" ]; then
    echo "Usage: $0 program_name"
    exit 1
fi

PROGNAME="$1"

# Step 1: Delete contents of data/ and animation_output/
echo "Cleaning folders..."
rm -rf data/* animation_output/*

# Step 2: Run program from bin/ in background
echo "Running bin/$PROGNAME ..."
./bin/"$PROGNAME" &
PROG_PID=$!

# Step 3: Wait 30 seconds
echo "Waiting 30 seconds..."
sleep 30

# Step 4: Kill the program
echo "Stopping program $PROG_PID ..."
kill "$PROG_PID" 2>/dev/null || true

# Step 5: Activate virtual environment
echo "Activating virtual environment..."
source venv/bin/activate

# Step 6: Run Python scripts
echo "Running python/map_test_2.py ..."
python python/map_test_2.py

echo "Running python/create_animation.py ..."
python python/create_animation.py

# Step 7: Deactivate virtual environment
echo "Deactivating virtual environment..."
deactivate

# Step 8: Open the generated GIF
echo "Opening animation_output/animation.gif ..."
xdg-open animation_output/animation.gif >/dev/null 2>&1 &