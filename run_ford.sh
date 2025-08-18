#!/bin/bash
# Run FORD automatically on project.md after deleting old docs

set -e  # exit on error

PROJECT_FILE="project.md"

# Step 1: Remove old docs
echo "Removing old documentation..."
rm -rf doc/

# Step 2: Run FORD
echo "Running FORD on $PROJECT_FILE ..."
ford "$PROJECT_FILE"

# Optional: open docs automatically (uncomment if desired)
# xdg-open doc/index.html >/dev/null 2>&1 &
