#!/usr/bin/env python3

from pathlib import Path
import pandas as pd


def analyze_file(file_path):
    try:
        df = pd.read_csv(file_path)
    except Exception as e:
        print(f"ERROR reading {file_path}: {e}")
        return 0, 0, 0

    required_cols = {"mother_id", "father_id"}
    missing_cols = required_cols - set(df.columns)

    if missing_cols:
        print(f"SKIPPING {file_path.name}: missing columns {missing_cols}")
        return 0, 0, len(df)

    # mother_id == -1 and father_id == -1
    both_missing = (df["mother_id"] == -1) & (df["father_id"] == -1)

    # mother_id != father_id and neither is -1
    mismatched = (
        (df["mother_id"] != -1)
        & (df["father_id"] != -1)
        & (df["mother_id"] == df["father_id"])
    )

    return int(both_missing.sum()), int(mismatched.sum()), len(df)


def main():
    # Resolve relative to project root, not current working directory
    script_dir = Path(__file__).resolve().parent
    project_root = script_dir.parent

    csv_dir = project_root / "output" / "export_simulation"

    print("=" * 80)
    print(f"Searching directory: {csv_dir}")
    print(f"Exists: {csv_dir.exists()}")
    print(f"Is directory: {csv_dir.is_dir()}")
    print("=" * 80)

    if not csv_dir.exists():
        print("ERROR: Directory does not exist.")
        return

    # Search recursively in case CSVs are in subdirectories
    csv_files = sorted(csv_dir.rglob("*.csv"))

    print(f"Found {len(csv_files)} CSV file(s)\n")

    if not csv_files:
        return

    total_rows = 0
    total_both_missing = 0
    total_matched = 0

    for csv_file in csv_files:
        both_missing, matched, nrows = analyze_file(csv_file)

        total_rows += nrows
        total_both_missing += both_missing
        total_matched += matched

        print(f"{csv_file}")
        print(f"  rows: {nrows}")
        print(f"  mother_id == father_id == -1: {both_missing}")
        print(
            f"  mother_id == father_id and both != -1: {matched}"
        )
        print()

    print("=" * 80)
    print("TOTALS")
    print("=" * 80)
    print(f"Total files: {len(csv_files)}")
    print(f"Total rows: {total_rows}")
    print(f"mother_id == father_id == -1: {total_both_missing}")
    print(
        f"mother_id == father_id and both != -1: {total_matched}"
    )


if __name__ == "__main__":
    main()