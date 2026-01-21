# merge_chunks.py

import os
import pandas as pd
import glob

# --- CONFIGURATION ---
CHUNK_DIR = '/Users/jw/Desktop/dt/jbs_work/Psychometric_position/ivan proj/synth_results_chunks/'

# The final, single CSV file you want to create
FINAL_OUTPUT_PATH = 'output/scored_essays/DS_sentsOutput_SYNTH_d2.csv'

# --- SCRIPT ---
def merge_csv_chunks(chunk_dir, final_path):
    """Finds all chunk*.csv files in a directory, merges them, and saves a single file."""

    # Find all CSV files in the directory that start with 'chunk_'
    chunk_files = glob.glob(os.path.join(chunk_dir, 'chunk_*.csv'))

    if not chunk_files:
        print(f"No chunk files found in directory: {chunk_dir}")
        return

    print(f"Found {len(chunk_files)} chunk files to merge.")

    # Read each chunk file into a DataFrame and store in a list
    df_list = []
    for f in sorted(chunk_files):  # Sorting ensures order, though not strictly necessary
        print(f"  - Reading {os.path.basename(f)}")
        df_list.append(pd.read_csv(f))

    # Concatenate all DataFrames in the list into a single DataFrame
    print("\nMerging all chunks...")
    final_df = pd.concat(df_list, ignore_index=True)

    # Save the final merged DataFrame
    final_df.to_csv(final_path, index=False)
    print(f"\n✅ Success! Merged {len(final_df)} rows into final file:\n{final_path}")


if __name__ == "__main__":
    merge_csv_chunks(CHUNK_DIR, FINAL_OUTPUT_PATH)