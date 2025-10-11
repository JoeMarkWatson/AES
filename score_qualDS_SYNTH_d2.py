# score_data_in_chunks.py

import os
import time
import pandas as pd
import numpy as np
from openai import OpenAI
from concurrent.futures import ThreadPoolExecutor, as_completed
from collections import defaultdict

# --- CONFIGURATION ---
QUAL_DATA_PATH = "output/synth_qual_resps_d2.csv"
TRAIN_RESP_PATH = 'output/synth_resps_train_d2UPDATED.csv'
TEST_RESP_PATH = 'output/synth_resps_test_d2UPDATED.csv'

OUTPUT_CHUNK_DIR = '/Users/jw/Desktop/dt/jbs_work/Psychometric_position/ivan proj/synth_results_chunks/'
PROMPT_DIR = './prompts/scoring_prompts/'
MODEL_NAME = "deepseek-chat"

NUM_CHUNKS = 100
MAX_WORKERS = 40
COUNTRY_OF_ORIGIN = "American"

PROMPT_FILENAMES = {
    'a': 'score_qual_a_d2.txt', 'b': 'score_qual_b_d2.txt',
    'c': 'score_qual_c_d2.txt', 'd': 'score_qual_d_d2.txt'
}


# --- CORE FUNCTIONS ---

# NOTE: This function has been updated with detailed error logging from your working script.
def get_llm_score(client, prompt_template, prompt_data):
    """Calls the LLM API and returns a single integer score."""
    try:
        formatted_prompt = prompt_template.format(**prompt_data)
        response = client.chat.completions.create(
            model=MODEL_NAME,
            messages=[{"role": "user", "content": formatted_prompt}],
            max_tokens=5,
            temperature=0,
            seed=123
        )
        content = response.choices[0].message.content.strip()
        return int(content)
    except ValueError:
        response_text = content if 'content' in locals() else 'N/A'
        print(f"  - WARNING: Could not convert LLM response ('{response_text}') to int. Storing None.", flush=True)
        return None
    except Exception as e:
        # This will now print the actual error message from the API.
        print(f"  - ERROR: API call failed: {e}. Storing None.", flush=True)
        time.sleep(1)
        return None


def load_prompts(prompt_dir, filenames):
    """Loads prompt templates from text files into a dictionary."""
    templates = {}
    for key, filename in filenames.items():
        path = os.path.join(prompt_dir, filename)
        with open(path, 'r', encoding='utf-8') as f:
            templates[key] = f.read()
    return templates


# --- MAIN EXECUTION BLOCK ---

def main():
    """Main function to run the data scoring pipeline in chunks."""
    try:
        client = OpenAI(api_key=os.environ.get("DEEPSEEK_API_KEY"), base_url="https://api.deepseek.com/v3.1_terminus_expires_on_20251015")  # not using https://api.deepseek.com/v1. Instead, using endpoint for v3.1 to promote consistency with real data scoring.
    except TypeError:
        print("ERROR: DEEPSEEK_API_KEY environment variable not set. Exiting.")
        exit()

    prompt_templates = load_prompts(PROMPT_DIR, PROMPT_FILENAMES)

    print("\n--- Loading and Preparing Synthetic Data ---")
    synth_qual_data = pd.read_csv(QUAL_DATA_PATH)
    synth_resps_train = pd.read_csv(TRAIN_RESP_PATH)
    synth_resps_test = pd.read_csv(TEST_RESP_PATH)
    synth_resps = pd.concat([synth_resps_train, synth_resps_test], ignore_index=True)
    all_data = pd.merge(synth_resps, synth_qual_data, on='ID')
    if 'trans_text' in all_data.columns:
        all_data.rename(columns={'trans_text': 'E'}, inplace=True)
    print(f"Successfully loaded and merged a total of {len(all_data)} data rows.")

    os.makedirs(OUTPUT_CHUNK_DIR, exist_ok=True)

    data_chunks = np.array_split(all_data, NUM_CHUNKS)
    print(f"\nData has been split into {len(data_chunks)} chunks of approximately {len(data_chunks[0])} rows each.")

    ss = 'the sentence completion prompt: "'
    gpt_query = {
        'E': ['the essay title: "My saddest experience"'],
        **{f'SC{i}': [ss + prompt + '"'] for i, prompt in enumerate([
            'Over the past week, I...', 'My parents don’t know that I...', 'On most days, my mood...',
            'When I stand by the window, I...', 'I actually...', 'My family...', 'At night, I often...',
            'Recently, I plan to...', 'I should...', 'Recently, my body...', 'The knife on the table can...',
            'Next week, I plan to...'
        ], start=1)}
    }

    for i, chunk_df in enumerate(data_chunks):
        chunk_num = i + 1
        chunk_output_path = os.path.join(OUTPUT_CHUNK_DIR, f'chunk_{chunk_num}_results.csv')

        print(f"\n{'=' * 50}\n--- Starting processing for Chunk {chunk_num}/{NUM_CHUNKS} ---\n{'=' * 50}")

        if os.path.exists(chunk_output_path):
            print(f"Chunk {chunk_num} already processed. Skipping.")
            continue

        results_map = {}
        future_to_metadata = {}

        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            for index, row in chunk_df.iterrows():
                student_id = row['ID']
                results_map[student_id] = {'ID': student_id}
                for col_prefix, prompt_full_text in gpt_query.items():
                    if col_prefix in row and pd.notna(row[col_prefix]):
                        for prompt_key, template in prompt_templates.items():
                            prompt_data = {
                                'id': student_id, 'col_prefix': col_prefix,
                                'writing_type': 'an essay' if col_prefix == 'E' else 'a sentence completion',
                                'writing_prompt': prompt_full_text[0],
                                'writing_prompt_short': prompt_full_text[0].split('"')[1],
                                'humans_response': row[col_prefix], 'country_of_origin': COUNTRY_OF_ORIGIN
                            }
                            future = executor.submit(get_llm_score, client, template, prompt_data)
                            future_to_metadata[future] = (student_id, col_prefix, prompt_key)

            total_tasks = len(future_to_metadata)
            print(f"Executing {total_tasks} API calls for this chunk...")
            processed_calls = 0

            for future in as_completed(future_to_metadata):
                student_id, col_prefix, prompt_key = future_to_metadata[future]
                score = future.result()
                column_name = f"{col_prefix}{prompt_key}"
                results_map[student_id][column_name] = score
                processed_calls += 1

                if processed_calls % 100 == 0:
                    print(f"  ... {processed_calls}/{total_tasks} calls complete for this chunk.", flush=True)

        # Save the completed chunk
        chunk_results_df = pd.DataFrame(list(results_map.values()))
        chunk_df_base = chunk_df.drop(columns=[col for col in chunk_results_df.columns if col != 'ID'], errors='ignore')
        output_chunk_df = pd.merge(chunk_df_base, chunk_results_df, on='ID', how='left')
        output_chunk_df.to_csv(chunk_output_path, index=False)
        print(f"--- ✅ Chunk {chunk_num} complete! Results saved to {chunk_output_path} ---")

    print("\n\nAll chunks have been processed!")


if __name__ == "__main__":
    main()