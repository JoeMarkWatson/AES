import os
import time
import pandas as pd
from openai import OpenAI
from concurrent.futures import ThreadPoolExecutor, as_completed
from collections import defaultdict

# --- CONFIGURATION ---
QUAL_DATA_PATH = "output/synth_qual_resps_d2.csv"
TRAIN_RESP_PATH = 'output/synth_resps_train_d2UPDATED.csv'
TEST_RESP_PATH = 'output/synth_resps_test_d2UPDATED.csv'

OUTPUT_CSV_PATH = '/Users/jw/Desktop/dt/jbs_work/Psychometric_position/ivan proj/DS_sentsOutput_4prompts_incrementalSYNTH_d2.csv'
PROMPT_DIR = './prompts/scoring_prompts/'
MODEL_NAME = "deepseek-chat"

# NOTE: SAVE_INTERVAL is now based on the number of fully processed ROWS (students).
SAVE_INTERVAL = 2  # Save after every 2 complete rows
MAX_WORKERS = 20
COUNTRY_OF_ORIGIN = "American"

PROMPT_FILENAMES = {
    'a': 'score_qual_a_d2.txt',
    'b': 'score_qual_b_d2.txt',
    'c': 'score_qual_c_d2.txt',
    'd': 'score_qual_d_d2.txt'
}


# --- CORE FUNCTIONS (get_llm_score, load_prompts remain the same) ---
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
        print(
            f"  - WARNING: Could not convert LLM response ('{response_text}') to int for ID {prompt_data.get('id', 'Unknown')}, Column {prompt_data.get('col_prefix', 'Unknown')}. Storing None.")
        return None
    except Exception as e:
        print(
            f"  - ERROR: API call failed for ID {prompt_data.get('id', 'Unknown')}, Column {prompt_data.get('col_prefix', 'Unknown')}: {e}. Storing None.")
        time.sleep(1)
        return None


def load_prompts(prompt_dir, filenames):
    """Loads prompt templates from text files into a dictionary."""
    templates = {}
    print("--- Loading Prompts ---")
    for key, filename in filenames.items():
        try:
            path = os.path.join(prompt_dir, filename)
            with open(path, 'r', encoding='utf-8') as f:
                templates[key] = f.read()
            print(f"Successfully loaded prompt '{filename}'")
        except FileNotFoundError:
            print(f"FATAL: Prompt file not found at {path}. Exiting.")
            exit()
    return templates


# --- MAIN EXECUTION BLOCK ---

def main():
    """Main function to run the synthetic data scoring pipeline in parallel."""
    try:
        client = OpenAI(
            api_key=os.environ.get("DEEPSEEK_API_KEY"),
            base_url="https://api.deepseek.com/v1"
        )
    except TypeError:
        print("ERROR: DEEPSEEK_API_KEY environment variable not set. Exiting.")
        exit()

    prompt_templates = load_prompts(PROMPT_DIR, PROMPT_FILENAMES)

    print("\n--- Loading and Preparing Synthetic Data ---")
    try:
        synth_qual_data = pd.read_csv(QUAL_DATA_PATH)
        synth_resps_train = pd.read_csv(TRAIN_RESP_PATH)
        synth_resps_test = pd.read_csv(TEST_RESP_PATH)
    except FileNotFoundError as e:
        print(f"FATAL: Input data file not found: {e}. Exiting.")
        exit()

    synth_resps = pd.concat([synth_resps_train, synth_resps_test], ignore_index=True)
    all_data = pd.merge(synth_resps, synth_qual_data, on='ID')
    if 'trans_text' in all_data.columns:
        all_data.rename(columns={'trans_text': 'E'}, inplace=True)
    print(f"Successfully loaded and merged a total of {len(all_data)} data rows.")

    processed_ids = set()
    results_map = {}
    if os.path.exists(OUTPUT_CSV_PATH):
        print(f"\n--- Resuming from existing output file: {OUTPUT_CSV_PATH} ---")
        existing_df = pd.read_csv(OUTPUT_CSV_PATH)
        processed_ids = set(existing_df['ID'])
        results_map = {row['ID']: row for row in existing_df.to_dict('records')}
        print(f"Found {len(processed_ids)} previously processed IDs.")

    unprocessed_data = all_data[~all_data['ID'].isin(processed_ids)].copy()
    if len(unprocessed_data) == 0:
        print("\nAll IDs have already been processed. Nothing to do. Exiting.")
        return

    print(f"\n--- Preparing to score {len(unprocessed_data)} new rows with up to {MAX_WORKERS} parallel workers ---")

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

    # NOTE: New dictionaries to track row-by-row progress
    row_task_counts = defaultdict(int)
    row_completion_counts = defaultdict(int)
    completed_rows_count = 0

    tasks = []
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        for index, row in unprocessed_data.iterrows():
            student_id = row['ID']
            if student_id not in results_map:
                results_map[student_id] = {'ID': student_id}

            for col_prefix, prompt_full_text in gpt_query.items():
                if col_prefix in row and pd.notna(row[col_prefix]):
                    for prompt_key, template in prompt_templates.items():
                        row_task_counts[student_id] += 1  # Count tasks per ID
                        prompt_data = {
                            'id': student_id, 'col_prefix': col_prefix,
                            'writing_type': 'an essay' if col_prefix == 'E' else 'a sentence completion',
                            'writing_prompt': prompt_full_text[0],
                            'writing_prompt_short': prompt_full_text[0].split('"')[1],
                            'humans_response': row[col_prefix], 'country_of_origin': COUNTRY_OF_ORIGIN
                        }
                        future = executor.submit(get_llm_score, client, template, prompt_data)
                        tasks.append((future, student_id, col_prefix, prompt_key))

        print(f"\n--- Executing {len(tasks)} total API calls in parallel. This may take a while... ---")

        for future, student_id, col_prefix, prompt_key in as_completed(tasks):
            score = future.result()
            column_name = f"{col_prefix}{prompt_key}"
            results_map[student_id][column_name] = score

            # NOTE: New logic to provide per-row feedback
            row_completion_counts[student_id] += 1
            if row_completion_counts[student_id] == row_task_counts[student_id]:
                completed_rows_count += 1
                print(
                    f"--- ✅ Row complete for ID: {student_id} ({completed_rows_count}/{len(unprocessed_data)} new rows complete) ---")

                if completed_rows_count > 0 and completed_rows_count % SAVE_INTERVAL == 0:
                    print(f"\n--- Saving progress ({len(results_map)} total rows)... ---")
                    temp_df = pd.DataFrame(list(results_map.values()))
                    temp_df.to_csv(OUTPUT_CSV_PATH, index=False)
                    print(f"Successfully saved to {OUTPUT_CSV_PATH}")

    # Final save at the end
    print("\n--- All parallel tasks complete. Performing final save... ---")
    final_df = pd.DataFrame(list(results_map.values()))
    cols_to_drop = [col for col in final_df.columns if col in all_data.columns and col != 'ID']
    all_data_base = all_data.drop(columns=cols_to_drop, errors='ignore')

    output_df = pd.merge(all_data_base, final_df, on='ID', how='left')
    output_df.to_csv(OUTPUT_CSV_PATH, index=False)
    print(f"\n✅ Success! All results saved to:\n{OUTPUT_CSV_PATH}")


if __name__ == "__main__":
    main()