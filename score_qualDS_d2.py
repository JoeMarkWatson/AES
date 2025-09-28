import os
import time
import pandas as pd
from openai import OpenAI

print(os.getcwd())

# --- CONFIGURATION ---
INPUT_CSV_PATH = '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv'
PROMPT_DIR = 'prompts/scoring_prompts/'
OUTPUT_CSV_PATH = '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/DS_sentsOutput_4prompts_incremental_d2.csv'
MODEL_NAME = "deepseek-chat"
SAVE_INTERVAL = 2  # Save after every 2 rows

PROMPT_FILENAMES = {
    'a': 'score_qual_a_d2.txt',
    'b': 'score_qual_b_d2.txt',
    'c': 'score_qual_c_d2.txt',
    'd': 'score_qual_d_d2.txt'
}


# --- CORE FUNCTIONS ---

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
        print(f"  - WARNING: Could not convert LLM response ('{content}') to int. Storing None.")
        return None
    except Exception as e:
        print(f"  - ERROR: API call failed: {e}. Storing None.")
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


def process_row(row, gpt_query, prompt_templates, client):
    """Processes a single row of the DataFrame and returns a dictionary of scores."""
    student_id = row["ID"]
    row_scores = {'ID': student_id}

    for col_prefix, prompt_full_text in gpt_query.items():
        if col_prefix not in row or pd.isna(row[col_prefix]):
            print(f"  - Skipping {col_prefix} (empty data)")
            continue

        print(f"  - Scoring {col_prefix}...")
        prompt_data = {
            'writing_type': 'an essay' if col_prefix == 'E' else 'a sentence completion',
            'writing_prompt': prompt_full_text[0],
            'writing_prompt_short': prompt_full_text[0].split('"')[1],
            'humans_response': row[col_prefix]
        }

        for prompt_key, template in prompt_templates.items():
            score = get_llm_score(client, template, prompt_data)
            column_name = f"{col_prefix}{prompt_key}"
            row_scores[column_name] = score

    return row_scores


# --- MAIN EXECUTION BLOCK ---

def main():
    """Main function to run the data scoring pipeline."""
    # Set up DeepSeek client
    try:
        client = OpenAI(
            api_key=os.environ.get("DEEPSEEK_API_KEY"),
            base_url="https://api.deepseek.com/v1"
        )
    except TypeError:
        print("ERROR: DEEPSEEK_API_KEY environment variable not set. Exiting.")
        exit()

    # Load prompts and data
    prompt_templates = load_prompts(PROMPT_DIR, PROMPT_FILENAMES)

    try:
        real_data = pd.read_csv(INPUT_CSV_PATH)
        real_data.rename(columns={'trans_text': 'E'}, inplace=True)
    except FileNotFoundError:
        print(f"FATAL: Input data file not found at {INPUT_CSV_PATH}. Exiting.")
        exit()

    # NOTE: Resume capability logic
    processed_ids = set()
    results_list = []
    if os.path.exists(OUTPUT_CSV_PATH):
        print(f"\n--- Resuming from existing output file: {OUTPUT_CSV_PATH} ---")
        existing_df = pd.read_csv(OUTPUT_CSV_PATH)
        processed_ids = set(existing_df['ID'])
        results_list = existing_df.to_dict('records')
        print(f"Found {len(processed_ids)} previously processed IDs.")

    # Filter out already processed rows
    unprocessed_data = real_data[~real_data['ID'].isin(processed_ids)]
    if len(unprocessed_data) == 0:
        print("\nAll IDs have already been processed. Nothing to do. Exiting.")
        return

    print(f"\n--- Starting scoring for {len(unprocessed_data)} new rows ---")

    # Define the mapping from data columns to prompt descriptions
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

    # Main Processing Loop
    for index, row in unprocessed_data.iterrows():
        print(f"\nProcessing ID: {row['ID']} ({len(results_list) + 1}/{len(real_data)})")

        row_scores = process_row(row, gpt_query, prompt_templates, client)
        results_list.append(row_scores)

        # NOTE: Incremental saving logic
        # We check the number of *newly processed* rows to trigger saving
        newly_processed_count = len(results_list) - len(processed_ids)
        if newly_processed_count > 0 and newly_processed_count % SAVE_INTERVAL == 0:
            print(f"\n--- Saving progress ({len(results_list)} total rows)... ---")
            pd.DataFrame(results_list).to_csv(OUTPUT_CSV_PATH, index=False)
            print(f"Successfully saved to {OUTPUT_CSV_PATH}")

    # Final save at the end of the script
    print("\n--- Scoring complete. Performing final save... ---")
    pd.DataFrame(results_list).to_csv(OUTPUT_CSV_PATH, index=False)
    print(f"\n✅ Success! All results saved to:\n{OUTPUT_CSV_PATH}")


if __name__ == "__main__":
    main()