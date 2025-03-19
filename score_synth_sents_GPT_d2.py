from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re

OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()

# to note. this script does not reverse-code rel items (like in score_essays_GPT_d2.py). Instead, this is does in est_LLM_single_sents_d2SYNTH.R and test_script_d2SYNTH.R


# Define funs
def get_logprob_response(prompt):
    """Calls GPT API and extracts the top logprob response."""
    response = get_completion([{"role": "user", "content": prompt}])
    logprobs = response.choices[0].logprobs.content[0].top_logprobs
    return logprobs[0] if logprobs else None


def get_completion(messages, model="gpt-4o-mini", max_tokens=500, temperature=0, stop=None, seed=123, tools=None,
                   logprobs=True, top_logprobs=1):
    """Handles API calls to OpenAI with specified parameters."""
    params = {
        "model": model,
        "messages": messages,
        "max_tokens": max_tokens,
        "temperature": temperature,
        "stop": stop,
        "seed": seed,
        "logprobs": logprobs,
        "top_logprobs": top_logprobs,
    }
    if tools:
        params["tools"] = tools
    return client.chat.completions.create(**params)


def load_or_create_df(filename):
    if os.path.exists(filename):
        return pd.read_csv(filename)
    else:
        return pd.DataFrame()


# Define prompts
CLASSIFICATION_PROMPT_EVIDENCE = """You will be given {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.

    Provide a 1 to 5 score representing the extent to which the student's written response evidences that {statement}.

    Use the following scale:
    - 1 = zero or very little evidence
    - 2 = little evidence
    - 3 = moderate evidence
    - 4 = strong evidence
    - 5 = very strong or overwhelming evidence

    Return only a single integer value between 1 and 5, and nothing else.

    Student response: ({writing_prompt_short}) "{humans_response}"."""

CLASSIFICATION_PROMPT_COMPARE = """You will be given {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.

    Provide a 1 to 5 score for the following statement, reflecting how the essay writer compares to a typical Chinese high school student. Statement: {statement}.

    Use the following scale:
    - 1 = much less than a typical student
    - 2 = slightly less than a typical student
    - 3 = about the same as a typical student
    - 4 = slightly more than a typical student
    - 5 = much more than a typical student

    Return only a single integer value between 1 and 5, and nothing else.

    Student response: ({writing_prompt_short}) "{humans_response}"."""

SUFFINFO_PROMPT_EVIDENCE = """You retrieved {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    The student's response is as follows: ({writing_prompt_short}) "{humans_response}".

    You will later be asked to choose a score, representing the extent to which the following statement is evidenced in the student's response. Statement: {statement}.

    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.

    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word, the boolean True or False. You must output the word 'True', or the word 'False', nothing else."""

SUFFINFO_PROMPT_COMPARE = """You retrieved {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    The student's response is as follows: ({writing_prompt_short}) "{humans_response}".

    You will later be asked to choose a score, representing how the student compares to a typical Chinese high school student on the following statement. Statement: {statement}.

    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.

    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word: either 'True' or 'False'. You must output the word 'True', or the word 'False', nothing else."""

# Load data
save_every = 5  # Save every n rows
synth_data = True
output_dir = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/"

if synth_data:
    synth_qual_data = pd.read_csv("output/synth_qual_resps_d2.csv")
    synth_resps_train = pd.read_csv('output/synth_resps_train_d2UPDATED.csv')
    synth_resps_test = pd.read_csv('output/synth_resps_test_d2UPDATED.csv')
    synth_resps = pd.concat([synth_resps_train, synth_resps_test])
    synth_resps.reset_index(drop=True, inplace=True)
    all_data = synth_resps.merge(synth_qual_data, on='ID')
else:
    all_data = pd.read_csv(
        '"/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv"')
    all_data.rename(columns={'trans_text': 'E'}, inplace=True)

#all_data = all_data.head(15)  # unhash if trialling
print(f"Scoring {len(all_data)} rows of data.")

open_qs = pd.read_csv('../translated_sc_names_d2.csv')
sentence_prefix = 'the sentence completion prompt: '
essay_prefix = 'the essay title: '
open_qs['prefix_n_q'] = np.where(open_qs['qual_source'] == 'E', essay_prefix + '"' + open_qs['translation'] + '." ',
                                 sentence_prefix + '"' + open_qs['translation'] + '" ')

consistent_statement = "the student has depression or a low mood."
varying_statements = [
    'The student has anxiety', 'The student has low motivation',
    'The student conceals thoughts or experiences from their parents',
    'The student experiences some sadness or emotional difficulty',
    'The student has a sense of hopelessness',
    'The student possesses a lowered sense of self-worth and/or has a need for validation',
    'The student feels supported by, or connected with, their family',
    'The student experiences insomnia or engage in overthinking at night',
    'The student feels optimism for the future', 'The student feels obligation or pressure',
    'The student experiences physical symptoms associated with depression',
    'The student engages in harmful ideation or has self-destructive thoughts',
    'The student has a sense of future purpose'
]

GPT_query = {key: (pq, vs) for key, pq, vs in zip(open_qs['qual_source'], open_qs['prefix_n_q'], varying_statements)}

# Initialize results lists
data_list_const_evidence = []
data_list_const_compare = []
data_list_vary_evidence = []
data_list_vary_compare = []

# Check if the saved CSV files exist
const_evidence_df = load_or_create_df(f"{output_dir}GPT_const_evidence_synth.csv")
const_compare_df = load_or_create_df(f"{output_dir}GPT_const_compare_synth.csv")
vary_evidence_df = load_or_create_df(f"{output_dir}GPT_vary_evidence_synth.csv")
vary_compare_df = load_or_create_df(f"{output_dir}GPT_vary_compare_synth.csv")

# Get the list of already processed IDs
processed_ids = set(const_evidence_df['ID']) if not const_evidence_df.empty else set()
print(f"Ultimately processing all rows. Restarting after having already processed {len(processed_ids)} rows")

# Process each essay
for i, row in all_data.iterrows():
    if row["ID"] in processed_ids:
        continue  # Skip already processed rows

    id = row["ID"]
    print(f"Processing ID: {id}")  # Debugging print

    scoring_const_evidence = {'ID': id}
    scoring_const_compare = {'ID': id}
    scoring_vary_evidence = {'ID': id}
    scoring_vary_compare = {'ID': id}

    for k, v in GPT_query.items():
        writing_type = 'an essay' if k == 'E' else 'a sentence'
        writing_prompt_short = v[0].split('"')[1]  # Extract short prompt

        for prompt_type, statement, scoring_dict in [
            (CLASSIFICATION_PROMPT_EVIDENCE, consistent_statement, scoring_const_evidence),
            (CLASSIFICATION_PROMPT_COMPARE, consistent_statement, scoring_const_compare),
            (CLASSIFICATION_PROMPT_EVIDENCE, v[1], scoring_vary_evidence),
            (CLASSIFICATION_PROMPT_COMPARE, v[1], scoring_vary_compare),
        ]:
            logprob = get_logprob_response(
                prompt_type.format(
                    writing_type=writing_type, writing_prompt=v[0],
                    humans_response=row[k], writing_prompt_short=writing_prompt_short,
                    statement=statement
                )
            )
            if logprob:
                scoring_dict[f"{k}T"] = int(logprob.token)
                scoring_dict[f"{k}P"] = np.round(np.exp(logprob.logprob) * 100, 2)

        for prompt_type, statement, scoring_dict in [
            (SUFFINFO_PROMPT_EVIDENCE, consistent_statement, scoring_const_evidence),
            (SUFFINFO_PROMPT_COMPARE, consistent_statement, scoring_const_compare),
            (SUFFINFO_PROMPT_EVIDENCE, v[1], scoring_vary_evidence),
            (SUFFINFO_PROMPT_COMPARE, v[1], scoring_vary_compare),
        ]:
            logprob = get_logprob_response(
                prompt_type.format(
                    writing_type=writing_type, writing_prompt=v[0],
                    humans_response=row[k], writing_prompt_short=writing_prompt_short,
                    statement=statement
                )
            )
            if logprob:
                scoring_dict[f"{k}E"] = logprob.token
                scoring_dict[f"{k}EP"] = np.round(np.exp(logprob.logprob) * 100, 2)

    # Append new data
    data_list_const_evidence.append(scoring_const_evidence)
    data_list_const_compare.append(scoring_const_compare)
    data_list_vary_evidence.append(scoring_vary_evidence)
    data_list_vary_compare.append(scoring_vary_compare)

    # Save every 5 iterations
    if (len(data_list_const_evidence) % save_every == 0):
        const_evidence_df = pd.concat([const_evidence_df, pd.DataFrame(data_list_const_evidence)])
        const_compare_df = pd.concat([const_compare_df, pd.DataFrame(data_list_const_compare)])
        vary_evidence_df = pd.concat([vary_evidence_df, pd.DataFrame(data_list_vary_evidence)])
        vary_compare_df = pd.concat([vary_compare_df, pd.DataFrame(data_list_vary_compare)])
        print(len(const_evidence_df))  # temp

        data_list_const_evidence.clear()
        data_list_const_compare.clear()
        data_list_vary_evidence.clear()
        data_list_vary_compare.clear()

        const_evidence_df.to_csv(f"{output_dir}GPT_const_evidence_synth.csv", index=False)
        const_compare_df.to_csv(f"{output_dir}GPT_const_compare_synth.csv", index=False)
        vary_evidence_df.to_csv(f"{output_dir}GPT_vary_evidence_synth.csv", index=False)
        vary_compare_df.to_csv(f"{output_dir}GPT_vary_compare_synth.csv", index=False)
        print(f"Autosaved after processing {i + 1} rows")


# Save remaining data at the end of processing
if data_list_const_evidence:
    pd.concat([const_evidence_df, pd.DataFrame(data_list_const_evidence)]).to_csv(f"{output_dir}GPT_const_evidence_synth.csv", index=False)
    pd.concat([const_compare_df, pd.DataFrame(data_list_const_compare)]).to_csv(f"{output_dir}GPT_const_compare_synth.csv", index=False)
    pd.concat([vary_evidence_df, pd.DataFrame(data_list_vary_evidence)]).to_csv(f"{output_dir}GPT_vary_evidence_synth.csv", index=False)
    pd.concat([vary_compare_df, pd.DataFrame(data_list_vary_compare)]).to_csv(f"{output_dir}GPT_vary_compare_synth.csv", index=False)
    print("Final save completed.")

print("Processing complete.")
