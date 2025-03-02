# RESTART WITH THIS SCRIPT. SEE TODO AT BOTTOM

from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re

OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()

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

    Provide a 1 to 5 score for the following statement, reflecting how the essay writer compares to a typical Chinese high school student.
    Statement: {statement}.

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
    
    You will later be asked to choose a score, representing the extent to which the following statement is evidenced in the student's response.
    Statement: {statement}.
    
    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.
    
    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word, the boolean True or False. You must output the word 'True', or the word 'False', nothing else."""

SUFFINFO_PROMPT_COMPARE = """You retrieved {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    The student's response is as follows: ({writing_prompt_short}) "{humans_response}". 

    You will later be asked to choose a score, representing how the student compares to a typical Chinese high school student on the following statement.
    Statement: {statement}.

    You will later be asked to choose a score, representing the extent to which the following statement is evidenced in the student's response.

    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word: either 'True' or 'False'. You must output the word 'True', or the word 'False', nothing else."""


# Load data
synth_data = True

if synth_data:
    synth_qual_data = pd.read_csv("output/synth_qual_resps_d2.csv")
    synth_resps_train = pd.read_csv('output/synth_resps_train_d2UPDATED.csv')
    synth_resps_test = pd.read_csv('output/synth_resps_test_d2UPDATED.csv')
    synth_resps = pd.concat([synth_resps_train, synth_resps_test])
    synth_resps.reset_index(drop=True, inplace=True)
    real_data = synth_resps.merge(synth_qual_data, on='ID')
else:  # if real data
    real_data = pd.read_csv('"/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv"')
    real_data.rename(columns={'trans_text': 'E'}, inplace=True)

# hash out unless trialling
real_data = real_data.head(2)  # hash out unless trialling
# hash out unless trialling

print(f"Loaded {len(real_data)} rows of data.")

open_qs = pd.read_csv('../translated_sc_names_d2.csv')
sentence_prefix = 'the sentence completion prompt: '
essay_prefix = 'the essay title: '
open_qs['prefix_n_q'] = np.where(open_qs['qual_source'] == 'E', essay_prefix + '"' + open_qs['translation'] + '." ',
                                 sentence_prefix + '"' + open_qs['translation'] + '" ')

consistent_statement = "the student has depression or a low mood."
varying_statements = ['The student has anxiety', 'The student has low motivation',
        'The student conceals thoughts or experiences from their parents', 'The student experiences some sadness or emotional difficulty',
        'The student has a sense of hopelessness', 'The student possesses a lowered sense of self-worth and/or has a need for validation',
        'The student feels supported by, or connected with, their family', 'The student experiences insomnia or engage in overthinking at night',
        'The student feels optimism for the future', 'The student feels obligation or pressure',
        'The student experiences physical symptoms associated with depression', 'The student engages in harmful ideation or has self-destructive thoughts',
        'The student has a sense of future purpose']

GPT_query = {key: (pq, vs) for key, pq, vs in zip(open_qs['qual_source'], open_qs['prefix_n_q'], varying_statements)}

# Initialize results lists
data_list_evidence = []
data_list_compare = []

# Process each essay
for _, row in real_data.iterrows():
    id = row["ID"]
    print(f"Processing ID: {id}")  # Debugging print
    scoring_data_evidence = {'ID': id}
    scoring_data_compare = {'ID': id}

    for k, v in GPT_query.items():
        writing_type = 'an essay' if k == 'E' else 'a sentence'
        writing_prompt_short = v[0].split('"')[1]  # Simplified extraction

        # # API calls - Classification
        # Consistent Evidence
        logprob1_evidence = get_logprob_response(
            CLASSIFICATION_PROMPT_EVIDENCE.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short,
                statement=consistent_statement
            )
        )
        if logprob1_evidence:
            scoring_data_evidence[f"{k}T"] = int(logprob1_evidence.token)
            scoring_data_evidence[f"{k}P"] = np.round(np.exp(logprob1_evidence.logprob) * 100, 2)

        # Consistent Comparison
        logprob1_compare = get_logprob_response(
            CLASSIFICATION_PROMPT_COMPARE.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short,
                statement=consistent_statement
            )
        )
        if logprob1_compare:
            scoring_data_compare[f"{k}T"] = int(logprob1_compare.token)
            scoring_data_compare[f"{k}P"] = np.round(np.exp(logprob1_compare.logprob) * 100, 2)

        # # API calls - Info Sufficiency
        # Consistent Evidence
        logprob2 = get_logprob_response(
            SUFFINFO_PROMPT_EVIDENCE.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short,
                statement=consistent_statement
            )
        )
        if logprob2:
            scoring_data_evidence[f"{k}E"] = logprob2.token
            scoring_data_evidence[f"{k}EP"] = np.round(np.exp(logprob2.logprob) * 100, 2)

        # Consistent Comparison
        logprob2 = get_logprob_response(
            SUFFINFO_PROMPT_COMPARE.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short,
                statement=consistent_statement
            )
        )
        if logprob2:
            scoring_data_compare[f"{k}E"] = logprob2.token
            scoring_data_compare[f"{k}EP"] = np.round(np.exp(logprob2.logprob) * 100, 2)

    # Append results
    data_list_evidence.append(scoring_data_evidence)
    data_list_compare.append(scoring_data_compare)

# Convert results to DataFrame and save
results_evidence_df = pd.DataFrame(data_list_evidence)
results_compare_df = pd.DataFrame(data_list_compare)

# Save outputs
output_dir = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/"
results_evidence_df.to_csv(f"{output_dir}GPT_const_evidence_synth.csv", index=False)
results_compare_df.to_csv(f"{output_dir}GPT_const_compare_synth.csv", index=False)

print("Results saved successfully.")

# TODO: more importantly - u've just added varying_statements. Use those to add another 2 calls: API calls - Classification, and another 2 calls: API calls - Info sufficiency.
# todo: then, add some save-every-10 so that you don't have to run this all during 1 monster session.
# TODO: then, change real_data to some neutral name.
# todo: then, final trial - does the save-every-10 just about work?
# lastly, have a quick look at ur GPT answers - are they good enough for a model to work on (and to be shared as sim.d data)
# then, i guess it's just running it.
# if u really wanna, you can replace ur 4 real_data scoring scripts with a single one that looks like this (or even, edit this to allow for it, although i don't think). But, do this after you've got output and have some manuscript write up going on.
