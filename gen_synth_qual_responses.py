from openai import OpenAI  # , openai, time
import numpy as np
import pandas as pd
import os
import json
import sys
import pandas as pd
import random
import regex as re

OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()


# def fun.s

def sample_rows_qual(df, ran=123):
  resps_train_sample = df.sample(n=3, random_state=ran)
  sc_cols = [f"SC{i}" for i in range(1, 13)]
  all_qual_cols = ['E'] + sc_cols
  resps_train_sample[all_qual_cols]
  return resps_train_sample


def weighted_state_choice():
  return random.choices(list(_states.keys()), weights=list(_states.values()))[0]


class Respondent:
   def __init__(self, real_qual) -> None:

      age_mean = real_qual['age'].mean()
      age_std = real_qual['age'].std()
      age_min = real_qual['age'].min()
      age_max = real_qual['age'].max()

      gender_counts = real_qual['gender'].value_counts(normalize=True).to_dict()  # {1: "female", 2: "male"}

      self.age = max(age_min, min(age_max, round(np.random.normal(age_mean, age_std))))
      self.gender = random.choices(["female", "male"], weights=[gender_counts[1], gender_counts[2]])[0]
      self.location = weighted_state_choice()
      self.hobby = random.choice(_hobbies)
      self.degree = random.choice(_degrees)


def D_level(n):
  """ Depression level descriptions from percentile bins (0-99)"""
  n = pd.to_numeric(n)
  if n <= 1:
    return "an extremely low level of depression"
  elif 2 <= n <= 5:
    return "a very low level of depression"
  elif 6 <= n <= 14:
    return "a low level of depression"
  elif 15 <= n <= 29:
    return "a moderately low level of depression"
  elif 30 <= n <= 39:
    return "a somewhat low level of depression"
  elif 40 <= n <= 59:
    return "a balanced (neither low nor high) level of depression"
  elif 60 <= n <= 69:
    return "a somewhat high level of depression"
  elif 70 <= n <= 84:
    return "a moderately high level of depression"
  elif 85 <= n <= 93:
    return "a high level of depression"
  elif 94 <= n <= 97:
    return "a very high level of depression"
  elif n >= 98:
    return "an extremely high level of depression"


def get_completion(messages, model="gpt-4o-mini", max_tokens=256, temperature=1, stop=None, seed=123, tools=None, logprobs=False):
    """use gpt-4o-mini to generate completions"""

    params = {
        "model": model,
        "messages": messages,
        "max_tokens": max_tokens,
        "temperature": temperature,  # temp=2 gives v strange output, inc a good deal of non-English text.
        "stop": stop,
        "seed": seed,
        "logprobs": logprobs
    }
    if tools:
        params["tools"] = tools

    return client.chat.completions.create(**params)


def generate_answer_length(q):

    length = max(q['min_words'], min(q['max_words'], round(np.random.normal(q['mean_words'], q['std_words']))))

    # Apply rounding rules
    if length < 10:
        return length  # Keep as is
    elif length < 50:
        return round(length / 5) * 5  # Round to nearest 5
    else:
        return round(length / 10) * 10  # Round to nearest 10


def make_open_qs_dict(open_qs, real_qual):
    '''loop through each key in open_qs'''

    # Lists to store computed values
    mean_words_list = []
    std_words_list = []
    max_words_list = []
    min_words_list = []

    for key in open_qs['key']:
        # Compute number of words in each response
        word_counts = real_qual[key].dropna().astype(str).apply(lambda x: len(x.split()))

        # Calculate mean & standard deviation of word counts
        mean_words = word_counts.mean()
        std_words = word_counts.std()
        max_words = word_counts.max()
        min_words = word_counts.min()

        # Append to lists
        mean_words_list.append(mean_words)
        std_words_list.append(std_words)
        max_words_list.append(max_words)
        min_words_list.append(min_words)

    open_qs['mean_words'] = mean_words_list
    open_qs['std_words'] = std_words_list
    open_qs['max_words'] = max_words_list
    open_qs['min_words'] = min_words_list

    open_qs['q_type'] = np.where(open_qs['key'] == 'E', 'essay title', 'sentence completion task')
    open_qs_dict = open_qs.set_index('key')[
        ['q_type', 'q_text', 'mean_words', 'std_words', 'min_words', 'max_words']].to_dict(orient='index')

    return (open_qs_dict)


def generate(q_name, percentile: int, exts, open_qs_dict, R) -> tuple[str, Respondent]:
    '''
    Returns tuple of `(response, Respondent)`
    '''

    q=open_qs_dict[q_name]
    essays = exts[q_name]
    essays.reset_index(drop=True, inplace=True)

    answer_len = generate_answer_length(q)
    n_words = '1 word' if answer_len == 1 else str(answer_len) + ' words'

    E_a = essays[0]
    E_b = essays[1]
    E_c = essays[2]

    with open("prompts/collect_synth_essays_demogs_d2.txt", "r", encoding="utf-8") as f:
        pretend_demogs = f.read().format(
          gender=R.gender,
          age=R.age,
          degree=R.degree,
          hobby=R.hobby,
          percentile=int(percentile),
          D_level_desc=D_level(percentile),
          location=R.location,
          # position=position,
          # level=level
        )

    messages = [
        {"role": "system", "content": "You are an AI that pretends to be a US college student, writing responses in the style of examples."},

        {"role": "user", "content": f"Example response, responding to the {q['q_type']}: {q['q_text']}."},
        {"role": "assistant", "content": f"{E_a}"},

        {"role": "user", "content": f"Another example response, responding to the {q['q_type']}: {q['q_text']}."},
        {"role": "assistant", "content": f"{E_b}"},

        {"role": "user", "content": f"Yet another example essay, responding to the {q['q_type']}: {q['q_text']}."},
        {"role": "assistant", "content": f"{E_c}"},

        {"role": "user", "content": f"Now, pretend that you are {pretend_demogs}.\n Write a response to the {q['q_type']}: {q['q_text']}. "
                                    f"The essay should be {n_words} long, and match the style of the examples."}
    ]

    #print(f"Now, pretend that you are {pretend_demogs}")  # to check pretend_demogs structure

    response = get_completion(messages=messages, max_tokens=((answer_len*2)+1))

    return response.choices[0].message.content


# use fun.s

_hobbies: list[str] = []
_degrees: list[str] = []
_states: dict[str, float] = {}

with open("input/hobbies.txt", "r", encoding="utf-8") as f:
  for line in f.read().splitlines():
    _hobbies.append(line.strip())

with open("input/degrees.txt", "r", encoding="utf-8") as f:
  for line in f.read().splitlines():
    _degrees.append(line.strip())

with open("input/states.json", "r", encoding="utf-8") as f:
  _states = json.load(f)
  for state in _states:
    _states[state] = _states[state] / 100  # percentage to decimal

# read in the data, resps_train, open q names
synth_resps_train = pd.read_csv('output/synth_resps_train_d2UPDATED.csv')
synth_resps_test = pd.read_csv('output/synth_resps_test_d2UPDATED.csv')
real_qual = pd.read_csv('../cdftlm_train_pur_d2UPDATED.csv')
real_qual.loc[real_qual['age'] == 175, 'age'] = 17  # fix age error
real_qual = real_qual.rename(columns={'trans_text': 'E'})
srts = sample_rows_qual(real_qual)  # sample real_qual
srts.to_csv('../sampled_example_rows.csv', index=False)

open_qs = pd.read_csv('../translated_sc_names_d2.csv')
open_qs.columns = ['key', 'q_text']

# stack synth_resps_train and synth_resps_test
synth_resps = pd.concat([synth_resps_train, synth_resps_test])
synth_resps.reset_index(drop=True, inplace=True)

# create percentile rank (0th to 99th percentile)
synth_resps['true_theta_percentile'] = np.floor(synth_resps['true_theta'].rank(pct=True) * 100).astype(int)
synth_resps['true_theta_percentile'] = synth_resps['true_theta_percentile'].clip(0, 99)  # Ensure max is 99

# # For all qs for all respondents
#synth_resps = synth_resps.head(4)  # hash out unless trialling

# make open qs dict
open_qs_dict = make_open_qs_dict(open_qs, real_qual)

# File path for saving
output_file = "output/synth_qual_resps_d2.csv"
#output_file = "output/synth_qual_resps_d2TRIAL.csv"
#output_file = "output/synth_qual_resps_d2TRIAL2.csv"
#output_file = "output/synth_qual_resps_d2TRIAL3.csv"

# Initialize a list to store responses
generated_responses = []

# Check if file already exists (resume functionality)
if os.path.exists(output_file):
    existing_df = pd.read_csv(output_file)
    completed_ids = set(existing_df['ID'])
    header_written = True
else:
    existing_df = None
    completed_ids = set()
    header_written = False

# Loop through each respondent
for _, row in synth_resps.iterrows():
    respondent_id = row['ID']  # Assuming 'ID' column exists

    # Skip already completed respondents
    if respondent_id in completed_ids:
        continue

    percentile = row['true_theta_percentile']
    R = Respondent(real_qual=real_qual)

    # Dictionary to store responses for this respondent
    response_dict = {'ID': int(respondent_id)}

    # Add demographic characteristics (used in prompt vignette)
    response_dict.update({
        'tru_th_percentile': int(percentile),
        'age': R.age,
        'gender': R.gender,
        'location': R.location,
        'hobby': R.hobby,
        'poss_future_degree': R.degree
    })

    # Generate responses for each question
    for k in open_qs_dict.keys():
        response = generate(q_name=k, percentile=percentile, exts=srts, open_qs_dict=open_qs_dict, R=R)
        response_dict[k] = response  # Store response in the dictionary

    # Append this respondent's responses to the list
    generated_responses.append(response_dict)

    # Save progress every 10 iterations
    if len(generated_responses) % 10 == 0:
        temp_df = pd.DataFrame(generated_responses)
        temp_df.to_csv(output_file, mode='a', header=not header_written, index=False)
        # After first save, set header_written to True
        header_written = True
        print(f"Saved another {len(generated_responses)} responses")
        generated_responses = []  # Clear list after saving to free memory

# Final save after loop
if generated_responses:
    temp_df = pd.DataFrame(generated_responses)
    temp_df.to_csv(output_file, mode='a', header=(existing_df is None), index=False)

