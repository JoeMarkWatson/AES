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

def sample_rows_qual(df, ran=1):
  resps_train_sample = df.sample(n=3, random_state=ran)
  sc_cols = [f"SC{i}" for i in range(1, 13)]
  all_qual_cols = ['trans_text'] + sc_cols
  resps_train_sample[all_qual_cols]
  return resps_train_sample


def weighted_state_choice():
  return random.choices(list(_states.keys()), weights=list(_states.values()))[0]


class Respondent:
  def __init__(self) -> None:
    self.age = random.randint(18, 30)
    self.degree = random.choice(_degrees)
    self.gender = random.choice(["male", "female"])
    self.hobby = random.choice(_hobbies)
    # for next attempt, randomise response length, using self.answer_len here and adapting prompt accordingly
    self.answer_len = (random.randint(50, 129) // 10) * 10
    self.location = weighted_state_choice()


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


def get_completion(messages, model="gpt-4o-mini", max_tokens=256, temperature=1, stop=None, seed=123, tools=None, logprobs=False, top_logprobs=1):
    """use gpt-4o-mini to generate completions"""

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

# TODO: add here - something that turns the essay/SC task descriptors and actual q.s into a dict/list - that get input as q
# todo: in the fun below
q_dict  # WHICH UR GOING TO NEED TO READ IN - U SAVE THESE TRANSLATIONS OF THE Q.S # RESTART HERE
# RESTART HERE
# RESTART HERE

def generate(q, exs, temp: float, percentile: int) -> tuple[str, Respondent]:
    '''
    Returns tuple of `(response, Respondent)`
    '''

    q='E'
    q_dict['q']

    exs=srts
    exs

    q_type = q[0]
    q_text = q[1]
    essay1 = exs[0]
    essay2 = exs[1]
    essay3 = exs[2]

    prompt = ""
    R = Respondent()

    with open("prompts/collect_synth_essays_demogs_d2.txt", "r", encoding="utf-8") as f:
        pretend_demogs = f.read().format(
          gender=R.gender,
          age=R.age,
          degree=R.degree,
          hobby=R.hobby,
          percentile=percentile,
          D_level_desc=D_level(percentile),
          answer_len=R.answer_len,
          location=R.location,
          # position=position,
          # level=level
        )

    messages = [
        {"role": "system", "content": "You are an AI that pretends to be a US college student, writing essays in the style of examples."},

        {"role": "user", "content": "Example essay, responding to {q_type}: {q_text}."},
        {"role": "assistant", "content": "{essay1}"},

        {"role": "user", "content": "Another example essay, responding to {q_type}: {q_text}."},
        {"role": "assistant", "content": "{essay2}"},

        {"role": "user", "content": "Yet another example essay, responding to {q_type}: {q_text}."},
        {"role": "assistant", "content": "{essay3}"},

        {"role": "user", "content": f"Now, pretend that you are {pretend_demogs}.\n Write a response to {q_type}: {q_text}. "
                                    f"The essay should match the style of the examples."}
    ]

    response = get_completion(messages=messages)

    return (response.choices[0].message.content, R)


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

# read in the data, resps_train
synth_resps_train = pd.read_csv('output/synth_resps_train_d2UPDATED.csv')
synth_resps_test = pd.read_csv('output/synth_resps_test_d2UPDATED.csv')
real_qual = pd.read_csv('../cdftlm_train_pur_d2UPDATED.csv')

srts = sample_rows_qual(real_qual)
srts = srts.rename(columns={'trans_text': 'E'})

# stack synth_resps_train and synth_resps_test
synth_resps = pd.concat([synth_resps_train, synth_resps_test])
synth_resps.reset_index(inplace=True)

# create percentile rank (0th to 99th percentile)
synth_resps['true_theta_percentile'] = np.floor(synth_resps['true_theta'].rank(pct=True) * 100).astype(int)
synth_resps['true_theta_percentile'] = synth_resps['true_theta_percentile'].clip(0, 99)  # Ensure max is 99

# # unhash for trialling
generate(percentile=0, q=['sentence completion task', ''], exs=srts)

print(generate(percentile=0, temp=1))  # temp=2 gives v strange output, inc a good deal of non-English text.
# print(generate(percentile=25, temp=1))
# print(generate(percentile=50, temp=1))
# print(generate(percentile=75, temp=1))
# print(generate(percentile=99, temp=1))




# # Input notes
 # True theta for all (contained in synth_resps_train_d2.csv and synth_resps_test_d2.csv)
 # Real qual (of which u sample n, contained in synth_resps_train_d2)
# # Output notes
 # Desired output: 3000 x 13 bits of qual, for (1) essay task and (12) sentence completion tasks


# # def funs



def sample_rows_qual(df, ran=1):
    resps_train_sample = df.sample(n=3, random_state=ran)
    sc_cols = [f"SC{i}" for i in range(1, 13)]
    all_qual_cols = ['trans_text'] + sc_cols
    resps_train_sample[all_qual_cols]
    return resps_train_sample


# # use funs

# read in the data, resps_train
synth_resps_train = pd.read_csv('output/synth_resps_train_d2UPDATED.csv')
synth_resps_test = pd.read_csv('output/synth_resps_test_d2UPDATED.csv')
real_qual = pd.read_csv('../cdftlm_train_pur_d2UPDATED.csv')

srts = sample_rows_qual(real_qual)
srts = srts.rename(columns={'trans_text': 'E'})

# stack synth_resps_train and synth_resps_test
synth_resps = pd.concat([synth_resps_train, synth_resps_test])
synth_resps.reset_index(inplace=True)

# create percentile rank (0th to 99th percentile)
synth_resps['true_theta_percentile'] = np.floor(synth_resps['true_theta'].rank(pct=True) * 100).astype(int)
synth_resps['true_theta_percentile'] = synth_resps['true_theta_percentile'].clip(0, 99)  # Ensure max is 99

# read in column titles (giving essay names)
open_qs = pd.read_csv('../translated_sc_names_d2.csv')
# open_qs_used = open_qs[~open_qs['qual_source'].isin(['SC8', 'SC9', 'SC12'])]  # could poss remove rows where
# qual_source == SC8, SC9, SC12
# as they got used in no real models, but retaining for now
open_qs_dict = open_qs.set_index('qual_source')['translation'].to_dict()


def gen_synth_qual(percentile, example_resps):
    # do some random thing to determine length, hobby, loc, etc.
    # this needs to be based on past Ivan work: https://github.com/JoeMarkWatson/AES/blob/main/gen_synth_essays_d.py


def synth_qual_all():
    '''making a new df, with 13 cols each containing an open response for each ID'''
    for k, v in open_qs_dict:
        # for each row in synth_resps
            # given the synth_resps['true_theta_percentile'] value - showing what percentile (0th to 99th) the person is in for depression with higher being more depressed and lower being less depressed
            # and some examples of real student open q responses from the k column of srts
            # use gen_synth_qual to write the students open-ended response, with all the nec randomisation


