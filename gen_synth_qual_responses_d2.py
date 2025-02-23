from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re


OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()


# # Input notes
 # True theta for all (contained in synth_resps_train_d2.csv and synth_resps_test_d2.csv)
 # Real qual (of which u sample n, contained in synth_resps_train_d2)
# # Output notes
 # Desired output: 3000 x 13 bits of qual, for (1) essay task and (12) sentence completion tasks


# # def funs

def get_completion(messages, model="gpt-4o-mini", max_tokens=500, temperature=0, stop=None, seed=123, tools=None, logprobs=True, top_logprobs=1):
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


