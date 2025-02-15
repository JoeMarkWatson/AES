from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re


# TODO: set env variable for openAI API key


OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()

# # Input notes
 # True theta for all (contained in synth_resps_train_d2.csv and synth_resps_test_d2.csv)
 # Theta hat for train (contained in synth_resps_train_d2)
 # Real qual and scores (of which u sample 5 from synth_resps_train_d2)

# # Output notes
 # 3000 x 12 bits of qual


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


# fun that pulls 5 random rows from synth_resps_train
def sample_5_rows(df, ran=0):
    synth_resps_train_sample = df.sample(n=5, random_state=ran)
    return synth_resps_train_sample


# # use funs

# read in the data, resps_train
synth_resps_train = pd.read_csv('output/synth_resps_train_d2.csv')
synth_resps_test = pd.read_csv('output/synth_resps_test_d2.csv')

srts = sample_5_rows(synth_resps_train)


# stack synth_resps_train and synth_resps_test
synth_resps = pd.concat([synth_resps_train, synth_resps_test])
synth_resps.reset_index(inplace=True)


# below is some inspiration




def gen_synth_qual(few_shot = True,
                        sampled_essays = pd.DataFrame(),
                        samples_scores = pd.DataFrame()):
    """ could maybe put sampled essays and scores together as same input argument"""

    prompt = GEN_QUAL_PROMPT

GEN_QUAL_PROMPT = """Write an essay by a Chinese high school student in response to {writing_prompt}.

    Provide a 1 to 5 score for the following statement, reflecting how the essay writer compares to a typical Chinese high school student.
    
    Statement: {statement}
    
    Use the following scale:
    - 1 = much less than a typical student
    - 2 = slightly less than a typical student
    - 3 = about the same as a typical student
    - 4 = slightly more than a typical student
    - 5 = much more than a typical student
    
    Return only a single integer value between 1 and 5, and nothing else. 
    
    Student response: ({writing_prompt_short}) "{humans_response}"."""

