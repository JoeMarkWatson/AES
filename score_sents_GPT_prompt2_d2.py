from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re

OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()


# # Def. fun.s

def get_completion(messages, model="gpt-4o-mini", max_tokens=500, temperature=0, stop=None, seed=123, tools=None, logprobs=True, top_logprobs=1):
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


# # Def. prompts
CLASSIFICATION_PROMPT = """You will be given {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    
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


EVIDENCE_PROMPT = """You retrieved {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    The student's response is as follows: ({writing_prompt_short}) "{humans_response}".

    You will later be asked to choose a score, representing how the essay writer compares to a typical Chinese high school student on the following statement: {statement}

    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.

    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word, the boolean True or False. You must output the word 'True', or the word 'False', nothing else."""


# Load data
real_data = pd.read_csv(
    '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv')
real_data.rename(columns={'trans_text': 'E'}, inplace=True)
#real_data = real_data.head(2)  # to permit trialling
print(f"Loaded {len(real_data)} rows of data.")

# Def GPT query mapping
ss = 'the sentence completion prompt: "'
GPT_query = {'E': ['the essay title, "My saddest experience"', 'The student has anxiety', 'N'],
             'SC1': [ss + 'Over the past week, I...' + '"', 'The student has low motivation', 'N'],
             'SC2': [ss + 'My parents don’t know that I...' + '"', 'The student has conceals thoughts or experiences from their parents',
                     'N'],
             'SC3': [ss + 'On most days, my mood...' + '"', 'The student experiences some sadness or emotional difficulty', 'N'],
             'SC4': [ss + 'When I stand by the window, I...' + '"', 'The student has a sense of hopelessness', 'N'],
             'SC5': [ss + 'I actually...' + '"',
                     'The student possesses a lowered sense of self-worth and/or has a need for validation', 'N'],
             'SC6': [ss + 'My family...' + '"', 'The student feels supported by, or connected with, their family', 'R'],
             'SC7': [ss + 'At night, I often...' + '"', 'The student experiences insomnia or engage in overthinking at night', 'N'],
             'SC8': [ss + 'Recently, I plan to...' + '"', 'The student feels optimism for the future', 'R'],
             'SC9': [ss + 'I should...' + '"', 'The student feels obligation or pressure', 'N'],
             'SC10': [ss + 'Recently, my body...' + '"', 'The student experiences physical symptoms associated with depression',
                      'N'],
             'SC11': [ss + 'The knife on the table can...' + '"',
                      'The student engages in harmful ideation or has self-destructive thoughts', 'N'],
             'SC12': [ss + 'Next week, I plan to...' + '"', 'The student has a sense of future purpose', 'R'],
             }


# Initialize a list to collect data for the DataFrame
data_list = []

# # apply fun.s

# Process each essay and store results
for _, row in real_data.iterrows():
    id = row["ID"]
    print(f"Processing ID: {id}")  # temp
    scoring_data = {'ID': id}  # Store only the essay ID  # RESTART HERE.
    real_data_id = real_data[real_data['ID'] == id]

    for k, v in GPT_query.items():
        writing_type = 'an essay' if 'E' in k else 'a sentence'
        writing_prompt_short = re.split('"', v[0])[1]
        human_prompt = 'the essay title, "My saddest experience"' if 'E' in k else 'a sentence completion prompt'
        is_reverse = v[2] == "R"

        # API call to get completion with log probabilities
        API_RESPONSE = get_completion(
            [{"role": "user", "content": CLASSIFICATION_PROMPT.format(writing_type=writing_type,
                                                                      writing_prompt=v[0],
                                                                      statement=v[1],
                                                                      humans_response=row[k],
                                                                      writing_prompt_short=writing_prompt_short)
              }]
        )

        top_logprobs = API_RESPONSE.choices[0].logprobs.content[0].top_logprobs
        logprob = top_logprobs[0]

        # Store tokens and probabilities in the dictionary
        # If the statement is reverse ('R'), we subtract the token value from 6, otherwise, we store it as-is
        scoring_data[f"{k}T"] = (6 - int(logprob.token)) if v[2] == 'R' else int(logprob.token)
        scoring_data[f"{k}P"] = np.round(np.exp(logprob.logprob) * 100, 2)

        # API call to get completion with log probabilities
        API_RESPONSE2 = get_completion(
            [{"role": "user", "content": EVIDENCE_PROMPT.format(writing_type=writing_type,
                                                                writing_prompt=v[0],
                                                                statement=v[1],
                                                                humans_response=row[k],
                                                                writing_prompt_short=writing_prompt_short)
              }]
        )

        top_logprobs2 = API_RESPONSE2.choices[0].logprobs.content[0].top_logprobs
        logprob2 = top_logprobs2[0]

        # Store tokens and probabilities in the dictionary
        scoring_data[f"{k}E"] = logprob2.token
        scoring_data[f"{k}EP"] = np.round(np.exp(logprob2.logprob) * 100, 2)

    # Append the essay data (for one essay) to the data list
    data_list.append(scoring_data)

# Convert the data list into a pandas DataFrame
results_df = pd.DataFrame(data_list)
print(results_df.head())
results_df.to_csv(
    '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/GPT_sentsOutput2_24122024.csv',
    index=False)

print("Results saved successfully.")
