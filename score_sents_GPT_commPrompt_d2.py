from openai import OpenAI
import numpy as np
import pandas as pd
import os
import regex as re

# Set API key
OpenAI.api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI()

# Define helper function for API calls
def get_logprob_response(prompt):
    response = get_completion([{"role": "user", "content": prompt}])
    logprobs = response.choices[0].logprobs.content[0].top_logprobs
    return logprobs[0] if logprobs else None

# Define completion function
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

# Define prompts
CLASSIFICATION_PROMPT = """You will be given {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.

    Provide a 1 to 5 score representing the extent to which the student's written response evidences that they have depression or a low mood.
    
    Use the following scale:
    - 1 = zero or very little evidence
    - 2 = little evidence
    - 3 = moderate evidence
    - 4 = strong evidence
    - 5 = very strong or overwhelming evidence
    
    Return only a single integer value between 1 and 5, and nothing else. 
    
    Student response: ({writing_prompt_short}) "{humans_response}"."""

EVIDENCE_PROMPT = """You retrieved {writing_type} written by a Chinese high school student, that they produced in response to {writing_prompt}.
    The student's response is as follows: ({writing_prompt_short}) "{humans_response}". 
    
    You will later be asked to choose a score, representing the extent to which the following statement is evidenced in the student's response.
    Statement: The student's written response evidences that they have depression or a low mood.
    
    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.
    
    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word, the boolean True or False. You must output the word 'True', or the word 'False', nothing else."""

# Load data
real_data = pd.read_csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv')
real_data.rename(columns={'trans_text': 'E'}, inplace=True)
print(f"Loaded {len(real_data)} rows of data.")

# Temp: Limit rows for trial
# real_data = real_data.head(2)  # To be removed after trialling

# Define GPT query mapping
ss = 'the sentence completion prompt: "'
GPT_query = {
    'E': ['the essay title: "My saddest experience"'],
    **{f'SC{i}': [ss + prompt + '"'] for i, prompt in enumerate([
        'Over the past week, I...', 'My parents don’t know that I...', 'On most days, my mood...',
        'When I stand by the window, I...', 'I actually...', 'My family...', 'At night, I often...',
        'Recently, I plan to...', 'I should...', 'Recently, my body...', 'The knife on the table can...',
        'Next week, I plan to...'
    ], start=1)}
}

# Initialize results list
data_list = []

# Process each essay
for _, row in real_data.iterrows():
    id = row["ID"]
    print(f"Processing ID: {id}")  # Debugging print
    scoring_data = {'ID': id}

    for k, v in GPT_query.items():
        writing_type = 'an essay' if k == 'E' else 'a sentence'
        writing_prompt_short = v[0].split('"')[1]  # Simplified extraction

        # First API call (classification)
        logprob1 = get_logprob_response(
            CLASSIFICATION_PROMPT.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short
            )
        )
        if logprob1:
            scoring_data[f"{k}T"] = int(logprob1.token)
            scoring_data[f"{k}P"] = np.round(np.exp(logprob1.logprob) * 100, 2)

        # Second API call (evidence sufficiency)
        logprob2 = get_logprob_response(
            EVIDENCE_PROMPT.format(
                writing_type=writing_type, writing_prompt=v[0],
                humans_response=row[k], writing_prompt_short=writing_prompt_short
            )
        )
        if logprob2:
            scoring_data[f"{k}E"] = logprob2.token
            scoring_data[f"{k}EP"] = np.round(np.exp(logprob2.logprob) * 100, 2)

    # Append result
    data_list.append(scoring_data)

# Convert results to DataFrame and save
results_df = pd.DataFrame(data_list)
print(results_df.head())
results_df.to_csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/GPT_sentsOutputEvidence_01022025.csv', index=False)
print("Results saved successfully.")
