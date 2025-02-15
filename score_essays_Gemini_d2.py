import google.generativeai as genai
import numpy as np
import pandas as pd
import os



genai.configure(api_key=os.environ.get('GOOGLE_API_KEY'))

for model in genai.list_models():
  if '002' in model.name:
    print(model.name)


CLASSIFICATION_PROMPT = """You will be given an essay written by a Chinese high school student, entitled 'My saddest experience'.

    Provide a 1 to 5 score for the following statement, reflecting how the essay writer compares to a typical Chinese high school student.

    Statement: {statement}

    Use the following scale:
    - 1 = much less than a typical student
    - 2 = slightly less than a typical student
    - 3 = about the same as a typical student
    - 4 = slightly more than a typical student
    - 5 = much more than a typical student

    Return only a single integer value between 1 and 5, and nothing else. 

    Student essay: {essay}"""

EVIDENCE_PROMPT = """You retrieved this essay by a Chinese high school student, entitled 'My saddest experience': {essay}. 
    You will later be asked to choose a score, representing how the essay writer compares to a typical Chinese high school student on the following statement: {statement}
    Before even answering the question, consider whether the essay provides sufficient information to give a somewhat accurate response.

    Your output should JUST be the boolean True or False, for whether the essay provides sufficient information to give a somewhat accurate response.
    Respond with just one word, the boolean True or False. You must output the word 'True', or the word 'False', nothing else."""

statements = [
    ['The student has anxiety.', 'N'],
    ['The essay writer suffers from depression or a low mood.', 'N'],
    ['The student essay writer feels socially isolated or lonely.', 'N'],
    ['The essay writer has low self-esteem or a diminished self-worth.', 'N'],
    ['The essay writer feels some hope for the future or maintains a positive outlook.', 'R'],
    ['The student essay writer exhibits physical or behavioral symptoms related to mental health.', 'N'],
    ['The essay writer describes feelings of hopelessness or despair.', 'N'],
    ['The student expresses a generally calm and worry-free demeanor.', 'R'],
    ['The essay suggests the student has confidence in their abilities.', 'R'],
    ['The student shows some level of happiness and satisfaction with life.', 'R']]

real_data = pd.read_csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_d2.csv')
essays = real_data['trans_text']
essay_ids = real_data['ID']

# Initialize a list to collect data for the DataFrame
data_list = []

model_name = "models/gemini-1.5-pro-002"
model = genai.GenerativeModel(model_name, generation_config={'temperature':0.0})  # like GPT, ranges from 0 to 2

# Process each essay and store results
for essay_id, essay in zip(essay_ids, essays):
    print(essay_id)  # temp
    essay_data = {'ID': essay_id}  # Store only the essay ID

    for i, statement in enumerate(statements):
        # API call to get completion with log probabilities
        response = model.generate_content(CLASSIFICATION_PROMPT.format(essay=essay, statement=statement[0]),
                                          generation_config=genai.GenerationConfig(max_output_tokens=1))
        text_out = response.parts[0].text
        avg_logprob = response.candidates[0].avg_logprobs

        # Store tokens and probabilities in the dictionary
        token_key = f"S{i + 1}T"  # Unique token key per statement
        prob_key = f"S{i + 1}P"  # Unique probability key per statement

        # If the statement is reverse ('R'), we subtract the token value from 6, otherwise, we store it as-is
        essay_data[token_key] = (6 - int(text_out)) if statement[1] == 'R' else int(text_out)
        essay_data[prob_key] = np.round(np.exp(avg_logprob) * 100, 2)

        # API call to get completion with log probabilities
        TFresponse = model.generate_content(EVIDENCE_PROMPT.format(essay=essay, statement=statement[0]),
                                          generation_config=genai.GenerationConfig(max_output_tokens=1))

        TFtext_out = TFresponse.parts[0].text
        TFavg_logprob = TFresponse.candidates[0].avg_logprobs

        # Store tokens and probabilities in the dictionary
        evidence_key = f"S{i + 1}E"  # Evidence token key per statement
        evi_prob_key = f"S{i + 1}EP"  # Evidence token probability key per statement

        essay_data[evidence_key] = TFtext_out
        essay_data[evi_prob_key] = np.round(np.exp(TFavg_logprob) * 100, 2)

    # Append the essay data (for one essay) to the data list
    data_list.append(essay_data)

df = pd.DataFrame(data_list)
df.to_csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/GEMINImarking_output_23102024.csv', index=False)

