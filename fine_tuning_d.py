# select 50 rows from train set
# fine tune using theses 50 rows
# get output for remaining 1950 in train and 1000 in test

import json
import os
import re

import numpy as np
import openai
import pandas as pd

os.chdir(__file__[:-len('fine_tuning_d.py')])
client = None

with open("../api_key/openai_key2.csv", "r") as f:
  client = openai.OpenAI(api_key=f.read().strip())


def extract_values(message: str) -> pd.DataFrame:
  split_str = re.split(r"ringent", message)
  split_strs = split_str[1]
  split_moderate = split_str[0]

  ssplit = re.split(r"enient", split_strs)
  split_stringent = ssplit[0]
  split_lenient = ssplit[1]

  sp_list = [split_moderate, split_stringent, split_lenient]

  colnames = ['GPT_Q1',
              'GPT_Q2',
              'GPT_Q3',
              'GPT_Q4',
              'GPT_Q5',
              'GPT_Q6',
              'GPT_Q7',
              'GPT_Q8',
              'GPT_Q9',
              'GPT_Q10']

  resps_df = pd.DataFrame(np.nan, columns=colnames, index=range(3))

  for k, sp in enumerate(sp_list):
    splits = re.split(r"[SQ]\\d+", sp)
    q_values = [np.nan] * 10

    for i in range(1, len(splits)):
      q = re.sub("\\D", "", splits[i])
      if q.isdigit():
        q_values[i - 1] = int(q)

    resps_df.iloc[k] = q_values

  return resps_df


def get_score_response(prompt: str, essay: str) -> str:
  attempts = 0
  max_attempts = 5
  success = False

  while (attempts < max_attempts) and (not success):
    try:
      response = client.chat.completions.create(
        model="ft:gpt-4o-mini-2024-07-18:personal::A5YmmGSK", #"gpt-4o-mini",
        messages=[
          {"role": "system", "content": prompt},
          {"role": "user", "content": f"Student essay: '{essay}'"},
        ],
        temperature=0,
      )
      message = response.choices[0].message.content
      if not message:
        raise Exception("Empty response")
      success = True
    except Exception as e:
      attempts += 1
      print(f"Attempt {attempts} failed: {e}")

  if not success:
    raise Exception("Failed to get response")

  return message


def prepare_data() -> bool:
  trainset = pd.read_csv('../cdftlm_train_pur.csv')

  samples = trainset.sample(50)
  trainset = trainset.drop(samples.index)

  # get GPT zero-shot output for the 50 samples
  with open("prompts/score_essays_prompt_d.txt", "r") as f:
    prompt = f.read()

  edict = {}
  for _, row in samples.iterrows():
    td = {}
    td["essay"] = row["essays"]
    td["score"] = get_score_response(prompt, row["essays"])
    edict[row["ID"]] = td

  # format into jsonl for fine-tuning
  with open("../fine_tuning_data_d.jsonl", "w") as f:
    for k, v in edict.items():
      msgs = [{"role": "system", "content": prompt},
              {"role": "user", "content": f"Student essay: '{v['essay']}'"},
              {"role": "assistant", "content": v['score']},
              ]
      f.write(json.dumps({"messages": msgs}) + "\n")
    f.flush()

  # save ids of the 50 samples
  with open("../fine_tuning_sample_ids_d.txt", "w") as f:
    for k in edict.keys():
      f.write(f"{k}\n")
    f.flush()

  return True


def finetune():
  if os.path.exists("../fine_tuning_data_d.jsonl"):
    print("Fine-tuning data already exists. Skipping data preparation.")
  else:
    print("Preparing data for fine-tuning...")
    prepare_data()
    print("Data preparation complete.")

  response = client.files.create(
    file=open("../fine_tuning_data_d.jsonl", "rb"),
    purpose="fine-tune",
  )

  file_id = response.id

  response = client.fine_tuning.jobs.create(
    training_file=file_id,
    model="gpt-4o-mini-2024-07-18",
  )

  print(f"Fine-tuning job started with id {response.id}.")

def check_ft_status(id: str):
  response = client.fine_tuning.jobs.retrieve(id)
  print(response)


if __name__ == "__main__":
  pass
  # model name = ft:gpt-4o-mini-2024-07-18:personal::A5YmmGSK

  # joe note: it is possible to select a model from a day. e.g., model name = gpt-4o-mini-2024-07-18
  # doing that in conjunction with few-shot could get closer to consistency.
  # the fully fine-tuned model is gauranteed consistency over time.

  # https://platform.openai.com/docs/guides/fine-tuning/create-a-fine-tuned-model
  # https://platform.openai.com/docs/guides/fine-tuning/which-models-can-be-fine-tuned
  # https://platform.openai.com/docs/api-reference/fine-tuning/create
  # https://platform.openai.com/docs/api-reference/files/create