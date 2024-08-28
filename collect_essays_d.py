import json
import sys
import os  # , openai, time
from openai import OpenAI
import pandas as pd
import random

_hobbies: list[str] = []
_degrees: list[str] = []
_states: dict[str, float] = {}


def load_env_ivan() -> dict[str, str]:
  env: dict[str, str] = {}

  with open(".env", "r") as f:
    for line in f.readlines():
      if not line:
        continue

      while "\n" in line:
        line = line.replace("\n", "")

      name, key = line.split("=")
      key = key.removeprefix("'").removesuffix("'")
      key = key.removeprefix('"').removesuffix('"')
      env[name] = key

  return env


if "ivano" in __file__:  # if running on ivan laptop
  OpenAI.api_key = load_env_ivan()["OPENAI_API_KEY"]
else:
  OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY")
client = OpenAI()

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


def generate(temp: float, percentile: int, no_api: bool = False) -> tuple[str, Respondent]:
  '''
  Returns tuple of `(response, Respondent)`
  '''

  prompt = ""
  R = Respondent()

  with open("prompts/collect_essays_prompt_d.txt", "r", encoding="utf-8") as f:
    prompt = f.read().format(
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

  if no_api:
    print(prompt)
  else:
    response = client.chat.completions.create(
      model='gpt-4o-mini',  # gpt-3.5-turbo
      messages=[{"role": "user", "content": prompt}],
      # default is 0.7. higher for more creative, lower for more coherent/consistent
      temperature=temp,
      max_tokens=256,
      # frequency_penalty=0.0  # we can maybe try increasing this a bit
      # presence_penalty=0.0  # this one too
    )

    return (response.choices[0].message.content, R)


# print(generate(percentile=0, temp=1))  # temp=2 gives v strange output, inc a good deal of non-English text.
# print(generate(percentile=25, temp=1))
# print(generate(percentile=50, temp=1))
# print(generate(percentile=75, temp=1))
# print(generate(percentile=99, temp=1))


def collect(responses_loc):
  """Generates and saves essay responses.
   Saves bulk responses (in a single string) to `output/responses... as a .json file`."""
  data = {}

  for pc in range(0, 100):
    print(f"Starting percentile {pc}...")
    for _ in range(1, 31):  # 31
      retry = True
      retries = 0
      MAX_RETRIES = 3

      while retry:
        try:
          retry = False  # by default, don't retry

          res, R = generate(percentile=pc, temp=0.7)
          pair = {str(R): res}

          if str(pc) in data:
            data[str(pc)].append(pair)
          else:
            data[str(pc)] = [pair]

          with open("output/" + responses_loc + ".json", "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)  # save after every response

          # time.sleep(20)  # rate limit from OpenAI, can be ommitted if using paid API key
        except Exception as e:
          if retries < MAX_RETRIES:
            print(
              f"\t\tError: {e}.\n\t\tRetrying... ({retries + 1}/{MAX_RETRIES})")
            retries += 1
            retry = True
            # time.sleep(20)  # rate limit from OpenAI, can be ommitted if using paid API key
          else:
            print(f"\t\tMaximum retries exceeded, exiting.")
            retry = False
            sys.exit(1)


if __name__ == "__main__":
  print('collecting all essays')
  collect(responses_loc='essay_responses_d')
  # collect(responses_d='essay_responses_test2_d')
