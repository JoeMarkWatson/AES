# data processing functions

import json, spacy, re, math
from collections import defaultdict  # helps simplify the code
from .nlp import split_answers
from .stats import get_value
import numpy as np
import pandas as pd

try:
    nlp = spacy.load("en_core_web_sm")
except:
    spacy.cli.download("en_core_web_sm")
    nlp = spacy.load("en_core_web_sm")


def swap_dict(d: dict) -> dict:
    return {v: k for k, v in d.items()}


def load_df() -> pd.DataFrame:
    data: dict = json.load(open("output/responses.json", "r", encoding="utf-8"))
    normal_dist_data = np.sort(np.random.normal(0, 1, 990))
    processed = defaultdict(list)
    # this is like a dict, but you can .append(x) to non-existent keys
    # which will create the key with the value [x]
    # if the key exists, it will simply append x to the value

    for pc, profiles in data.items():
        sub_percentile = 0.0
        for profile in profiles:
            answers = split_answers(list(profile.values())[0])
            answers = [remove_punct(answer) for answer in answers]

            processed["theta"].append(get_value(normal_dist_data, int(pc) + sub_percentile))
            processed["R1"].append(answers[0])
            processed["R2"].append(answers[1])
            processed["R3"].append(answers[2])

            sub_percentile += 0.1  # for some variation of the theta value

    return pd.DataFrame(processed)


def load_df_new_convert() -> pd.DataFrame:
    data: dict = json.load(open("output/responses.json", "r", encoding="utf-8"))
    responses = pd.read_csv('output/closed_responses.csv')
    normal_dist_data = responses['true_theta']
    processed = defaultdict(list)

    for pc, profiles in data.items():
        for profile in profiles:
            answers = split_answers(list(profile.values())[0])
            answers = [remove_punct(answer) for answer in answers]

            processed["theta"].append(get_value(normal_dist_data, int(pc)))
            processed["R1"].append(answers[0])
            processed["R2"].append(answers[1])
            processed["R3"].append(answers[2])

    return pd.DataFrame(processed)


def updated_load_df() -> pd.DataFrame:
    "loads new GPT responses and exports to `percentiles_resps.csv`"
    q0_data: list[dict[str, str]] = json.load(open("output/ResponsesQ0_13122023.json", "r", encoding="utf-8"))
    q1_data: list[dict[str, str]] = json.load(open("output/ResponsesQ1_13122023.json", "r", encoding="utf-8"))
    q2_data: list[dict[str, str]] = json.load(open("output/ResponsesQ2_13122023.json", "r", encoding="utf-8"))

    processed = defaultdict(list)

    for i, (a, b, c) in enumerate(zip(q0_data, q1_data, q2_data)):
        processed["percentile"].append(int(math.floor(i / 10) + 1))
        processed["R1"].append(remove_punct(list(a.values())[0]))
        processed["R2"].append(remove_punct(list(b.values())[0]))
        processed["R3"].append(remove_punct(list(c.values())[0]))

    df = pd.DataFrame(processed)
    df.to_csv("output/percentiles_resps.csv", index=False)

    return df


def load_df_r() -> pd.DataFrame:
    return pd.read_csv("output/resps_theta.csv")


def lemmatizer(text: str) -> list[str]:
    doc = nlp(text)
    return [token.lemma_ for token in doc]


def tokenizer_spacy(text: str) -> list[str]:
    doc = nlp(text)
    return [token.text for token in doc]


def tokenizer_regex(text: str) -> list[str]:
    return re.findall(r"\w+|[^\w\s]+", text)


def tokenizer_simple_space(text: str) -> list[str]:
    return text.split(" ")


def remove_punct(text: str) -> str:
    punct = '''!()-[]{};:'"\,<>./?@#$£%^&*_©~“”‘’`|¬¦—–'''
    no_punct = text.translate(str.maketrans('', '', punct))

    # remove double spaces
    no_punct = re.sub(r" +", " ", no_punct)
    return no_punct

