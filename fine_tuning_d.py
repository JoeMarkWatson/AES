# select 50 rows from train set
# fine tune using theses 50 rows
# get output for remaining 1950 in train and 1000 in test

import os
import openai
import pandas as pd

os.chdir(__file__[:-len('fine_tuning.py')])


def prepare_data() -> tuple[pd.DataFrame, pd.DataFrame, dict[str, list[dict[str, str]]]]:
  trainset = pd.read_csv('../cdftlm_train_pur.csv')
  testset = pd.read_csv('../cdftlm_test.csv')

  samples = trainset.sample(50)
  trainset = trainset.drop(samples.index)

  # get GPT zero-shot output for the 50 samples

  return (trainset, testset, "")
