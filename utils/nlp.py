# nlp functions

import spacy, json
import numpy as np

nlp = spacy.load("en_core_web_sm")

def split_answers(raw : str) -> list[str]:
  '''
  Splits `raw (str)` into a list of three answers

  Returns `(list[str])`
  '''
  raw = raw.replace("\n-", "\n")
  raw = raw.replace("\n -", "\n")
  raw = raw.replace("\n\n", "\n")
  raw = raw.replace("\n\n", "\n") # do this twice in case there are three newlines (unlikely but possible)
  return [k.lstrip("-").strip() for k in raw.split("\n")]