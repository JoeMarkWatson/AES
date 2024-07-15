#import os
#print(os.getcwd())

import warnings
from numpy import arange, prod
import pandas as pd
from pandas import json_normalize, read_csv
from sklearn.exceptions import ConvergenceWarning
# joe temporarily runs all_utils_joe, instead of this
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import Lasso
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri
from rpy2.robjects.vectors import StrVector
pandas2ri.activate()
tm = rpackages.importr("tm")

from utils.data import tokenizer_simple_space

# ignore 'Objective did not converge' warning
warnings.filterwarnings("ignore", category=ConvergenceWarning)  # not filtering out ConvergenceWarning on Joe machine

training_params_v2 = {
  'tfidf__tokenizer': ([tokenizer_simple_space]),
  'tfidf__max_df': (arange(0.75, 0.9, 0.025)),
  'tfidf__min_df': (arange(10 / 1000, 25 / 1000, 1 / 2000)),
  'lasso__alpha': arange(0.0025, 0.01, 0.00125),
}

# fixed_params_tfidf = {
#   'tokenizer': (tokenizer_simple_space),
#   'max_features': (75),
#   'max_df': (0.9),
#   'min_df': (2.3466993163962863e-05),
# }
#
# fixed_params_lasso = {
#   'alpha': (0.01),
# }


def val_format(val: float):
  if val > 0:
    return f"+{val}"  # fun exits after any return
  return str(val)


def export_weights(pipeline: Pipeline):
  tfstep: TfidfVectorizer = pipeline.named_steps['tfidf']
  lasso_step: Lasso = pipeline.named_steps['lasso']

  weights = dict(zip(tfstep.get_feature_names_out(), lasso_step.coef_))
  weights = {k: v for k, v in weights.items() if v != 0}
  weights = {k: val_format(v) for k, v in weights.items()}
  weights = dict(sorted(weights.items(), key=lambda x: x[0], reverse=True))
  weights["<none>"] = lasso_step.intercept_
  out_weights = json_normalize(weights).T
  out_weights.to_csv(
      f"output/t_p_s_s/weights.csv", header=False)

  idfs = dict(zip(tfstep.get_feature_names_out(), tfstep.idf_))
  idfs = {k: v for k, v in idfs.items() if v != 0}  # likely not needed
  out_idfs = json_normalize(idfs).T
  out_idfs = out_idfs[out_idfs.index.isin(out_weights.index)]

  # Notes. calc.ing tf-idf is tf*idf --- i.e., tf(t, d) * idf(t)
  # So you need the idf of each term, in a sim format to lasso_step.coef_
  # Then, when you score new essays you just multiply each dtm term by the idf, before working with weights

  out_idfs.to_csv(
    f"output/t_p_s_s/idfs.csv", header=False)


def train_v2():
  "for `Predict Subtest Score` method"

  print(f"Training...")
  print(
    f"Total folds: {prod([len(k) for k in training_params_v2.values()])}")

  df = pd.read_csv('output/closed_responses_test.csv')[['sub_score', 'essays']]  # RESTART HERE - this sorted

  #df = read_csv('output/t_p_s_s/group_a.csv', header=0,  # THIS IS WHAT UR TRYING TO MAKE  # OK - U JUST MANAGED TO SORT IT WITH THE ABOVE
  #              # LINE. BUT NOTE THAT U HAVE SOME SLIGHT DIFFERENCES IN FORMAT - MAYBE JUST THAT UR df NOW CONTAINS TRUE_PERC.
  #              index_col=None)  # updated_load_df()

  # shuffle data
  df = df.sample(frac=1).reset_index(drop=True)

  # put all words to lower, remove html tags, remove remaining punct
  df['essays'] = df['essays'].str.lower()
  html_pattern = r"<.*?>"  # Pattern for HTML tags
  df['essays'] = df['essays'].str.replace(html_pattern, "", regex=True)
  punctuation_pattern = r"[^\w\s]"  # Pattern for non-alphabetic characters (excluding whitespace)
  df['essays'] = df['essays'].str.replace(punctuation_pattern, "", regex=True)

  # drop all stopwords and numbers, and stem words, all using R tm package to ensure compatibility
  open_resps = StrVector(df['essays'])

  remove_numbers = robjects.r['removeNumbers']
  remove_words = robjects.r['removeWords']
  stopwords = robjects.r['stopwords']
  stem_document = robjects.r['stemDocument']

  open_resps = remove_numbers(open_resps)  # Remove numbers
  open_resps = remove_words(open_resps, stopwords("en"))  # Remove English stopwords
  open_resps = stem_document(open_resps)  # Stem the text

  X = pd.Series(open_resps)
  print([a for a in X if ' the ' in a])
  y = df['sub_score']

  pipeline = Pipeline([
    # suppress 'the parameter will not be used' warning
    ('tfidf', TfidfVectorizer(token_pattern=None)),
    ('lasso', Lasso())
  ])

  # use verbose=2 for more info
  grid_search = GridSearchCV(pipeline, training_params_v2,
                             n_jobs=-1, cv=10)
  grid_search.fit(X, y)

  with open(f"output/t_p_s_s/model_scores.txt", "a") as f:
    f.write(f"\n----- Results for train set -----\n")
    f.write(str(grid_search.best_score_) + "\n")
    f.write(str(grid_search.best_params_) + "\n")

  best_params = {}
  best_params["max_df"] = round(grid_search.best_params_["tfidf__max_df"], 2)
  # best_params["max_features"] = grid_search.best_params_["tfidf__max_features"]
  best_params["min_df"] = grid_search.best_params_["tfidf__min_df"] / 990

  json_normalize(best_params).to_csv(
    f"output/t_p_s_s/model_params.csv", index=False)

  export_weights(grid_search.best_estimator_)


train_v2()
print('___________reached end script___________')
