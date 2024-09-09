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


# import os
# os.getcwd()


training_params_v2 = {
  'tfidf__tokenizer': ([tokenizer_simple_space]),
  'tfidf__max_df': (arange(0.3, 0.9, 0.05)),
  'tfidf__min_df': (arange(1 / 990, 25 / 990, 1 / 990)),
  'lasso__alpha': ([0.005, 0.01, 0.02]),  # 0.001, 0.0001 (0.01 is winner when highest available)
}

fixed_params_tfidf = {# to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~
  'tokenizer': (tokenizer_simple_space),# to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~
  'max_features': (75),# to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~
  'max_df': (0.9),# to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~
  'min_df': (2.3466993163962863e-05), # to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~
}  # to be del.d, or relaced with new optimal values !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~ !!!!!!!!!!!!!~

def val_format(val: float):
  if val > 0:
    return f"+{val}"
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
      f"output/new_convert/group_a_weights_04052024.csv", header=False)

  # Notes: calc.ing tf-idf is simply tf*idf --- i.e., tf(t, d) * idf(t)
  # so you just need the idf of each term, in a sim format to lasso_step.coef_
  # Then, when you score new essays you just multiply each dtm term
  # by the idf, before working with weights
  idfs = dict(zip(tfstep.get_feature_names_out(), tfstep.idf_))
  idfs = {k: v for k, v in idfs.items() if v != 0}  # likely not needed
  out_idfs = json_normalize(idfs).T
  out_idfs = out_idfs[out_idfs.index.isin(out_weights.index)]

  out_idfs.to_csv(
    f"output/new_convert/group_a_idfs_04052024.csv", header=False)  # CHANGE THIS PATH FILE !!!!!! !!!!!! !!!!!! !!!!!! !!!!!! !!!!!!

def train_v2(train=True):
  "for `Predict Subtest Score` method"

  if train:
    print(f"Training...")
    print(
      f"Total folds: {prod([len(k) for k in training_params_v2.values()])}")
    df = read_csv('output/new_convert/group_a.csv', header=0,
                  index_col=None)  # updated_load_df()

    # shuffle data
    df = df.sample(frac=1).reset_index(drop=True)

    # put all words to lower, remove html tags, remove remaining punct
    df['open_resp'] = df['open_resp'].str.lower()
    html_pattern = r"<.*?>"  # Pattern for HTML tags
    df['open_resp'] = df['open_resp'].str.replace(html_pattern, "", regex=True)
    punctuation_pattern = r"[^\w\s]"  # Pattern for non-alphabetic characters (excluding whitespace)
    df['open_resp'] = df['open_resp'].str.replace(punctuation_pattern, "", regex=True)

    # drop all stopwords and numbers, and stem words, all using R tm package to ensure compatibility
    open_resps = StrVector(df['open_resp'])

    remove_numbers = robjects.r['removeNumbers']
    remove_words = robjects.r['removeWords']
    stopwords = robjects.r['stopwords']
    stem_document = robjects.r['stemDocument']

    open_resps = remove_numbers(open_resps)  # Remove numbers
    # transform to lower
    open_resps = remove_words(open_resps, stopwords("en"))  # Remove English stopwords
    open_resps = stem_document(open_resps)  # Stem the text

    X = pd.Series(open_resps)
    print([a for a in X if ' the ' in a])
    y = df['score']

    pipeline = Pipeline([
      # suppress 'the parameter will not be used' warning
      ('tfidf', TfidfVectorizer(token_pattern=None)),
      ('lasso', Lasso())
    ])

    # use verbose=2 for more info
    grid_search = GridSearchCV(pipeline, training_params_v2,
                               n_jobs=-1)
    grid_search.fit(X, y)

    with open(f"output/new_convert/group_a_scores_04052024.txt", "a") as f:
      f.write(f"\n----- Results for group A -----\n")
      f.write(str(grid_search.best_score_) + "\n")
      f.write(str(grid_search.best_params_) + "\n")

    best_params = {}
    best_params["max_df"] = round(grid_search.best_params_["tfidf__max_df"], 2)
    # best_params["max_features"] = grid_search.best_params_["tfidf__max_features"]
    best_params["min_df"] = grid_search.best_params_["tfidf__min_df"] / 990

    json_normalize(best_params).to_csv(
      f"output/new_convert/group_a_params_04052024.csv", index=False)

    export_weights(grid_search.best_estimator_)
  else:
    print(f"Training skipped, using existing model...")
    tfidf = TfidfVectorizer(token_pattern=None, **fixed_params_tfidf)
    lasso = Lasso(**fixed_params_lasso)
    pipeline = Pipeline([
      ('tfidf', tfidf),
      ('lasso', lasso)
    ])

    df = read_csv('output/new_convert/group_a.csv', header=0,
                  index_col=None)  # updated_load_df()
    df = df.sample(frac=1).reset_index(drop=True)

    X = df['open_resp']
    y = df['score']

    pipeline.fit(X, y)
    export_weights(pipeline)


train_v2(train=True)
print('___________reached end script___________')
