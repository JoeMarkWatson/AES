library(catR)
library(foreach)
library(dplyr)
library(stringr)
library(mirt)
library(psych)
library(tm)
library(SnowballC)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. load item bank - the one u already saved ___________________________________________________/
# 2. score essays by applying ML model __________________________________________________________/
# 3. run mirt on just closed items, saving the parameters _______________________________________/
# 4. run mirt on closed and ML-scored item, fixing closed items params __________________________/
# 5. do some checks _____________________________________________________________________________/
# - item info of the ML item ____________________________________________________________________/
# - test info with/without the ML item __________________________________________________________/

# load closed items
data = read.csv('output/closed_responses_train.csv')
# remove closed items used for model training
items_used_for_ML = read.csv('output/sampled_col_names.csv')$x
data = data[, !(names(data) %in% items_used_for_ML)]

# set percentile
true_percentile = data$percentile

# get essay scored using previously-trained ML model 
idfs = read.csv('output/t_p_s_s/idfs.csv', header = F)

# vectorise
sents <- data[, "essays"]

clean_text <- function(text) {
  # basic manip using baseR
  text <- tolower(text)  # Convert to lowercase
  text <- gsub("<.*?>", "", text)  # Remove HTML tags (e.g., <br>, <p>, etc.)
  text <- gsub("[[:punct:]]", "", text)  # Remove punctuation and other non-alphabetic characters
  #text <- gsub("\\s+", " ", text)  # Remove extra spaces
  
  # remove numbers, stopwords and stem using tm package
  text = tm::removeNumbers(text)
  text = tm::removeWords(text, stopwords("en"))
  text = tm::stemDocument(text)
  
  return(text)
}

corpus <- Corpus(VectorSource(unlist(lapply(sents, clean_text))))
dobjr <- DocumentTermMatrix(corpus)
dtm <- as.data.frame(as.matrix(dobjr))
dtm_idfs = dtm[names(dtm) %in% idfs$V1]

# predict theta with ML model
weights_ml = as.list(read.csv(paste("output/t_p_s_s/weights.csv", sep=""), header=FALSE))
token_names = weights_ml$V1
token_weights = weights_ml$V2
ML_pred_subtest <- 0.0

for (j in seq(from=1, to=length(token_names))) {
  tok = token_names[j]
  if (tok == "<none>") {
    ML_pred_subtest = ML_pred_subtest + token_weights[j]
  } else {
    if (tok %in% names(dtm_idfs)) {
      ML_pred_subtest = ML_pred_subtest + (dtm_idfs[,tok] * token_weights[j])
    }
  }
}

quintiles <- quantile(ML_pred_subtest, probs = seq(0, 1, by = 0.2))

quintiles[1] = -10000  # adjust lower and upper limit of bottom and top quintiles so that cut works on any value
quintiles[length(quintiles)] = 10000

# save quintiles - it gets used for model_comparison using new data
quintiles_df = data.frame(quintiles)
write.csv(quintiles_df, 'output/t_p_s_s/quintiles.csv')

scores <- cut(ML_pred_subtest, breaks = quintiles, labels = FALSE)
data$ML_quin = scores

# run mirt on just closed items, saving the parameters
fitc = mirt(data[, grep("^X", names(data))], model = 1, technical=list(NCYCLES=3000)) # wrong columns being selected here. which ones are correct?
fitc_params = mod2values(fitc)
fitc_params$est = F

# updating params to accommodate new_item_params
new_item_params <- data.frame(
  group = "all",
  item = "ML_quin",
  class = "graded",
  name = c("a1", "d1", "d2", "d3", "d4"),
  parnum = c(2001:2005),
  value = c(5:1),
  lbound = -Inf,
  ubound = Inf,
  est = TRUE,
  prior.type = "none",
  prior_1 = NaN,
  prior_2 = NaN
)

all_params = rbind(fitc_params[fitc_params$item != "GROUP", ],
                   new_item_params,
                   fitc_params[fitc_params$item == "GROUP", ])
all_params$parnum = c(1:nrow(all_params))  # format all_params
fit = mirt(data[, grep("^(X|ML)", names(data))], model = 1, technical=list(NCYCLES=3000), pars=all_params)
save(fit, file = "output/t_p_s_s/pred_subtest_mirt.RData")

fit_params = mod2values(fit)
write.csv(fit_params, 'output/t_p_s_s/params.csv', row.names = F)

# examining output
coefs <- coef(fit, IRTpar=TRUE, simplify=TRUE)
item_params = data.frame(coefs$items)
tail(item_params)

# test info with/without the dtm items
Theta <- matrix(seq(-4,4,.01))
tinfo_conly = testinfo(fitc, Theta)
tinfo_all = testinfo(fit, Theta)
plot(Theta, tinfo_conly, type='l')
plot(Theta, tinfo_all, type='l')  # info goes up

# item info of the ML item only
info_ML = iteminfo(extract.item(fit, 'ML_quin'), Theta)
plot(Theta, info_ML, type='l')

#_____ _____ _____ _____ _____ _____ _____ _____ _____

# NOTE 1. The ML model was trained on 10 randomly selected items, which are therefore removed from the data

# NOTE 2. Assuming for now that it is OK not to split the set that we are using for parameter creation only
# This may exag the ML_quin discrim param.s, as responses were made using a ML model trained to predict theta hat
# But, params for all closed items sort of have the same 'shortcoming'