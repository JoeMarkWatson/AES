# Make true theta, (closed) item bank and (closed item) responses

library(catR)
library(rjson)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set wd


# extract text fun
extract_text <- function(data) {
  extracted_texts <- c()
  
  for (key in names(data)) {
    if (is.list(data[[key]])) {
      for (subkey in seq_along(data[[key]])) {
        if (is.list(data[[key]][[subkey]])) {
          for (subsubkey in names(data[[key]][[subkey]])) {
            if (is.character(data[[key]][[subkey]][[subsubkey]])) {
              extracted_texts <- c(extracted_texts, data[[key]][[subkey]][[subsubkey]])
            }
          }
        }
      }
    }
  }
  
  return(extracted_texts)
}


# closed q item bank and responses
for (t in c('train', 'test')) {
  print(t)
  true_theta <- sort(rnorm(1000, 0, 1))
  true_perc = rep(0:99, each = 10)
  
  if (t == 'train') {
    itemBank <- genDichoMatrix(items=29,model="2PL",aPrior=c("norm",1,0.2),bPrior=c("norm",0,1))  # seed default is 1
    write.csv(itemBank, file='output/item_bank.csv', row.names = F)
  }
  
  data <- data.frame(genPattern(th=true_theta,it=itemBank,model=NULL,seed=1))
  
  if (t == 'train') {
    # set seed and sample 10 cols, for use in train_pred_subtest_score
    set.seed(1)
    sampled_cols = sample(ncol(data), 10)
    sampled_col_names = names(data[, sampled_cols])
    write.csv(sampled_col_names, file='output/sampled_col_names.csv', row.names = F)
  }
  
  sub_score = rowSums(data[, sampled_cols])
  
  # load open resps
  json_file = paste0('output/essay_responses_', t, '2.json')
  json_content <- fromJSON(paste(readLines(json_file, warn = F), collapse=""))
  essays = extract_text(json_content)
  
  # put all together
  data = cbind(true_theta, true_perc, sub_score, data, essays)
  print(head(data, 2))
  
  # save sampled cols
  write.csv(data, file=paste0('output/closed_responses_', t, '.csv'), row.names = F)
}


