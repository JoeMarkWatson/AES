# gen_synth_closed_d using real item bank parameters, and combine with synth essays

library(catR)
library(rjson)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set wd

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

true_theta <- sort(rnorm(3000, 0, 1))
itemBank <- read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_params.csv')
names(itemBank) = c('item', 'alphaj', 'betaj1', 'betaj2')

resps = data.frame(genPattern(th=true_theta, model='GRM', it=itemBank[c('alphaj', 'betaj1', 'betaj2')], seed=1))
names(resps) = itemBank$item
resps$ID = c(1:nrow(resps))
resps$true_theta = true_theta

# incorporate synth essays
json_file = paste0('output/essay_responses_d.json')
json_content <- fromJSON(paste(readLines(json_file, warn = F), collapse=""))
essays = extract_text(json_content)

#essays_df = data.frame(matrix(nrow=3000, ncol=2))
#names(essays_df) = c('percentile', 'essays')
#essays_df$percentile = rep(0:99, each = 30)
#essays_df$essays = essays
resps$essays = essays

# sample 2000 resps for train, 1000 for test
set.seed(1)
sampled_IDs <- sample(resps$ID, 2000, replace = FALSE)
resps_train = resps[resps$ID %in% sampled_IDs, ]
resps_test = resps[!resps$ID %in% sampled_IDs, ]

# write csv for train and test sets 
write.csv(resps_train, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/synth_resps_train_d.csv', row.names = F)
write.csv(resps_test, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/synth_resps_test_d.csv', row.names = F)

