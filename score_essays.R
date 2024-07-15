library(catR)
library(dplyr)
library(stringr)
library(TAM)
library(httr)
library(tidyverse)
library(WrightMap)
library(cowplot)
library(psych)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set wd

# 1. load item bank - the one u already saved ___________________________________________________
# 2. get essays scored by GPT ___________________________________________________________________
# 3. save output ________________________________________

# WRITE FUNCTIONS

get_gpt_mark = function(essay) {
  content_in = paste0('Provide scores for the following statements based on a student essay. Each question must be marked on a scale (1, strongly disagree; 2, disagree; 3, neutral; 4, agree; 5, strongly agree).
  
                      Q1: The student has an internal locus of control
                      Q2: The student believes they have complete control over their future
                      Q3: The student attributes their successes and failures to their own efforts
                      Q4: The student believes that their own actions influence their fate to at least some degree
                      Q5: The student takes full responsibility for the events in their life
                      Q6: The student believes that external events have been a completely unimportant factor in shaping their life

                      Please provide three sets of scores:
                      1. A set of scores where you act as a moderate rater, assigning values when evaluating the essay that are neither elevated nor diminished.
                      2. A set of scores where you act as a stringent rater, leaning towards lower values (closer to 1) when evaluating the essay.
                      3. A set of scores where you act as a lenient rater, leaning towards higher values (closer to 5) when evaluating the essay.

                      While the scores are to be chosen by you, your response must adopt this format: 
                      
                      Moderate -  Q1, 3; Q2, 2; Q3, 3; Q4, 3; Q5, 1; Q6, 3.\n
                      Stringent - Q1, 2; Q1, 1; Q3, 2; Q4, 3; Q5, 1; Q6, 2.\n
                      Lenient -   Q1, 3; Q2, 2; Q3, 4; Q4, 4; Q5, 1; Q6, 3.
                      
                      : \nStudent text: "', essay, '"')
  
  success = F
  i = 1
  while (i < 5 && success == F) {
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      # -H "Authorization: Bearer $OPENAI_API_KEY"
      add_headers(Authorization = paste("Bearer", api_key)),
      # -H "Content-Type: application/json"
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        messages = list(list(role = "user", content = content_in))
      )
    )
    content_out = content(response)
    message_out = content_out$choices[[1]]$message$content
    i = i + 1
    if (length(message_out) != 0) {
      success = T
    }
  }
  return(message_out)
}

# LOAD DATA AND APPLY FUNCTIONS

api_key = suppressWarnings(read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/api_key/openai_key.csv', header = F))
api_key = as.character(api_key)

# load closed items
data = read.csv('output/closed_responses_train.csv')

# add open response(s)
#open_resps = read.csv('output/percentiles_resps.csv')  # DELETED
#open_resps = open_resps[order(open_resps$percentile),]  # sort open_resps by percentile  # DELETED
#true_percentile = open_resps$percentile  # DELETED

# get essay scored by GPT
#head(open_resps$R1)
get_gpt_mark(essay = data$essays[990])  # to check fun (1)
apply(head(data["essays"]), 1, function(x) get_gpt_mark(essay = x))  # to check fun (2)
gpt_marks <- apply(data["essays"], 1, function(x) get_gpt_mark(essay = x))

# RESTART HERE - LIKELY TO FINISH 18:10 (17:25 START).

extract_values <- function(resp_string) {
  
  split_str <- unlist(strsplit(resp_string, "ringent"))
  split_strs = split_str[2]
  
  split_moderate = split_str[1]
  split_stringent = unlist(strsplit(split_strs, "enient"))[1]
  split_lenient = unlist(strsplit(split_strs, "enient"))[2]
  sp_list = list(split_moderate, split_stringent, split_lenient)
  
  resps_df = data.frame(matrix(nrow=3, ncol=6))  # Initialize df to store q outputs
  names(resps_df) = c('GPT_Q1', 'GPT_Q2', 'GPT_Q3', 'GPT_Q4', 'GPT_Q5', 'GPT_Q6')
  
  for (k in seq_along(sp_list)) {
    splits <- unlist(strsplit(sp_list[k][[1]], "Q\\d"))
    
    q_values <- as.vector(rep(NA, 6))  # Initialize vector to store values for each question
    
    # Extract the first number after each split
    for (i in 2:length(splits)) { # Skip first element
      q <- as.numeric(gsub("\\D", "", splits[i])) # Extract only digits
      if (!is.na(q)) {
        q_values[i - 1] <- q
      }
    }
    resps_df[k, ] = q_values  # store values in df
  }
  
  
  # Return the values for each question
  return(resps_df)
}

extract_values(resp_string)
resps_list = lapply(gpt_marks, extract_values)
rm = do.call(rbind, resps_list)  # rm for resps_matrix
rm$person = rownames(rm)
rm$person = round(as.numeric(rm$person))

#write.csv(rm, file='output/all_params/gpt_mfr_values.csv', row.names = F)  # to save here
#rm = read.csv('output/all_params/gpt_mfr_values.csv')  # or load from here

# change scoring to match closed items
rm[, 1:6] = rm[, 1:6]-1
