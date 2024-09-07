# for joe: unhash lines 275 onwards to run

# score_real_essays_d

# Real essay GPT scoring, of train and test (with these scores being separable by ID)


library(catR)
library(dplyr)
library(stringr)
library(httr)
library(tidyverse)
library(psych)
library(mirt)
library(glue) # for string templating
#library(TAM)
#library(WrightMap)
#library(cowplot)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set wd


# WRITE FUNCTIONS

get_fs_score = function(id, scores) {
  # returns the expected GPT response for essay with ID 'id'
  essay_scores <- scores[scores$ID == id,]
  
  standard <- essay_scores[essay_scores$rater == 1,]
  stringent <- essay_scores[essay_scores$rater == 2,]
  lenient <- essay_scores[essay_scores$rater == 3,]
  
  return(glue(
    "
  Standard - S1, {standard$GPT1}; S2, {standard$GPT2}; S3, {standard$GPT3}; S4, {standard$GPT4}; S5, {standard$GPT5}; S6, {standard$GPT6}; S7, {standard$GPT7}; S8, {standard$GPT8}; S9, {standard$GPT9}; S10, {standard$GPT10}.\n
  Stringent - S1, {stringent$GPT1}; S2, {stringent$GPT2}; S3, {stringent$GPT3}; S4, {stringent$GPT4}; S5, {stringent$GPT5}; S6, {stringent$GPT6}; S7, {stringent$GPT7}; S8, {stringent$GPT8}; S9, {stringent$GPT9}; S10, {stringent$GPT10}.\n
  Lenient - S1, {lenient$GPT1}; S2, {lenient$GPT2}; S3, {lenient$GPT3}; S4, {lenient$GPT4}; S5, {lenient$GPT5}; S6, {lenient$GPT6}; S7, {lenient$GPT7}; S8, {lenient$GPT8}; S9, {lenient$GPT9}; S10, {lenient$GPT10}."
  ))
  
}

get_gpt_mark = function(essay,
                        few_shot = FALSE,
                        samples = data.frame(),
                        scores = data.frame()) {
  # few_shot: set TRUE to use sample essays. otherwise uses zero-shot prompt
  # samples = dataframe of 5 sample essays + mcq ans
  # scores = dataframe of 5 sample scores generated with zero-shot prompt
  
  
  system_prompt <- "Rate the statements S1 to S10 on a scale from 1 to 5, based on the extent to which they are evidenced in a high-school student's essay about the thing that makes them most sad.
  Use the following scale: 1 for zero evidence, 2 for little evidence, 3 for moderate evidence, 4 for strong evidence, and 5 for overwhelming evidence.

  Please provide three sets of scores:
    1. A set of scores where you act as a standard rater, assigning values when evaluating the essay that are neither elevated nor diminished.
    2. A set of scores where you act as a stringent rater, leaning towards lower values (closer to 1) when evaluating the essay.
    3. A set of scores where you act as a lenient rater, leaning towards higher values (closer to 5) when evaluating the essay.

  Format your response as follows, where n is an integer from 1 to 5 (including no additional text):

  Standard - S1, n; S2, n; S3, n; S4, n; S5, n; S6, n; S7, n; S8, n; S9, n; S10, n.\n
  Stringent - S1, n; S2, n; S3, n; S4, n; S5, n; S6, n; S7, n; S8, n; S9, n; S10, n.\n
  Lenient - S1, n; S2, n; S3, n; S4, n; S5, n; S6, n; S7, n; S8, n; S9, n; S10, n.

  Statements:

  S1: The student has depression.
  S2: The student experiences problems with concentration or decision-making.
  S3: The student struggles to derive pleasure from activities.
  S4: The student has sleep problems, like insomnia or sleeping too much.
  S5: The student experiences feelings of distress or sadness.
  S6: The student has feelings of isolation or loneliness.
  S7: The student has a lowered self-worth.
  S8: The student lacks energy.
  S9: The student feels anger or irritability.
  S10: The student engages in self-critical thoughts or behaviors."
  
  success = F
  i = 1
  while (i < 5 && success == F) {
    response <- NA
    
    if (few_shot && nrow(samples) == 5) {
      
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        # -H "Authorization: Bearer $OPENAI_API_KEY"
        add_headers(Authorization = paste("Bearer", api_key)),
        # -H "Content-Type: application/json"
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4o-mini",
          messages = list(
            list(role = "system", content = system_prompt),
            list(
              role = "user",
              content = paste0("Student essay: '", samples[1,]$essays, "'")
            ),
            list(
              role = "assistant",
              content = get_fs_score(id = samples[1,]$ID, scores = scores)
            ),
            list(
              role = "user",
              content = paste0("Student essay: '", samples[2,]$essays, "'")
            ),
            list(
              role = "assistant",
              content = get_fs_score(id = samples[2,]$ID, scores = scores)
            ),
            list(
              role = "user",
              content = paste0("Student essay: '", samples[3,]$essays, "'")
            ),
            list(
              role = "assistant",
              content = get_fs_score(id = samples[3,]$ID, scores = scores)
            ),
            list(
              role = "user",
              content = paste0("Student essay: '", samples[4,]$essays, "'")
            ),
            list(
              role = "assistant",
              content = get_fs_score(id = samples[4,]$ID, scores = scores)
            ),
            list(
              role = "user",
              content = paste0("Student essay: '", samples[5,]$essays, "'")
            ),
            list(
              role = "assistant",
              content = get_fs_score(id = samples[5,]$ID, scores = scores)
            ),
            list(
              role = "user",
              content = paste0("Student essay: '", essay, "'")
            )
          ),
          temperature = 0
        )
      )
    } else {
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        # -H "Authorization: Bearer $OPENAI_API_KEY"
        add_headers(Authorization = paste("Bearer", api_key)),
        # -H "Content-Type: application/json"
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4o-mini",
          messages = list(
            list(role = "system", content = system_prompt),
            list(
              role = "user",
              content = paste0("Student essay: '", essay, "'")
            )
          ),
          temperature = 0
        )
      )
    }
    content_out = content(response)
    message_out = content_out$choices[[1]]$message$content
    i = i + 1
    if (length(message_out) != 0) {
      success = T
    }
  }
  return(message_out)
}


extract_values <- function(resp_string) {
  split_str <- unlist(strsplit(resp_string, "ringent"))
  split_strs = split_str[2]
  
  split_moderate = split_str[1]
  split_stringent = unlist(strsplit(split_strs, "enient"))[1]
  split_lenient = unlist(strsplit(split_strs, "enient"))[2]
  sp_list = list(split_moderate, split_stringent, split_lenient)
  
  resps_df = data.frame(matrix(nrow = 3, ncol = 10))  # Initialize df to store q outputs
  names(resps_df) = c(
    'GPT_Q1',
    'GPT_Q2',
    'GPT_Q3',
    'GPT_Q4',
    'GPT_Q5',
    'GPT_Q6',
    'GPT_Q7',
    'GPT_Q8',
    'GPT_Q9',
    'GPT_Q10'
  )
  
  for (k in seq_along(sp_list)) {
    splits <- unlist(strsplit(sp_list[k][[1]], "[SQ]\\d+"))
    
    q_values <- as.vector(rep(NA, 10))  # Initialize vector to store values for each question
    
    # Extract the first number after each split
    for (i in 2:length(splits)) {
      # Skip first element
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



# LOAD DATA AND APPLY FUNCTIONS

if (grepl('/jw/', getwd())) {
  api_key = suppressWarnings(read.csv('../api_key/openai_key2.csv', header = F)) # relative paths
} else {
  api_key = suppressWarnings(read.csv('../api_key/openai_key.csv', header = F))
}
api_key = as.character(api_key)


data_train = read.csv('../cdftlm_train_pur.csv')
data_test = read.csv('../cdftlm_test.csv')
data_test$q16p <- NULL

set.seed(1)
sample_inds <- sample(nrow(data_train), 5)
sample_data <- data_train[sample_inds, ]
data <- rbind(data_train[-sample_inds, ][c("ID", "essays")], data_test[c("ID", "essays")])

raw_scores <- apply(sample_data["essays"], 1, function(x)
  get_gpt_mark(essay = x)) # this is the only get_gpt_mark() call where we don't use few_shot = TRUE 
scores <- data.frame(do.call(rbind, lapply(raw_scores, extract_values)))
scores$ID = rep(sample_data$ID, each = 3)
scores$rater = c(1, 2, 3)
names(scores) = c(
  'GPT1',
  'GPT2',
  'GPT3',
  'GPT4',
  'GPT5',
  'GPT6',
  'GPT7',
  'GPT8',
  'GPT9',
  'GPT10',
  'ID',
  'rater'
)

# get essay scored by GPT

get_gpt_mark(essay = data$essays[7],
             samples = sample_data,
             scores = scores,
             few_shot = TRUE)  # to check fun (1)

apply(head(data["essays"]), 1, function(x) get_gpt_mark(
      essay = x,
      samples = sample_data,
      scores = scores,
      few_shot = TRUE
    ))  # to check fun (2)


### tested up to here, next few lines should work fine tho - IT'S RUNNING - SEE COMMENT ON 285
gpt_marks <- apply(data["essays"], 1, function(x)
  get_gpt_mark(
    essay = x,
    samples = sample_data,
    scores = scores,
    few_shot = TRUE
  ))
# RUNNING FROM 16:26 ON 06092024. MAYBE DONE IN 30 MINS
# U HAVE RUN NONE OF THE LOWER LINES.

resps_list = lapply(gpt_marks, extract_values)
rm = do.call(rbind, resps_list)  # rm for resps_matrix

# change scoring to match closed items
rm = rm - 1
rm = data.frame(rm)
rm$ID = rep(data$ID, each = 3)
rm$rater = c(1, 2, 3)
names(rm) = c(
  'GPT1',
  'GPT2',
  'GPT3',
  'GPT4',
  'GPT5',
  'GPT6',
  'GPT7',
  'GPT8',
  'GPT9',
  'GPT10',
  'ID',
  'rater'
)

# write.csv(rm, 'output/real_scores_incontext_4mini_d.csv', row.names = F)


# NOTES
# You do not need to save a separate file telling you which IDs were selected as training examples,
# as these can be identified from real_scores_incontext_4mini_d.csv. Specifically, the first
# 435 (440-5) IDs are your train set, and the final 220 IDs are the test set.

# read.delim("prompts/score_essays_prompt_d.txt", header=F)  # could poss be used
# paste(system_prompt[[1]], collapse = "\n")  # followed by this, instead of writing out prompt in script.
# Not implemented at present, as small chance it changes prompt formatting.
