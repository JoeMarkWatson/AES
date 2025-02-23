# translate_real_essays_d2.R

library(readxl)
library(dplyr)
library(catR)
library(dplyr)
library(stringr)
library(TAM)
library(httr)
library(tidyverse)
library(psych)
library(mirt)
#library(WrightMap)
#library(cowplot)


# # # write fun.s

translate_colnames <- function(df) {
  # Function to translate column names of a data frame using GPT
  translate_name <- function(col_name) {
    # Create the prompt for translation
    prompt <- paste0(
      "Translate the following column name to English, returning only the translated text with no other information. 
      The column name is the beginning of a sentence, used for a sentence completion task.
      Your translation should therefore end in '...' , indicating that the student should complete the sentence: '", col_name, "'"
    )
    
    # Retry mechanism for robustness
    success <- FALSE
    i <- 1
    translated_name <- col_name # Default to original name if no translation occurs
    
    while (i <= 5 && !success) {
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4o-mini",
          temperature = 0,
          messages = list(
            list(role = "user", content = prompt)
          )
        )
      )
      
      content_out <- content(response)
      if (!is.null(content_out$choices) && length(content_out$choices) > 0) {
        message_out <- content_out$choices[[1]]$message$content
        translated_name <- gsub("TRANSLATED_NAME: ", "", message_out)
        success <- TRUE
      } else {
        i <- i + 1
      }
    }
    return(translated_name)
  }
  
  # Apply translation to each column name
  new_col_names <- sapply(colnames(df), translate_name)
  colnames(df) <- new_col_names
  
  return(df)
}


translate_sentence_completions <- function(df, sentence_start_df=fss_df) {
  # Helper function to translate a single cell
  translate_cell <- function(text, sen_start) {
    # Skip translation for NA or empty values
    if (is.na(text) || text == "") {
      return(text)
    }
    
    # Construct the translation prompt
    prompt <- paste0(
      "Translate the following text to English, returning only the translated text. This text is in response to a sentence completion task, where the student completed a sentence beginning;'", 
      sen_start, 
      "'.\n\nText to translate: '", 
      text, 
      "'"
    )
    
    # Retry mechanism
    success <- FALSE
    i <- 1
    translated_text <- text # Default to original if translation fails
    
    while (i <= 5 && !success) {
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4o-mini",
          temperature = 0,
          messages = list(
            list(role = "user", content = prompt)
          )
        )
      )
      
      content_out <- content(response)
      if (!is.null(content_out$choices) && length(content_out$choices) > 0) {
        message_out <- content_out$choices[[1]]$message$content
        translated_text <- gsub("TRANSLATED_TEXT: ", "", message_out)
        success <- TRUE
      } else {
        i <- i + 1
      }
    }
    return(translated_text)
  }
  
  # Apply translation row by row
  for (col in colnames(df)) {
    sen_prompt = sentence_start_df[sentence_start_df$names.sct. == col, ]$formatted_sentence_starts
    # Apply translation cell by cell for each column
    df[[col]] <- sapply(df[[col]], translate_cell, sen_start = sen_prompt)
  }
  
  return(df)
}


get_gpt_trans = function(essay) {
  content_in = paste0('Translate the following essay to English, which was written by a Chinese high-school student in response to the prompt "The saddest experience I have had", describing a real experience from their life.
                      
                      Your response must adopt this format: 
                      "TRANSLATED_ESSAY: Text of the translated essay." 
                      
                      Do not put quotation marks around the translated essay.
                      
                      \nEssay to be translated: "', essay, '"')
  
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
        model = "gpt-4o-mini",
        temperature = 0,
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


clean_essay <- function(text) {
  # Performs basic text cleaning. Could also remove various versions of essay title (when at start 
  # of essay and broadly reflects essay question), but avoiding this to keep process simple and replicable.
  
  text = gsub("TRANSLATED_ESSAY:|Translated essay:", "", text)
  text <- trimws(text)
  if (startsWith(text, "'m ")) {
    text <- paste0("I'm ", substr(text, 4, nchar(text)))
  }
  if (startsWith(text, '"') && endsWith(text, '"')) {
    text <- substr(text, 2, nchar(text) - 1)
  }
  if (startsWith(text, "'") && endsWith(text, "'")) {
    text <- substr(text, 2, nchar(text) - 1)
  }
  if (endsWith(text, "'")) {
    text <- substr(text, 1, nchar(text) - 1)
  }
  
  return(text)
}


clean_columns <- function(df) {
  target_columns <- paste0("SC", 1:12)
  
  # Apply clean_essay to each specified column
  for (col in target_columns) {
    if (col %in% colnames(df)) { # Check if the column exists
      df[[col]] <- sapply(df[[col]], clean_essay)
    }
  }
  
  return(df)
}


categorise_slider_vals <- function(x) {
  x = ifelse(x < 5, 0, 1)  # transforming to 0-1 (binary), with an inclusive lower bound of 5 (i.e., 5 is low)
  return(x)
}


# # # employ fun.s

# load data

api_key = suppressWarnings(read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/api_key/openai_key2.csv', header = F))
api_key = as.character(api_key)

cdf = read_xlsx('/Users/jw/Downloads/data_Joe_30052024.xlsx')  # closed items and longer essay
sc_data = read_xlsx('/Users/jw/Downloads/data_Joe_13112024.xlsx')  # shorter sentence completion tasks only

# cleaning of sc_data
sct = translate_colnames(sc_data[, c(17:28)])  # translating column names of sentence completion tasks
formatted_sentence_starts = names(sct)
names(sct) = paste0('SC', c(1:12))
fss_df = cbind(data.frame(names(sct)), formatted_sentence_starts)
sct = cbind(sc_data[, c(1:3)], sct)
# sct_copy2 = sct  # back up
sct = data.frame(sct)

cdft = na.omit(cdf)  # 713 rows have zero missingness
sctt = na.omit(sct)  # 1461 rows have zero missingness
cdftm = merge(cdft, sctt, by=c("ID","gender","age"), all=F)  # 694 rows

# translate all sentence completion items (cols 25 to 36)
translated_sc <- translate_sentence_completions(cdftm[, c(25:36)])  # began around 14:50 Ended around 16:10
translated_sc_id = cbind(cdftm$ID, translated_sc)
names(translated_sc_id)[1] = "ID"
names(fss_df) = c('qual_source', 'translation')
new_row = data.frame(qual_source = "E", translation = "The saddest experience I have had")
fss_df = rbind(new_row, fss_df)
#write.csv(translated_sc_id, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/translated_sc_d2.csv', row.names = F)  # saved 01122024
#write.csv(fss_df, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/translated_sc_names_d2.csv', row.names = F)  # saved 01122024

translated_sc_id_clean = clean_columns(translated_sc_id)

# and translate essay
#cdf_trial = apply(head(cdfw), 1, function(row) get_gpt_trans(row['text']))  # trial
cdftt = apply(cdft, 1, function(row) get_gpt_trans(row['text']))
cdft$trans_text = cdftt
cdftl = cdft
cdftl$ttnchar = nchar(cdftl$trans_text)  # even short essays could be informative (when taken in context of the essay question)

cdftl$trans_text <- sapply(cdftl$trans_text, clean_essay)

# # closed questions

reverse_cols = c('q8', 'q12', 'q16')

for (i in 1:20) {
  column_name <- paste0("q", i)
  new_column_name <- paste0("q", i, "p")
  cdftl[[new_column_name]] <- categorise_slider_vals(cdftl[[column_name]])
  if (column_name %in% reverse_cols) {
    cdftl[[new_column_name]] = 1-cdftl[[new_column_name]]
  }
}


# # manip data 

keep_cols = c("ID", "gender", "age", "trans_text", "ttnchar", "q1p", "q2p", "q3p", "q4p", "q5p", "q6p", "q7p", "q8p", "q9p",
              "q10p", "q11p", "q12p", "q13p", "q14p", "q15p", "q16p", "q17p", "q18p", "q19p", "q20p")
cdftlkc = cdftl[keep_cols]
all_data = merge(cdftlkc, translated_sc_id_clean, by='ID')

#write.csv(all_data, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv', row.names = F)

