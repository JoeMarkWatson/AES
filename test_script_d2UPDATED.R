# real model testing

# comparing models all viable models

library(catR)
library(dplyr)
library(stringr)
library(mirt)
library(httr)
library(tm)
library(TAM)
library(tidyverse)
library(WrightMap)
library(cowplot)
library(psych)


# # def fun.s

load_kept_GPT_items = function(fit, kept_items, df=data_all) {
  
  # use kept_items to subset data_all
  for (row in c(1:nrow(kept_items))) {
    litem = kept_items[row, 'item']
    conf_val = kept_items[row, 'conf_val']
    
    if (conf_val > 0) {
      SxT_col <- litem
      SxE_col <- gsub("T", "E", litem)
      SxEP_col <- gsub("T", "EP", litem)
      
      if (conf_val == 50) {  # if required confidence is 50+
        df[[SxT_col]][df[[SxE_col]] == F] <- NA  # remove only where most likely resp == F
      }
      if (conf_val < 50) {
        df[[SxT_col]][(df[[SxE_col]] == F) & (df[[SxEP_col]] >= 100-conf_val)] <- NA  # remove only where resp == F is more than 75% prob
      } else {  # if conf_val > 50
        df[[SxT_col]][df[[SxE_col]] == F] <- NA
        df[[SxT_col]][df[[SxEP_col]] < conf_val] <- NA
      }
    }
  }
  
  df = df[c(closed_items, kept_items$item)]
  
  return(df)
}


create_obj = function(ur_df=data_all, closed_item_names=closed_items) {
  df_names = names(ur_df) 
  ncols = length(closed_item_names) + 1  # + 1 to account for when 0 closed items administered
  nrows = nrow(ur_df)
  df = data.frame(matrix(nrow=nrows, ncol=ncols))
  return(df)
}


cat_sim = function(fit_obj, data_all) {
  
  all_items = names(data_all)
  closed_items = all_items[grepl("q", all_items)]
  
  params = coef(fit_obj, IRTpar=T, simplify=TRUE)
  itembank = params$items
  itembank_closed = itembank[rownames(itembank) %in% closed_items, ]
  
  q_just_asked_df = create_obj(data_all)
  theta_df = create_obj(data_all)
  tse_df = create_obj(data_all)
  
  open_items = all_items[!all_items %in% c(closed_items, 'ID')]  # which may be none
  
  for (r in c(1:nrow(data_all))) {
    
    closed_resps = rep(NA, length(closed_items))
    outvec = c()
    q_just_asked_vec = c()
    theta_vec = c()
    tse_vec = c()
    
    gptm_only_resp_pat = as.vector(unlist(data_all[open_items][r, ]))
    
    for (i in c(0:length(closed_items))) {
      gptm_resp_pat = c(closed_resps, gptm_only_resp_pat)
      
      if ((sum(!is.na(gptm_resp_pat))) > 0) {  # to produce NA for 0th closed item where model uses closed only
        fso_fm = fscores(fit_obj, response.pattern=gptm_resp_pat)
        fso_F1 = fso_fm[colnames(fso_fm) == 'F1']
        fso_SE_F1 = fso_fm[colnames(fso_fm) == 'SE_F1']
        # could poss revert to 'MAP' when it convergences. Or could go to NA. (Or, can change source data, via idea on iPhone notes.)
      } else {
        fso_fm = NA
        fso_F1 = NA
        fso_SE_F1 = NA
      }
      
      q_just_asked_vec = c(q_just_asked_vec, ifelse(length(outvec)==0, NA, outvec[length(outvec)]))
      theta_vec = c(theta_vec, fso_F1)
      tse_vec = c(tse_vec, fso_SE_F1)
      
      if (sum(is.na(closed_resps)) >= 1) {
        fso_ni = nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1, out=outvec)
        fso_niName = fso_ni$name
        fso_niNum = fso_ni$item
        closed_resps[fso_niNum] = data_all[r, fso_niName]
        outvec = c(outvec, fso_niNum)
      }
    }
    
    q_just_asked_df[r, ] = q_just_asked_vec
    theta_df[r, ] = theta_vec
    tse_df[r, ] = tse_vec
    
  }
  return(list(q_just_asked_df, theta_df, tse_df))
}


get_mean_values <- function(df) {
  col_means <- colMeans(df, na.rm = TRUE)
  new_names <- gsub("^A", "X", names(col_means))
  dataf <- data.frame(item = new_names, mean_value = col_means)
  return(dataf)
}


plot_lines = function(which_dfs=3) {
  # Prepare the data by calculating mean distances from true theta for each dataframe
  closed_means <- get_mean_values(closed_objs_r[[which_dfs]])
  someNA_means <- get_mean_values(someNA_objs_r[[which_dfs]])
  noNA_means <- get_mean_values(noNA_objs_r[[which_dfs]])  
  bestSingle_means <- get_mean_values(bestSingle_objs_r[[which_dfs]])  
  
  DOevi_means <- get_mean_values(DOevi_objs_r[[which_dfs]])
  DOcom_means <- get_mean_values(DOcom_objs_r[[which_dfs]])
  DO_means <- get_mean_values(DO_objs_r[[which_dfs]])  
  
  # Add a column to identify the type of CAT
  closed_means$CAT <- "closed"
  someNA_means$CAT <- "some NA"
  noNA_means$CAT <- "no NA"
  bestSingle_means$CAT <- "best single item" 
  
  DOevi_means$CAT <- "constant evidence only"
  DOcom_means$CAT <- "constant comparison only"
  DO_means$CAT <- "constant evi or compari" 
  
  combined_means <- bind_rows(closed_means, someNA_means, noNA_means, bestSingle_means, DOevi_means, DOcom_means, DO_means)
  combined_means$item <- c(0:19)
  rownames(combined_means) <- NULL
  
  # Create the joined dot plot
  if (which_dfs == 2 | which_dfs == 4) {
    y_lab = ifelse(which_dfs==4, "Mean Distance from True Theta", "Mean Theta")
    ggtitle = ifelse(which_dfs==4, "Connected Scatter Plot of Mean Distance from True Theta", "Connected Scatter Plot of Mean Theta")
    print(ggplot(combined_means, aes(y = mean_value, x = item, color = CAT)) +
            geom_point() + 
            geom_line(aes(group = CAT), size = 1) +
            theme_minimal() +
            labs(x = "Closed Items Administered", y = y_lab, color = "Approach") +
            ggtitle(ggtitle))
  } else {
    y_lab = "Mean Theta Est SE"
    ggtitle = "Connected Scatter Plot of Mean Theta Est SE"
    print(ggplot(combined_means, aes(y = mean_value, x = item, color = CAT)) +
            geom_point() + 
            geom_line(aes(group = CAT), size = 1) +
            theme_minimal() +
            labs(x = "Closed Items Administered", y = y_lab, color = "Approach") +
            ggtitle(ggtitle) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0.27, NA)))
  }
}


# # employ fun.s

# setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load some models
load("output/fit_closed28122024_d2.RData")  # loads as fitc - the closed only baseline model
load("output/fit_noNA28122024_d2.RData")  # loads as fit_no_NA
load("output/fit_someNA28122024_d2.RData")  # loads as fit_some_NA
load("output/fit_best_single28122024_d2.RData")  # loads as fit_best_single
load("output/fit_DOevi08022025_d2.RData")  # loads as fit_DOevi
load("output/fit_DOcom08022025_d2.RData")  # loads as fit_DOcom
load("output/fit_DO08022025_d2.RData")  # loads as fit_DO
#load("output/main_fit22122024_d2.RData")  # loads as fit  # old trial model

# load GPT scores for essays and sents, as well as what items got selected
file_paths <- list(
  closed_items_df_path = '../cdftlm_test_d2.csv',
  
  MarksGPT_evidence = '../GPT_sentsOutput_11122024.csv',
  MarksGPT_compare = '../GPT_sentsOutput2_24122024.csv',
  MarksGPT_DO_evidence = '../GPT_sentsOutputEvidence_01022025.csv',
  MarksGPT_DO_compare = '../GPT_sentsOutputCompare_01022025.csv',
  
  items_to_keep_someNA = 'output/items_to_keep_someNA.csv',
  items_to_keep_noNA = 'output/items_to_keep_noNA.csv',
  items_to_keep_best_single = 'output/items_to_keep_best_single.csv',
  items_to_keepDOevi = 'output/items_to_keepDOevi.csv',
  items_to_keepDOcom = 'output/items_to_keepDOcom.csv',
  items_to_keepDO = 'output/items_to_keepDO.csv'
)
datasets <- lapply(file_paths, read.csv)

# define item prefixes and column names
prefixes <- c("E", paste0("SC", 1:12))
suffixes <- c("T", "P", "E", "EP")
LLM_items <- as.vector(t(outer(prefixes, suffixes, paste0)))
LLMdf_evi_names = c('ID', LLM_items)
LLMdf_com_names = paste0(LLM_items, 'com')
LLMdf_com_names = c('ID', LLMdf_com_names)

LLMdf_DO_evi_names = paste0(LLM_items, '_DO')
LLMdf_DO_evi_names = c('ID', LLMdf_DO_evi_names)
LLMdf_DO_com_names = paste0(LLM_items, 'com_DO')
LLMdf_DO_com_names = c('ID', LLMdf_DO_com_names)

LLMdf_names = c(LLMdf_evi_names, LLMdf_com_names, LLMdf_DO_evi_names, LLMdf_DO_com_names)

# rename MarksGPT_compare etc items to distinguish from evidence GPT items
names(datasets$MarksGPT_compare) = LLMdf_com_names
names(datasets$MarksGPT_DO_evidence) = LLMdf_DO_evi_names
names(datasets$MarksGPT_DO_compare) = LLMdf_DO_com_names

# run IRT on closed only
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
data_c = datasets$closed_items_df_path[c('ID', closed_items)]
data_all = merge(data_c, datasets$MarksGPT_evidence, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_compare, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_DO_evidence, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_DO_compare, by = 'ID')

data_all[data_all == "False"] = F
data_all[data_all == "True"] = T

itk_sNA_df = load_kept_GPT_items(kept_items = datasets$items_to_keep_someNA, fit = fit_some_NA)
itk_noNA_df = load_kept_GPT_items(kept_items = datasets$items_to_keep_noNA, fit = fit_no_NA)
itkbs_df = load_kept_GPT_items(kept_items = datasets$items_to_keep_best_single, fit = fit_best_single)

itk_DOevi_df = load_kept_GPT_items(kept_items = datasets$items_to_keepDOevi, fit = fit_some_NA)
itk_DOcom_df = load_kept_GPT_items(kept_items = datasets$items_to_keepDOcom, fit = fit_no_NA)
itk_DO_df = load_kept_GPT_items(kept_items = datasets$items_to_keepDO, fit = fit_best_single)

closed_only_df = data_all[closed_items]

# run the sim
closed_objs_r = cat_sim(fit_obj = fitc, data_all = closed_only_df)
someNA_objs_r = cat_sim(fit_obj = fit_some_NA, data_all = itk_sNA_df)
noNA_objs_r = cat_sim(fit_obj = fit_no_NA, data_all = itk_noNA_df)

DOevi_objs_r = cat_sim(fit_obj = fit_DOevi, data_all = itk_DOevi_df)
DOcom_objs_r = cat_sim(fit_obj = fit_DOcom, data_all = itk_DOcom_df)
DO_objs_r = cat_sim(fit_obj = fit_DO, data_all = itk_DO_df)

bestSingle_objs_r = cat_sim(fit_obj = fit_best_single, data_all = itkbs_df)

# plot
plot_lines(which_dfs = 3)  # warning message is OK - it is for missing 0th item value for closed only
plot_lines(which_dfs = 2)  # shows that there is overall shift in est theta - to be revealed through sim whether this is OK

# look at some of the individual points on the plot
mean(closed_objs_r[[3]]$X2)
mean(someNA_objs_r[[3]]$X2)
mean(closed_objs_r[[3]]$X20)
mean(someNA_objs_r[[3]]$X20)

