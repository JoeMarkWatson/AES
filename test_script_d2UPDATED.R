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


plot_lines = function(obj_list, which_dfs = 3) {
  
  # Initialize an empty list to store processed data frames
  processed_dfs <- list()
  
  # Loop through each category in obj_list
  for (cat_name in names(obj_list)) {
    obj <- obj_list[[cat_name]]
    df <- obj$data[[which_dfs]]
    
    # Only process if df is not NULL
    if (!is.null(df)) {
      df_means <- get_mean_values(df)
      df_means$CAT <- obj$label
      processed_dfs[[cat_name]] <- df_means  # Store processed data
    }
  }
  
  # Combine all processed data frames into one
  combined_means <- bind_rows(processed_dfs)
  
  # Ensure "item" column is added correctly
  combined_means$item = 0:19
  rownames(combined_means) <- NULL
  
  # Define plot labels dynamically
  y_lab <- ifelse(which_dfs == 5, "Mean Distance from True Theta (Bias)",
                  ifelse(which_dfs == 4, "Mean Absolute Distance from True Theta", 
                         ifelse(which_dfs == 2, "Mean Theta", "Mean Theta Est SE")))
  ggtitle <- ifelse(which_dfs == 5, "Connected Scatter Plot of Mean Distance Between Theta Hat and True Theta (Bias)",
                    ifelse(which_dfs == 4, "Connected Scatter Plot of Mean Absolute Distance Between Theta Hat and True Theta", 
                           ifelse(which_dfs == 2, "Connected Scatter Plot of Mean Theta Hat", "Connected Scatter Plot of Mean Theta Hat Est SE")))
  
  # Generate plot
  p <- ggplot(combined_means, aes(y = mean_value, x = item, color = CAT)) +
    geom_point() + 
    geom_line(aes(group = CAT), size = 1) +
    theme_minimal() +
    labs(x = "Closed Items Administered", y = y_lab, color = "Approach") +
    ggtitle(ggtitle)
  
  # Adjust y-axis if which_dfs is not 2, 4 or 5
  if (!(which_dfs %in% c(2, 4, 5))) {
    p <- p + scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0.27, NA))
  }
  
  print(p)
}


# # employ fun.s

# setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load some models
load("output/fit_closed_d2_real.RData")  # loads as fitc - the closed only baseline model
load("output/fit_bsi_d2_real.RData")
load("output/fit_bciai_sNA_d2_real.RData")
load("output/fit_bciai_nNA_d2_real.RData")
load("output/fit_bcvc_sNA_d2_real.RData")
load("output/fit_bcve_sNA_d2_real.RData")
load("output/fit_bcve_nNA_d2_real.RData")
load("output/fit_bccc_sNA_d2_real.RData")
load("output/fit_bccc_nNA_d2_real.RData")
load("output/fit_bcce_sNA_d2_real.RData")
load("output/fit_bcce_nNA_d2_real.RData")

# load GPT scores for essays and sents, as well as what items got selected
file_paths <- list(
  closed_items_df_path = '../cdftlm_test_d2.csv',
  
  MarksGPT_evidence = '../GPT_sentsOutput_11122024.csv',
  MarksGPT_compare = '../GPT_sentsOutput2_24122024.csv',
  MarksGPT_DO_evidence = '../GPT_sentsOutputEvidence_01022025.csv',
  MarksGPT_DO_compare = '../GPT_sentsOutputCompare_01022025.csv',
  
  items_to_keep_bsi = 'output/items_to_keep_bsi_REALd2.csv',
  items_to_keep_bciai_sNA = 'output/items_to_keep_bciai_sNA_REALd2.csv',
  items_to_keep_bciai_nNA = 'output/items_to_keep_bciai_nNA_REALd2.csv',
  items_to_keep_bccc_sNA = 'output/items_to_keep_bccc_sNA_REALd2.csv',
  items_to_keep_bccc_nNA = 'output/items_to_keep_bccc_nNA_REALd2.csv',
  items_to_keep_bcce_sNA = 'output/items_to_keep_bcce_sNA_REALd2.csv',
  items_to_keep_bcce_nNA = 'output/items_to_keep_bcce_nNA_REALd2.csv',
  items_to_keep_bcvc_sNA = 'output/items_to_keep_bcvc_sNA_REALd2.csv',
  items_to_keep_bcve_sNA = 'output/items_to_keep_bcve_sNA_REALd2.csv',
  items_to_keep_bcve_nNA = 'output/items_to_keep_bcve_nNA_REALd2.csv'
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

# load item banks
itk_bsi = load_kept_GPT_items(kept_items = datasets$items_to_keep_bsi, fit = fit_bsi)
itk_bciai_sNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bciai_sNA, fit = fit_bciai_sNA)
itk_bciai_nNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bciai_nNA, fit = fit_bciai_nNA)
itk_bccc_sNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bccc_sNA, fit = fit_bccc_sNA)
itk_bccc_nNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bccc_nNA, fit = fit_bccc_nNA)
itk_bcce_sNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bcce_sNA, fit = fit_bcce_sNA)
itk_bcce_nNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bcce_nNA, fit = fit_bcce_nNA)
itk_bcvc_sNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bcvc_sNA, fit = fit_bcvc_sNA)
itk_bcve_sNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bcve_sNA, fit = fit_bcve_sNA)
itk_bcve_nNA = load_kept_GPT_items(kept_items = datasets$items_to_keep_bcve_nNA, fit = fit_bcve_nNA)

closed_only_df = data_all[closed_items]

# run the sim
closed_objs_r = cat_sim(fit_obj = fitc, data_all = closed_only_df)
bsi_objs_r = cat_sim(fit_obj = fit_bsi, data_all = itk_bsi)
bciai_sNA_objs_r = cat_sim(fit_obj = fit_bciai_sNA, data_all = itk_bciai_sNA)
bciai_nNA_objs_r = cat_sim(fit_obj = fit_bciai_nNA, data_all = itk_bciai_nNA)
bccc_sNA_objs_r = cat_sim(fit_obj = fit_bccc_sNA, data_all = itk_bccc_sNA)
bccc_nNA_objs_r = cat_sim(fit_obj = fit_bccc_nNA, data_all = itk_bccc_nNA)
bcce_sNA_objs_r = cat_sim(fit_obj = fit_bcce_sNA, data_all = itk_bcce_sNA)
bcce_nNA_objs_r = cat_sim(fit_obj = fit_bcce_nNA, data_all = itk_bcce_nNA)
bcvc_sNA_objs_r = cat_sim(fit_obj = fit_bcvc_sNA, data_all = itk_bcvc_sNA)
bcve_sNA_objs_r = cat_sim(fit_obj = fit_bcve_sNA, data_all = itk_bcve_sNA)
bcve_nNA_objs_r = cat_sim(fit_obj = fit_bcve_nNA, data_all = itk_bcve_nNA)

# check sim output
obj_list <- list(
  closed = list(data = closed_objs_r, label = "closed"),
  bsi = list(data = bsi_objs_r, label = 'best single item'),
  bciai_sNA = list(data = bciai_sNA_objs_r, label = "best all items"),
  bciai_nNA = list(data = bciai_nNA_objs_r, label = "best all items, no NA permitted"),
  bccc_sNA = list(data = bccc_sNA_objs_r, label = "consistent comparison only"),
  bccc_nNA = list(data = bccc_nNA_objs_r, label = "consistent comparison only, no NA permitted"),
  bcce_sNA = list(data = bcce_sNA_objs_r, label = "consistent evidence only"),    
  bcce_nNA = list(data = bcce_nNA_objs_r, label = "consistent evidence only, no NA permitted"),    
  bcvc_sNA = list(data = bcvc_sNA_objs_r, label = "varying comparison only"),    
  bcve_sNA = list(data = bcve_sNA_objs_r, label = "varying evidence only"),
  bcve_nNA = list(data = bcve_nNA_objs_r, label = "varying evidence only, no NA permitted")
)


# plot
plot_lines(obj_list, which_dfs = 3)  # warning message is OK - it is for missing 0th item value for closed only
plot_lines(obj_list, which_dfs = 2)  # shows that there is overall shift in est theta - to be revealed through sim whether this is OK

# look at some of the individual points on the plot
mean(closed_objs_r[[3]]$X2)
mean(bciai_sNA_objs_r[[3]]$X2)
mean(closed_objs_r[[3]]$X20)
mean(bciai_sNA_objs_r[[3]]$X20)

