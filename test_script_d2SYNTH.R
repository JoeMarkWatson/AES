# FIND RESTART HERE below

# synth model testing

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
library(ggplot2)
library(gridExtra)


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
  
  df = df[c(closed_items, kept_items$item, "ID", "true_theta")]
  
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
  open_items = all_items[!all_items %in% c(closed_items, 'ID', 'true_theta')]  # which may be none
  
  params = coef(fit_obj, IRTpar=T, simplify=TRUE)
  itembank = params$items
  itembank_closed = itembank[rownames(itembank) %in% closed_items, ]
  
  q_just_asked_df = create_obj(data_all)
  theta_df = create_obj(data_all)
  tse_df = create_obj(data_all)
  ttd_df = create_obj(data_all)
  bias_df = create_obj(data_all)
  
  for (r in c(1:nrow(data_all))) {
    
    closed_resps = rep(NA, length(closed_items))
    true_theta = data_all[r, 'true_theta']
    outvec = c()
    q_just_asked_vec = c()
    theta_vec = c()
    tse_vec = c()
    # ttd_vec = c()  # true theta distance vector
    
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
      ttd_vec = abs(theta_vec - true_theta)
      bias_vec = theta_vec - true_theta
      
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
    ttd_df[r, ] = ttd_vec
    bias_df[r, ] = bias_vec
    
  }
  return(list(q_just_asked_df, theta_df, tse_df, ttd_df, bias_df))
}


cat_sim2 = function(fit_obj, data_all) {
  
  all_items = names(data_all)
  closed_items = all_items[grepl("q", all_items)]
  open_items = all_items[!all_items %in% c(closed_items, 'ID', 'true_theta')]
  
  params = coef(fit_obj, IRTpar=T, simplify=TRUE)
  itembank = params$items
  itembank_closed = itembank[rownames(itembank) %in% closed_items, ]
  
  # Split data into 4 chunks based on true_theta
  chunks = list(
    data_all[data_all$true_theta < -0.63, ],
    data_all[data_all$true_theta >= -0.63 & data_all$true_theta < 0, ],
    data_all[data_all$true_theta >= 0 & data_all$true_theta < 0.63, ],
    data_all[data_all$true_theta >= 0.63, ]
  )
  
  results = list()
  
  for (c in 1:4) {
    chunk_data = chunks[[c]]
    
    q_just_asked_df = create_obj(chunk_data)
    ttd_df = create_obj(chunk_data)
    bias_df = create_obj(chunk_data)
    
    for (r in c(1:nrow(chunk_data))) {
      
      closed_resps = rep(NA, length(closed_items))
      true_theta = chunk_data[r, 'true_theta']
      outvec = c()
      q_just_asked_vec = c()
      theta_vec = c()
      tse_vec = c()
      
      gptm_only_resp_pat = as.vector(unlist(chunk_data[open_items][r, ]))
      
      for (i in c(0:length(closed_items))) {
        gptm_resp_pat = c(closed_resps, gptm_only_resp_pat)
        
        if ((sum(!is.na(gptm_resp_pat))) > 0) {
          fso_fm = fscores(fit_obj, response.pattern=gptm_resp_pat)
          fso_F1 = fso_fm[colnames(fso_fm) == 'F1']
          fso_SE_F1 = fso_fm[colnames(fso_fm) == 'SE_F1']
        } else {
          fso_fm = NA
          fso_F1 = NA
          fso_SE_F1 = NA
        }
        
        q_just_asked_vec = c(q_just_asked_vec, ifelse(length(outvec)==0, NA, outvec[length(outvec)]))
        theta_vec = c(theta_vec, fso_F1)
        tse_vec = c(tse_vec, fso_SE_F1)
        ttd_vec = abs(theta_vec - true_theta)
        bias_vec = theta_vec - true_theta
        
        if (sum(is.na(closed_resps)) >= 1) {
          fso_ni = nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1, out=outvec)
          fso_niName = fso_ni$name
          fso_niNum = fso_ni$item
          closed_resps[fso_niNum] = chunk_data[r, fso_niName]
          outvec = c(outvec, fso_niNum)
        }
      }
      
      q_just_asked_df[r, ] = q_just_asked_vec
      ttd_df[r, ] = ttd_vec
      bias_df[r, ] = bias_vec
    }
    
    results[[paste0("q_just_asked_df", c)]] = q_just_asked_df
    results[[paste0("ttd_df", c)]] = ttd_df
    results[[paste0("bias_df", c)]] = bias_df
  }
  
  return(results)
}


load_n_cat_sim = function(closed_only=F, kept_items_df, fit_object) {
  if (closed_only) {
    itk_df = data_all[c(closed_items, "ID", "true_theta")]
  } else {
    itk_df = load_kept_GPT_items(kept_items = kept_items_df, fit = fit_object)
  }
  r_obj = cat_sim(fit_obj=fit_object, data_all=itk_df)
  
  return(r_obj)
}


load_n_cat_sim2 = function(closed_only=F, kept_items_df, fit_object) {
  if (closed_only) {
    itk_df = data_all[c(closed_items, "ID", "true_theta")]
  } else {
    itk_df = load_kept_GPT_items(kept_items = kept_items_df, fit = fit_object)
  }
  r_obj = cat_sim2(fit_obj=fit_object, data_all=itk_df)
  
  return(r_obj)
}


get_mean_values <- function(df) {
  col_means <- colMeans(df, na.rm = TRUE)
  new_names <- gsub("^A", "X", names(col_means))
  dataf <- data.frame(item = new_names, mean_value = col_means)
  return(dataf)
}


plot_lines = function(obj_list, which_dfs = 3) {
  
  # for individual plot plotting
  # not currently
  
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


plot_subplots_whole_sample = function(obj_list) {
  processed_data = list()
  df_indices = 2:5  # The four data frames to be plotted
  
  y_axis_labels = c("Theta Est", "Theta Est SE", "Absolute Distance from Theta Est to True Theta", "Distance from Theta Est to True Theta")
  metric_titles = c("Mean Theta Est", "Mean Theta Est SE", "Mean Theta Est Accuracy", "Mean Theta Est Bias")
  metric_labels = setNames(metric_titles, paste0("Metric ", df_indices))
  y_labels = setNames(y_axis_labels, paste0("Metric ", df_indices))
  
  plot_list = list()
  
  for (i in seq_along(df_indices)) {
    metric = paste0("Metric ", df_indices[i])
    plot_data = bind_rows(lapply(names(obj_list), function(cat_name) {
      obj = obj_list[[cat_name]]
      df = obj$data[[df_indices[i]]]
      if (!is.null(df)) {
        df_means = get_mean_values(df)
        df_means$Metric = metric
        df_means$CAT = obj$label
        return(df_means)
      }
      return(NULL)
    }))
    
    plot_data$item = 0:19
    
    color_mapping <- setNames(rep_len(RColorBrewer::brewer.pal(8, "Dark2"), length(unique(plot_data$CAT))),
                              unique(plot_data$CAT))
    color_mapping["Closed Only"] <- "black"
    
    p = ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + 
      geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = setNames(ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"), names(color_mapping))) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      labs(x = "Closed Items Administered", y = y_axis_labels[i], title = metric_titles[i], color = "Approach", linetype = "Approach") +
      theme(strip.text = element_text(size = 12), axis.title.y = element_text(size = 12))
    
    if (i == 3) {  # Add legend to subplot 3
      p = p + theme(legend.position = c(0.79, 0.71),
                    legend.background = element_rect(color = "black", fill = NA, size = 1))
    } else {
      p = p + theme(legend.position = "none")
    }
    
    plot_list[[i]] = p
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}


plot_subplots_theta_groups = function(obj_list, metric = "ttd", subplot_pstn=3) {
  if (subplot_pstn == 3) {
    legend_loc = c(0.79, 0.71)
  } else {
    legend_loc = c(0.79, 0.29)
  }
  
  processed_data = list()
  metric_dfs = paste0(metric, "_df")
  
  if (metric == "ttd") {
    y_axis_labels = c("Absolute Distance from Theta Est to True Theta", " ", " ", " ")
  } else {
    y_axis_labels = c("Distance from Theta Est to True Theta", " ", " ", " ")
  }
  x_axis_labels = c(" ", " ", "Closed Items Administered", " ")
  
  chunk_titles = c("Below -1 SD", "Between -1 SD and 0 SD", "Between 0 SD and 1 SD", "Above 1 SD")
  
  plot_list = list()
  
  for (i in 1:4) {
    df_name = paste0(metric_dfs, i)
    
    plot_data = bind_rows(lapply(names(obj_list), function(cat_name) {
      obj = obj_list[[cat_name]]
      df = obj$data[[df_name]]
      if (!is.null(df)) {
        df_means = get_mean_values(df)
        df_means$Chunk = chunk_titles[i]
        df_means$CAT = obj$label
        return(df_means)
      }
      return(NULL)
    }))
    
    plot_data$item = 0:19
    
    color_mapping <- setNames(rep_len(RColorBrewer::brewer.pal(8, "Dark2"), length(unique(plot_data$CAT))),
                              unique(plot_data$CAT))
    color_mapping["Closed Only"] <- "black"
    
    p = ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + 
      geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = setNames(ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"), names(color_mapping))) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      labs(x = x_axis_labels[i], y = y_axis_labels[i], title = chunk_titles[i], color = "Approach", linetype = "Approach") +
      theme(strip.text = element_text(size = 12), axis.title.y = element_text(size = 12))
    
    if (i == subplot_pstn) {  # Add legend to chosen subplot
      p = p + theme(legend.position = legend_loc,
                    legend.background = element_rect(color = "black", fill = NA, size = 1))
    } else {
      p = p + theme(legend.position = "none")
    }
    
    if (metric == "ttd") {
      p = p + ylim(0.18, 1)
    } else if (metric == "bias") {
      p = p + ylim(-1, 1)
    }
    
    plot_list[[i]] = p
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}



reverse_code <- function(x, max_score = 5) {
  return(max_score + 1 - x)  # Adjust max_score based on your scale
}


# # employ fun.s

# setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load some models
load("output/fit_closed_d2_synth.RData")  # loads as fitc - the closed only baseline model
load("output/fit_bsi_d2_synth.RData")  # loads as fit_bsi
load("output/fit_bciai_sNA_d2_synth.RData")  # loads as fit_bciai_sNA
load("output/fit_bcvc_sNA_d2_synth.RData")  # loads as fit_bcvc_sNA
load("output/fit_bcvc_nNA_d2_synth.RData")  # loads as fit_bcvc_nNA
load("output/fit_bcve_sNA_d2_synth.RData")  # loads as fit_bcve_sNA
load("output/fit_bccc_sNA_d2_synth.RData")  # loads as fit_bccc_sNA
load("output/fit_bcce_sNA_d2_synth.RData")  # loads as fit_bcce_sNA


# load GPT scores for essays and sents, as well as what items got selected
root = '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/'
file_paths <- list(
  closed_items_df_path = paste0(root, 'git_repo/output/synth_resps_test_d2UPDATED.csv'),
  
  MarksGPT_evidence = paste0(root, 'GPT_vary_evidence_synth.csv'),
  MarksGPT_compare = paste0(root, 'GPT_vary_compare_synth.csv'),
  MarksGPT_DO_evidence = paste0(root, 'GPT_const_compare_synth.csv'),
  MarksGPT_DO_compare = paste0(root, 'GPT_const_evidence_synth.csv'),
  
  items_to_keep_bsi_synth = paste0(root, 'git_repo/output/items_to_keep_bsi_synth.csv'),
  items_to_keep_bciai_sNA_synth = paste0(root, 'git_repo/output/items_to_keep_bciai_sNA_synth.csv'),
  items_to_keep_bcvc_sNA_synth = paste0(root, 'git_repo/output/items_to_keep_bcvc_sNA_synth.csv'),
  items_to_keep_bcvc_nNA_synth = paste0(root, 'git_repo/output/items_to_keep_bcvc_nNA_synth.csv'),
  items_to_keep_bcve_sNA_synth = paste0(root, 'git_repo/output/items_to_keep_bcve_sNA_synth.csv'),
  items_to_keep_bccc_sNA_synth = paste0(root, 'git_repo/output/items_to_keep_bccc_sNA_synth.csv'),
  items_to_keep_bcce_sNA_synth = paste0(root, 'git_repo/output/items_to_keep_bcce_sNA_synth.csv')
)

datasets <- lapply(file_paths, read.csv)

# MarksGPT_evidence and MarksGPT_compare reverse-code items SC6T, SC8T, SC12T
items_to_reverse <- c("SC6T", "SC8T", "SC12T")
datasets$MarksGPT_evidence[items_to_reverse] <- lapply(datasets$MarksGPT_evidence[items_to_reverse], reverse_code)
datasets$MarksGPT_compare[items_to_reverse] <- lapply(datasets$MarksGPT_compare[items_to_reverse], reverse_code)

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
data_c = datasets$closed_items_df_path[c('ID', 'true_theta', closed_items)]
data_all = merge(data_c, datasets$MarksGPT_evidence, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_compare, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_DO_evidence, by = 'ID')
data_all = merge(data_all, datasets$MarksGPT_DO_compare, by = 'ID')

data_all[data_all == "False"] = F
data_all[data_all == "True"] = T

# run the sim
closed_objs_r = load_n_cat_sim(closed_only = T, kept_items_df=NA, fit_object=fitc)
bsi_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bsi_synth, fit_object = fit_bsi)  
bciai_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bciai_sNA_synth, fit_object = fit_bciai_sNA)
bcvc_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcvc_sNA_synth, fit_object = fit_bcvc_sNA)
bcvc_nNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcvc_nNA_synth, fit_object = fit_bcvc_nNA)
bcve_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcve_sNA_synth, fit_object = fit_bcve_sNA)
bccc_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bccc_sNA_synth, fit_object = fit_bccc_sNA)
bcce_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcce_sNA_synth, fit_object = fit_bcce_sNA)

# plot sim output
obj_list <- list(
  closed = list(data = closed_objs_r, label = "Closed Only"),
  bsi = list(data = bsi_objs_r, label = "Best Single Item"),
  bccc_sNA = list(data = bccc_sNA_objs_r, label = "Consistent Comparison Only"),  
  bcce_sNA = list(data = bcce_sNA_objs_r, label = "Consistent Evidence Only"),    
  bciai_sNA = list(data = bciai_sNA_objs_r, label = "Best All Items"),
  bcvc_nNA = list(data = bcvc_nNA_objs_r, label = "Varying Comparison Only (no NA)"),    
  bcvc_sNA = list(data = bcvc_sNA_objs_r, label = "Varying Comparison Only"),    
  bcve_sNA = list(data = bcve_sNA_objs_r, label = "Varying Evidence Only")
)

#plot_lines(obj_list, which_dfs = 2)  # mean theta hat  # warning message is OK - it is for missing 0th item value for closed only
#plot_lines(obj_list, which_dfs = 3)  # theta hat SE
#plot_lines(obj_list, which_dfs = 4)  # absolute distance between theta hat and true theta
#plot_lines(obj_list, which_dfs = 5)  # distance between theta hat and true theta (bias)

plot_subplots_whole_sample(obj_list)  # plot all plots together


# run the 2nd sim
closed_objs_r2 = load_n_cat_sim2(closed_only = T, kept_items_df=NA, fit_object=fitc)
bsi_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bsi_synth, fit_object = fit_bsi)  
bciai_sNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bciai_sNA_synth, fit_object = fit_bciai_sNA)
bcvc_sNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bcvc_sNA_synth, fit_object = fit_bcvc_sNA)
bcvc_nNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bcvc_nNA_synth, fit_object = fit_bcvc_nNA)
bcve_sNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bcve_sNA_synth, fit_object = fit_bcve_sNA)
bccc_sNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bccc_sNA_synth, fit_object = fit_bccc_sNA)
bcce_sNA_objs_r2 = load_n_cat_sim2(kept_items_df = datasets$items_to_keep_bcce_sNA_synth, fit_object = fit_bcce_sNA)

# plot sim2 output
obj_list2 <- list(
  closed = list(data = closed_objs_r2, label = "Closed Only"),
  bsi = list(data = bsi_objs_r2, label = "Best Single Item"),
  bciai_sNA = list(data = bciai_sNA_objs_r2, label = "Best All Items"),
  bcvc_sNA = list(data = bcvc_sNA_objs_r2, label = "Varying Comparison Only"),
  bcvc_nNA = list(data = bcvc_nNA_objs_r2, label = "Varying Comparison Only (No NA)"),
  bcve_sNA = list(data = bcve_sNA_objs_r2, label = "Varying Evidence Only"),
  bccc_sNA = list(data = bccc_sNA_objs_r2, label = "Consistent Comparison Only"),
  bcce_sNA = list(data = bcce_sNA_objs_r2, label = "Consistent Evidence Only")
)

plot_subplots_theta_groups(obj_list2, metric = "ttd")  # for theta est accuracy
plot_subplots_theta_groups(obj_list2, metric = "bias", subplot_pstn = 1)  # for theta est bias


# # #

## checking that plot code has worked
## look at some of the individual points on the plot
#mean(closed_objs_r[[3]]$X2)  # after 1 closed item
#mean(bciai_sNA_objs_r[[3]]$X2)  # after 1 closed item
#mean(closed_objs_r[[3]]$X20)  # after all closed items
#mean(bciai_sNA_objs_r[[3]]$X20)  # after all closed items
