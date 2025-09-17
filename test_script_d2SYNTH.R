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
    ttd_vec = c()  # true theta distance vector
    bias_vec = c()  # bias vector
    
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
      ttd_vec = c(ttd_vec, abs(fso_F1 - true_theta))  # formerly: abs(theta_vec - true_theta)  # inefficient to re-calc for all in vector after each closed q administered
      bias_vec = c(bias_vec, fso_F1 - true_theta)  # formerly: abs(theta_vec - true_theta)  # same change reason
      
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
  theta_range = seq(-4, 4, 0.1)
  # get testinfo of all items (x and sometimes o)
  model_test_info = testinfo(x=fit_object, Theta=theta_range)  
  
  if (closed_only) {
    # load data
    itk_df = data_all[c(closed_items, "ID", "true_theta")]
    
    # Calculate the AVERAGE information per closed item
    selected_item_s_info = model_test_info / length(closed_items)
    
  } else {
    # load data
    itk_df = load_kept_GPT_items(kept_items = kept_items_df, fit = fit_object)
    
    # get o item iteminfo
    o_item_names_meta = colnames(itk_df)[!grepl("^q", colnames(itk_df))]  # remove q items
    o_item_names = setdiff(o_item_names_meta, c("ID", "true_theta"))  # Also exclude metadata columns
    all_item_names = colnames(fit_object@Data$data)
    o_item_indices = match(o_item_names, all_item_names)
    
    model_items_info = testinfo(x = fit_object, Theta = theta_range, which.items = o_item_indices)
    if (is.null(dim(model_items_info))) {
      model_items_info = matrix(model_items_info, ncol = 1)
    }
    selected_item_s_info = rowSums(model_items_info)  # For each theta, sum the info across the selected items
  }
  
  r_obj = cat_sim(fit_obj=fit_object, data_all=itk_df)
  r_obj_ext = c(r_obj, list(model_test_info), list(selected_item_s_info))  # append model_test_info and selected_item_s_info to r_obj
  
  return(r_obj_ext)
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


plot_subplots_whole_sample = function(obj_list, color_mapping=color_map) {
  processed_data = list()
  df_indices = 2:5
  
  y_axis_labels = c("Mean θ Estimate", 
                    "Mean θ Estimate Standard Error", 
                    "Mean Absolute Error |θ - true θ|",
                    "Mean Bias (θ - true θ)")
  x_axis_labels = rep("Closed Items Administered", 4)
  metric_titles = c("A. Theta Estimation", 
                    "B. Estimation Precision", 
                    "C. Accuracy", 
                    "D. Bias",
                    "E. Total Test Information",
                    "F. Information From LLM Items vs Average Closed Item")
  metric_labels = setNames(metric_titles, paste0("Metric ", df_indices))
  y_labels = setNames(y_axis_labels, paste0("Metric ", df_indices))
  
  # Define linetypes
  linetypes <- setNames(ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"), names(color_mapping))
  
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
    
    # Ensure factor level ordering for CAT to put Closed Only first
    plot_data$CAT <- factor(plot_data$CAT, levels = names(color_mapping))
    
    p = ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + 
      geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = linetypes) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      labs(x = x_axis_labels[i], y = y_labels[[i]], title = metric_titles[i], color = "Approach", linetype = "Approach") +
      theme(strip.text = element_text(size = 12), axis.title.y = element_text(size = 12))
    
    if (i == 2) {
      p = p + theme(legend.position = c(0.78, 0.63),
                    legend.background = element_rect(color = "black", fill = NA, size = 1))
    } else {
      p = p + theme(legend.position = "none")
    }
    
    plot_list[[i]] = p
  }
  
  # Plot 5: model_test_info
  plot5_data = bind_rows(lapply(names(obj_list), function(cat_name) {
    obj = obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1),
           info = obj$data[[6]],
           CAT = obj$label)
  }))
  plot5_data$CAT <- factor(plot5_data$CAT, levels = names(color_mapping))
  
  p5 = ggplot(plot5_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = metric_titles[5], x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none")
  plot_list[[5]] = p5
  
  # Plot 6: selected_item_s_info
  plot6_data = bind_rows(lapply(names(obj_list), function(cat_name) {
    obj = obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1),
           info = obj$data[[7]],
           CAT = obj$label)
  }))
  plot6_data$CAT <- factor(plot6_data$CAT, levels = names(color_mapping))
  
  p6 = ggplot(plot6_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = metric_titles[6], x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none")
  plot_list[[6]] = p6
  
  # Combine all plots
  grid.arrange(grobs = plot_list, ncol = 2)
}


plot_subplots_theta_groups = function(obj_list, metric = "ttd", subplot_pstn = 2, color_mapping = color_map) {
  if (subplot_pstn == 2) {
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
  
  chunk_titles = c("A. Below -1 SD", "B. Between -1 SD and 0 SD", "C. Between 0 SD and 1 SD", "D. Above 1 SD")
  
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
    
    # Force legend order to match color_mapping (with "Closed Only" first)
    plot_data$CAT = factor(plot_data$CAT, levels = names(color_mapping))
    
    p = ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + 
      geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = setNames(
        ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"),
        names(color_mapping)
      )) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      labs(x = x_axis_labels[i], y = y_axis_labels[i], title = chunk_titles[i], color = "Approach", linetype = "Approach") +
      theme(strip.text = element_text(size = 12), axis.title.y = element_text(size = 12))
    
    if (i == subplot_pstn) {
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


plot_divergence_from_closed <- function(obj_list, color_mapping = color_map) {
  
  # Combine divergence data from all models into a single dataframe
  plot_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
    obj <- obj_list[[cat_name]]
    # The divergence_df is the 8th element we will add
    df <- obj$data[[8]] 
    
    if (!is.null(df)) {
      df_means <- get_mean_values(df)
      df_means$CAT <- obj$label # Add model label
      return(df_means)
    }
    return(NULL)
  }))
  
  # Add the item step number (0 to 19)
  plot_data$item <- 0:19
  
  # Ensure factor level ordering to match the legend
  plot_data$CAT <- factor(plot_data$CAT, levels = names(color_mapping))
  
  # Create the plot
  p <- ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
    geom_point() +
    geom_line(size = 1) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = setNames(
      ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"),
      names(color_mapping)
    )) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.position = "right",
      axis.title.y = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(
      title = "Divergence from Closed-Only Estimates",
      y = expression(paste("Mean Absolute Divergence |", hat(theta), " - ", hat(theta)["closed-final"], "|")),
      x = "Closed Items Administered",
      color = "Approach",
      linetype = "Approach"
    )
  
  # Print the plot
  print(p)
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
  bciai_sNA = list(data = bciai_sNA_objs_r, label = "Best All Items"),
  bccc_sNA = list(data = bccc_sNA_objs_r, label = "Consistent Comparison Only"),  
  bcce_sNA = list(data = bcce_sNA_objs_r, label = "Consistent Evidence Only"),    
  bcvc_nNA = list(data = bcvc_nNA_objs_r, label = "Varying Comparison Only"),    
  bcvc_sNA = list(data = bcvc_sNA_objs_r, label = "Varying Comparison Only (some NA)"),    
  bcve_sNA = list(data = bcve_sNA_objs_r, label = "Varying Evidence Only")
)

color_map <- c(
  "Consistent Comparison Only" = "#d95f02",
  "Consistent Evidence Only" = "#7570b3",
  "Varying Comparison Only" = "#15703c",
  "Varying Comparison Only (some NA)" = "#e6ab02",
  "Varying Evidence Only" = "#a6761d",
  "Best Single Item" = "#1f78b4",
  "Best All Items" = "#e7298a",
  "Closed Only" = "black"
)


# --- Calculate Divergence from Closed-Only Final Thetas ---

# Extract the final theta estimates from the "Closed Only" simulation results
# The theta_df is the 2nd list element, and we need the last column.
closed_only_final_thetas <- obj_list$closed$data[[2]][, ncol(obj_list$closed$data[[2]])]

# Loop through each model in obj_list to calculate its divergence
for (model_name in names(obj_list)) {
  # Get the theta estimates dataframe (at each step) for the current model
  model_theta_df <- obj_list[[model_name]]$data[[2]]
  
  # Calculate the absolute difference from the final closed-only thetas
  # sweep() subtracts the vector from each row of the dataframe
  divergence_df <- abs(sweep(model_theta_df, 1, closed_only_final_thetas, FUN = "-"))
  
  # Store this new dataframe in the list at index 8.
  obj_list[[model_name]]$data[[8]] <- divergence_df
}


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
  bcvc_sNA = list(data = bcvc_sNA_objs_r2, label = "Varying Comparison Only (some NA)"),
  bcvc_nNA = list(data = bcvc_nNA_objs_r2, label = "Varying Comparison Only"),
  bcve_sNA = list(data = bcve_sNA_objs_r2, label = "Varying Evidence Only"),
  bccc_sNA = list(data = bccc_sNA_objs_r2, label = "Consistent Comparison Only"),
  bcce_sNA = list(data = bcce_sNA_objs_r2, label = "Consistent Evidence Only")
)

plot_subplots_theta_groups(obj_list2, metric = "ttd", subplot_pstn = 2)  # for theta est accuracy
plot_subplots_theta_groups(obj_list2, metric = "bias", subplot_pstn = 1)  # for theta est bias

# --- Plot the new Divergence Figure ---
plot_divergence_from_closed(obj_list)

# # #

## checking that plot code has worked
## look at some of the individual points on the plot
#mean(closed_objs_r[[3]]$X2)  # after 1 closed item
#mean(bciai_sNA_objs_r[[3]]$X2)  # after 1 closed item
#mean(closed_objs_r[[3]]$X20)  # after all closed items
#mean(bciai_sNA_objs_r[[3]]$X20)  # after all closed items


# STEPS FOR CARRYING OUT ANOVA

# Load the required libraries for analysis
library(afex)
library(emmeans)
library(tidyverse)

#################################################################
## Step 1: Define the Reusable Analysis Function
#################################################################
# This is the same versatile function from before. It prepares the data,
# runs the ANOVA, and performs the post-hoc tests.

perform_anova_analysis <- function(obj_list, data_index, dv_name) {
  
  # --- Part A: Prepare the Data in a "Long" Format ---
  all_models_data <- list()
  
  for (model_name in names(obj_list)) {
    model_label <- obj_list[[model_name]]$label
    metric_data <- obj_list[[model_name]]$data[[data_index]] 
    
    long_df <- metric_data %>%
      mutate(Id = row_number()) %>%
      pivot_longer(
        cols = -Id,
        names_to = "ItemStep",
        values_to = dv_name 
      ) %>%
      mutate(
        Model = model_label,
        ItemNumber = as.numeric(str_replace(ItemStep, "X", "")) - 1
      )
    
    all_models_data[[model_name]] <- long_df
  }
  
  anova_data <- bind_rows(all_models_data) %>%
    select(Id, Model, ItemNumber, all_of(dv_name)) 
  
  # Convert to factors and filter out ItemNumber 0 to avoid errors with NAs
  anova_data$Model <- factor(anova_data$Model, levels = names(color_map))
  anova_data$Id <- as.factor(anova_data$Id)
  anova_data_filtered <- anova_data %>% filter(ItemNumber > 0)
  
  # --- Part B: Run the Repeated Measures ANOVA ---
  cat(paste("\n--- Running ANOVA for:", dv_name, "---\n"))
  
  aov_results <- aov_ez(
    data = anova_data_filtered,
    dv = dv_name,
    id = "Id",
    within = c("Model", "ItemNumber")
  )
  print(aov_results)
  
  # --- Part C: Run Post-Hoc Tests vs. Baseline ---
  cat(paste("\n--- Running Post-Hoc Comparisons for:", dv_name, "---\n"))
  
  posthoc_results <- emmeans(aov_results, ~ Model, model = "multivariate") %>%
    pairs(ref = "Closed Only", adjust = "bonferroni")
  
  return(posthoc_results)
}


#################################################################
## Step 2: Run the Analysis for Each Metric
#################################################################
# Call the function for Estimation Precision
posthoc_results_precision <- perform_anova_analysis(
  obj_list = obj_list,
  data_index = 3, # tse_df is the 3rd element
  dv_name = "TSE"
)

# Call the function for Accuracy
posthoc_results_accuracy <- perform_anova_analysis(
  obj_list = obj_list,
  data_index = 4, # ttd_df is the 4th element
  dv_name = "Accuracy"
)


#################################################################
## Step 3: View Results and Save to CSV
#################################################################

# --- Estimation Precision ---
cat("\n\n\n--- FINAL RESULTS: ESTIMATION PRECISION ---\n")
print(posthoc_results_precision)
precision_df <- as.data.frame(posthoc_results_precision)
#write.csv(precision_df, "synth_posthoc_precision_results.csv", row.names = FALSE)

# --- Accuracy ---
cat("\n\n\n--- FINAL RESULTS: ACCURACY ---\n")
print(posthoc_results_accuracy)
accuracy_df <- as.data.frame(posthoc_results_accuracy)
#write.csv(accuracy_df, "synth_posthoc_accuracy_results.csv", row.names = FALSE)


#################################################################
## Calculate Information Equivalence for ALL Models
#################################################################

# 1. INITIALIZE A LIST TO STORE RESULTS
# We'll put the results for each model in this list as we loop through them.
results_list <- list()

# 2. EXTRACT THE BASELINE INFORMATION
# We only need to get the average closed item info once.
avg_closed_info <- obj_list$closed$data[[7]]
theta_range <- seq(-4, 4, 0.1)
summary_range <- theta_range >= -2 & theta_range <= 2

# 3. LOOP THROUGH EACH MODEL IN obj_list
# This loop calculates the equivalence ratio for every model except the baseline itself.
for (model_name in names(obj_list)) {
  # Skip the 'closed' model since we can't compare it to itself
  if (model_name == "closed") {
    next
  }
  
  # Extract the info for the current model in the loop
  llm_info <- obj_list[[model_name]]$data[[7]]
  model_label <- obj_list[[model_name]]$label
  
  # Calculate the equivalence ratio
  equivalence_ratio <- llm_info / (avg_closed_info + 1e-9)
  
  # Calculate the average equivalence within the specified theta range
  avg_equivalence <- mean(equivalence_ratio[summary_range])
  
  # Store the results in a temporary data frame
  results_list[[model_name]] <- data.frame(
    Model = model_label,
    AvgEquivalence = avg_equivalence
  )
}

# 4. CREATE AND DISPLAY THE SUMMARY TABLE
# Combine the list of results into a single, clean data frame.
summary_table <- bind_rows(results_list)

# Sort the table from best to worst
summary_table <- summary_table %>%
  arrange(desc(AvgEquivalence))

cat("\n--- Information Equivalence Summary Table ---\n")
print(summary_table)
write.csv(summary_table, "synth_infoEquivSummary.csv", row.names = FALSE)

# 5. IDENTIFY THE BEST MODEL AND GENERATE THE SENTENCE
# The best model is now the first row in our sorted table.
best_model <- summary_table[1, ]

cat("\n--- Top Performing Model ---\n")
cat(sprintf(
  "Across the theta range of [-2, 2], the top-performing '%s' item set provides information equivalent to approximately %.1f average closed items.\n",
  best_model$Model,
  best_model$AvgEquivalence
))

