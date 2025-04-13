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
  
  #df = df[c(closed_items, kept_items$item)]
  df = df[c(closed_items, kept_items$item, "ID", "fitcF1", "suicide_ideation")]
  
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
  
  params = coef(fit_obj, IRTpar = TRUE, simplify = TRUE)
  itembank = params$items
  itembank_closed = itembank[rownames(itembank) %in% closed_items, ]
  
  # Initialize output objects
  q_just_asked_df = create_obj(data_all)
  theta_df = create_obj(data_all)
  tse_df = create_obj(data_all)
  ctd_df = create_obj(data_all)
  
  open_items = all_items[!all_items %in% c(closed_items, 'ID', 'fitcF1', 'suicide_ideation')]
  suicide_ideation_all = data_all$suicide_ideation  # External measure for all respondents
  
  # Loop over respondents
  for (r in 1:nrow(data_all)) {
    closed_resps = rep(NA, length(closed_items))
    outvec = c()
    q_just_asked_vec = c()
    theta_vec = c()
    tse_vec = c()
    ctd_vec = c()
    
    gptm_only_resp_pat = as.vector(unlist(data_all[open_items][r, ]))
    
    # Loop over CAT steps
    for (i in 0:length(closed_items)) {
      gptm_resp_pat = c(closed_resps, gptm_only_resp_pat)
      
      if (sum(!is.na(gptm_resp_pat)) > 0) {
        fso_fm = fscores(fit_obj, response.pattern = gptm_resp_pat)
        fso_F1 = fso_fm[colnames(fso_fm) == 'F1']
        fso_SE_F1 = fso_fm[colnames(fso_fm) == 'SE_F1']
      } else {
        fso_F1 = NA
        fso_SE_F1 = NA
      }
      
      q_just_asked_vec = c(q_just_asked_vec, ifelse(length(outvec) == 0, NA, outvec[length(outvec)]))
      theta_vec = c(theta_vec, fso_F1)
      tse_vec = c(tse_vec, fso_SE_F1)
      ctd_vec = c(ctd_vec, abs(fso_F1 - data_all[r, 'fitcF1']))
      
      if (sum(is.na(closed_resps)) >= 1) {
        fso_ni = nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1, out = outvec)
        closed_resps[fso_ni$item] = data_all[r, fso_ni$name]
        outvec = c(outvec, fso_ni$item)
      }
    }
    
    q_just_asked_df[r, ] = q_just_asked_vec
    theta_df[r, ] = theta_vec
    tse_df[r, ] = tse_vec
    ctd_df[r, ] = ctd_vec
  }
  
  # Compute correlations AFTER all respondents are processed
  cor_vec = sapply(1:ncol(theta_df), function(i) {
    if (all(is.na(theta_df[, i]))) {
      NA
    } else {
      cor(theta_df[, i], suicide_ideation_all, use = "complete.obs")
    }
  })
  
  return(list(
    q_just_asked_df,
    theta_df,
    tse_df,
    ctd_df,
    cor_vec
  ))
}


get_mean_values <- function(df) {
  col_means <- colMeans(df, na.rm = TRUE)
  new_names <- gsub("^A", "X", names(col_means))
  dataf <- data.frame(item = new_names, mean_value = col_means)
  return(dataf)
}


load_n_cat_sim = function(closed_only = FALSE, kept_items_df, fit_object) {
  theta_range = seq(-4, 4, 0.1)
  
  # Get test information for all items
  model_test_info = testinfo(x = fit_object, Theta = theta_range)
  
  if (closed_only) {
    # Load data (now includes suicide_ideation)
    itk_df = data_all[c(closed_items, "ID", "fitcF1", "suicide_ideation")]  
    
    # Get median item information for closed items
    model_items_info = testinfo(x = fit_object, Theta = theta_range, individual = TRUE)
    selected_item_s_info = apply(model_items_info, 1, median)  # Median info across items
    
  } else {
    # Load data (includes GPT items + suicide_ideation)
    itk_df = load_kept_GPT_items(kept_items = kept_items_df, fit = fit_object)
    
    # Get information for faux-items (open-ended)
    o_item_names_meta = colnames(itk_df)[!grepl("^q", colnames(itk_df))]  # Exclude closed items
    o_item_names = setdiff(o_item_names_meta, c("ID", "fitcF1", "suicide_ideation"))  # Exclude metadata
    all_item_names = colnames(fit_object@Data$data)
    o_item_indices = match(o_item_names, all_item_names)
    
    model_items_info = testinfo(x = fit_object, Theta = theta_range, which.items = o_item_indices)
    
    if (is.null(dim(model_items_info))) {
      model_items_info = matrix(model_items_info, ncol = 1)
    }
    selected_item_s_info = rowSums(model_items_info)  # Sum info across faux-items
  }
  
  # Run CAT simulation (now returns cor_df)
  r_obj = cat_sim(fit_obj = fit_object, data_all = itk_df)
  
  # Append additional info (including correlation results)
  r_obj_ext = c(
    r_obj, 
    list(
      model_test_info = model_test_info,
      selected_item_s_info = selected_item_s_info
    )
  )
  
  return(r_obj_ext)
}


plot_2_subplots_whole_sample = function(obj_list, color_mapping = color_map) {
  processed_data = list()
  df_indices = 2:5  # Plot type 1 for first three subplots
  
  # Define linetypes
  linetypes <- setNames(ifelse(names(color_mapping) == "Closed Only", "dashed", "solid"), names(color_mapping))
  
  y_axis_labels = c(
    "Mean θ Estimate", 
    "Mean θ Estimate Standard Error", 
    "Mean Absolute Divergence from Closed-Only θ",
    "Correlation with Suicidality"  # New label
  )
  x_axis_labels = rep("Closed Items Administered", 4)
  metric_titles = c(
    "Theta Estimation", 
    "Estimation Precision", 
    "Divergence from Closed-Only Estimates",
    "Convergent Validity"  # New title
  )
  y_labels = setNames(y_axis_labels, paste0("Metric ", df_indices))
  
  plot_list = list()
  
  for (i in seq_along(df_indices)) {
    metric = paste0("Metric ", df_indices[i])
    plot_data = bind_rows(lapply(names(obj_list), function(cat_name) {
      obj = obj_list[[cat_name]]
      if (df_indices[i] <= 4) {
        # Original metrics (theta, SE, distance)
        df = obj$data[[df_indices[i]]]
      } else {
        # New correlation metric (5th element)
        df = data.frame(matrix(obj$data[[5]], nrow = 1))  # Convert vector to 1-row df
      }
      
      if (!is.null(df)) {
        df_means = if (df_indices[i] <= 4) {
          get_mean_values(df)
        } else {
          # For correlation, we already have aggregated values
          data.frame(mean_value = as.numeric(df[1, ]))
        }
        df_means$Metric = metric
        df_means$CAT = obj$label
        return(df_means)
      }
      return(NULL)
    }))
    
    plot_data$item = 0:19
    
    # Force factor levels to match desired legend order
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
      labs(x = x_axis_labels[i], y = y_axis_labels[i], title = metric_titles[i],
           color = "Approach", linetype = "Approach") +
      theme(strip.text = element_text(size = 12), axis.title.y = element_text(size = 12))
    
    if (i == 2) {
      p = p + theme(legend.position = c(0.77, 0.61),
                    legend.background = element_rect(color = "black", fill = NA, size = 1))
    } else {
      p = p + theme(legend.position = "none")
    }
    
    plot_list[[i]] = p
  }
  
  # 2nd to last plot: model_test_info
  plot_tinfo_data = bind_rows(lapply(names(obj_list), function(cat_name) {
    obj = obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1),
           info = obj$data[[max(df_indices)+1]],
           CAT = obj$label)
  }))
  plot_tinfo_data$CAT <- factor(plot_tinfo_data$CAT, levels = names(color_mapping))
  
  p_tinfo = ggplot(plot_tinfo_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Total Test Information", x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none")
  plot_list[[length(plot_list) + 1]] <- p_tinfo
  
  # Last plot: selected_item_s_info
  plot_iinfo_data = bind_rows(lapply(names(obj_list), function(cat_name) {
    obj = obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1),
           info = obj$data[[max(df_indices)+2]],
           CAT = obj$label)
  }))
  plot_iinfo_data$CAT <- factor(plot_iinfo_data$CAT, levels = names(color_mapping))
  
  p_iinfo = ggplot(plot_iinfo_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Information From GPT Items vs. Median Closed Item", x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none")
  plot_list[[length(plot_list) + 1]] <- p_iinfo
  
  grid.arrange(grobs = plot_list, ncol = 2)
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

# add final theta est from closed items to data all
fitc_ests_out <- fscores(fitc, response.pattern = data_all[, closed_items])
colnames(fitc_ests_out) <- c("fitcF1", "fitcSE_F1")
data_all <- cbind(data_all, fitc_ests_out)  # put on rhs of existing data_all cols

# add suicide ideation response
ideation_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/ideation_df_TEMPd2.csv')
data_all <- merge(data_all, ideation_df, by = "ID")


# run the sim
closed_objs_r = load_n_cat_sim(closed_only = T, kept_items_df=NA, fit_object=fitc)
bsi_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bsi, fit_object = fit_bsi)
bciai_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bciai_sNA, fit_object = fit_bciai_sNA)
bciai_nNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bciai_nNA, fit_object = fit_bciai_nNA)
bccc_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bccc_sNA, fit_object = fit_bccc_sNA)
bccc_nNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bccc_nNA, fit_object = fit_bccc_nNA)
bcce_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcce_sNA, fit_object = fit_bcce_sNA)
bcce_nNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcce_nNA, fit_object = fit_bcce_nNA)
bcvc_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcvc_sNA, fit_object = fit_bcvc_sNA)
bcve_sNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcve_sNA, fit_object = fit_bcve_sNA)
bcve_nNA_objs_r = load_n_cat_sim(kept_items_df = datasets$items_to_keep_bcve_nNA, fit_object = fit_bcve_nNA)


# plot sim output
obj_list <- list(
  closed = list(data = closed_objs_r, label = "Closed Only"),
  bsi = list(data = bsi_objs_r, label = 'Best Single Item'),
  bciai_sNA = list(data = bciai_sNA_objs_r, label = "Best All Items (some NA)"),
  bciai_nNA = list(data = bciai_nNA_objs_r, label = "Best All Items"),
  bccc_sNA = list(data = bccc_sNA_objs_r, label = "Consistent Comparison Only (some NA)"),
  bccc_nNA = list(data = bccc_nNA_objs_r, label = "Consistent Comparison Only"),
  bcce_sNA = list(data = bcce_sNA_objs_r, label = "Consistent Evidence Only (some NA)"),    
  bcce_nNA = list(data = bcce_nNA_objs_r, label = "Consistent Evidence Only"),    
  bcvc_sNA = list(data = bcvc_sNA_objs_r, label = "Varying Comparison Only"),    
  bcve_sNA = list(data = bcve_sNA_objs_r, label = "Varying Evidence Only (some NA)"),
  bcve_nNA = list(data = bcve_nNA_objs_r, label = "Varying Evidence Only")
)

color_map <- c(
  "Consistent Comparison Only" = "#d95f02",  # also in SYNTH
  "Consistent Comparison Only (some NA)" = "#7fd3b5",
  "Consistent Evidence Only" = "#7570b3",  # also in SYNTH
  "Consistent Evidence Only (some NA)" = "#fb8072",
  "Varying Comparison Only" = "#15703c",             
  "Varying Evidence Only" = "#a6761d",  # also in SYNTH
  "Varying Evidence Only (some NA)" = "#80b1d3",
  "Best Single Item" = "#1f78b4",  # also in SYNTH
  "Best All Items" = "#e7298a",  # also in SYNTH
  "Best All Items (some NA)" = "#ffd92f",
  "Closed Only" = "black"  # also in SYNTH
)


# plot
plot_2_subplots_whole_sample(obj_list)


## look at some of the individual points on the plot
#mean(closed_objs_r[[3]]$X2)
#mean(bciai_sNA_objs_r[[3]]$X2)
#mean(closed_objs_r[[3]]$X20)
#mean(bciai_sNA_objs_r[[3]]$X20)