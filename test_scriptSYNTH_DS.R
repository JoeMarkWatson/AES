# # # CAT Simulation and Analysis for SYNTHETIC Data # # #

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# # 1. Load packages and Define Functions ----
library(mirt)
library(dplyr)
library(catR)
library(tidyverse)
library(gridExtra)
library(afex)      # For ANOVA
library(emmeans)   # For post-hoc tests
#library(plyr)

# # # FUNCTION DEFINITIONS # # #

# Prepares the dataset for a given set of chosen LLM items.
load_kept_GPT_items = function(kept_items_df, full_df, closed_item_names) {
  required_cols <- c("ID", "true_theta", closed_item_names, kept_items_df$item)
  return(full_df[, required_cols])
}

# Creates an empty dataframe to store simulation results.
create_obj = function(ur_df, closed_item_names) { 
  ncols = length(closed_item_names) + 1 
  nrows = nrow(ur_df)
  df = data.frame(matrix(nrow=nrows, ncol=ncols))
  return(df)
}

# Main CAT simulation engine (whole sample).
cat_sim = function(fit_obj, data_for_sim) {
  all_items <- names(data_for_sim)
  closed_items <- all_items[grepl("^q.*p$", all_items)]
  open_items <- all_items[!all_items %in% c(closed_items, 'ID', 'true_theta')]
  
  params <- coef(fit_obj, IRTpar=TRUE, simplify=TRUE)
  itembank <- params$items
  itembank_closed <- itembank[rownames(itembank) %in% closed_items, ]
  
  theta_df <- create_obj(data_for_sim, closed_items)
  tse_df <- create_obj(data_for_sim, closed_items)
  ttd_df <- create_obj(data_for_sim, closed_items) # True Theta Distance (Accuracy)
  bias_df <- create_obj(data_for_sim, closed_items) # Bias
  
  for (r in 1:nrow(data_for_sim)) {
    closed_resps <- rep(NA, length(closed_items))
    true_theta <- data_for_sim[r, 'true_theta']
    outvec <- c(); theta_vec <- c(); tse_vec <- c(); ttd_vec <- c(); bias_vec <- c()
    gptm_only_resp_pat <- as.vector(unlist(data_for_sim[open_items][r, ]))
    
    for (i in 0:length(closed_items)) {
      full_resp_pat <- c(closed_resps, gptm_only_resp_pat)
      if (sum(!is.na(full_resp_pat)) > 0) {
        fso_fm <- fscores(fit_obj, response.pattern=full_resp_pat)
        fso_F1 <- fso_fm[,'F1']
        fso_SE_F1 <- fso_fm[,'SE_F1']
      } else { fso_F1 <- NA; fso_SE_F1 <- NA }
      
      theta_vec <- c(theta_vec, fso_F1)
      tse_vec <- c(tse_vec, fso_SE_F1)
      ttd_vec <- c(ttd_vec, abs(fso_F1 - true_theta))
      bias_vec <- c(bias_vec, fso_F1 - true_theta)
      
      if (sum(is.na(closed_resps)) >= 1) {
        fso_F1_nextItem = ifelse(is.na(fso_F1), 0, fso_F1) # added 08102025
        fso_ni <- nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1_nextItem, out = outvec)
        closed_resps[fso_ni$item] <- data_for_sim[r, fso_ni$name]
        outvec <- c(outvec, fso_ni$item)
      }
    }
    theta_df[r, ] <- theta_vec; tse_df[r, ] <- tse_vec
    ttd_df[r, ] <- ttd_vec; bias_df[r, ] <- bias_vec
  }
  return(list(NA, theta_df, tse_df, ttd_df, bias_df)) # Return NA for 1st element for consistency
}

# CAT simulation engine for theta-based subgroups.
cat_sim2 = function(fit_obj, data_all) {
  all_items <- names(data_all)
  closed_items <- all_items[grepl("^q.*p$", all_items)]
  open_items <- all_items[!all_items %in% c(closed_items, 'ID', 'true_theta')]
  
  params <- coef(fit_obj, IRTpar=TRUE, simplify=TRUE)
  itembank_closed <- params$items[rownames(params$items) %in% closed_items, ]
  
  chunks <- list(
    data_all[data_all$true_theta < -0.67, ],
    data_all[data_all$true_theta >= -0.67 & data_all$true_theta < 0, ],
    data_all[data_all$true_theta >= 0 & data_all$true_theta < 0.67, ],
    data_all[data_all$true_theta >= 0.67, ]
  )
  
  results <- list()
  for (c in 1:4) {
    chunk_data <- chunks[[c]]
    if (nrow(chunk_data) == 0) next # Skip empty chunks
    
    ttd_df <- create_obj(chunk_data, closed_items)
    bias_df <- create_obj(chunk_data, closed_items)
    
    for (r in 1:nrow(chunk_data)) {
      closed_resps <- rep(NA, length(closed_items))
      true_theta <- chunk_data[r, 'true_theta']
      outvec <- c(); theta_vec <- c()
      gptm_only_resp_pat <- as.vector(unlist(chunk_data[open_items][r, ]))
      
      for (i in 0:length(closed_items)) {
        full_resp_pat <- c(closed_resps, gptm_only_resp_pat)
        if (sum(!is.na(full_resp_pat)) > 0) {
          fso_fm <- fscores(fit_obj, response.pattern=full_resp_pat)
          fso_F1 <- fso_fm[,'F1']
        } else { fso_F1 <- NA }
        theta_vec <- c(theta_vec, fso_F1)
        if (sum(is.na(closed_resps)) >= 1) {
          fso_F1_nextItem = ifelse(is.na(fso_F1), 0, fso_F1) # added 08102025
          fso_ni <- nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1_nextItem, out=outvec)
          closed_resps[fso_ni$item] <- chunk_data[r, fso_ni$name]
          outvec <- c(outvec, fso_ni$item)
        }
      }
      ttd_df[r, ] <- abs(theta_vec - true_theta)
      bias_df[r, ] <- theta_vec - true_theta
    }
    results[[paste0("ttd_df", c)]] <- ttd_df
    results[[paste0("bias_df", c)]] <- bias_df
  }
  return(results)
}

# Wrapper for sim 1.
load_n_cat_sim = function(closed_only=FALSE, kept_items_df, fit_object, full_df, closed_item_names) {
  theta_range <- seq(-4, 4, 0.1)
  model_test_info <- testinfo(x=fit_object, Theta=theta_range)  
  
  if (closed_only) {
    itk_df <- full_df[c("ID", "true_theta", closed_item_names)]
    selected_item_s_info <- model_test_info / length(closed_item_names)
  } else {
    itk_df <- load_kept_GPT_items(kept_items_df = kept_items_df, full_df = full_df, closed_item_names = closed_item_names)
    o_item_names <- kept_items_df$item
    all_model_items <- colnames(fit_object@Data$data)
    o_item_indices <- match(o_item_names, all_model_items)
    model_items_info <- testinfo(x = fit_object, Theta = theta_range, which.items = o_item_indices)
    if (is.null(dim(model_items_info))) model_items_info <- matrix(model_items_info, ncol = 1)
    selected_item_s_info <- rowSums(model_items_info)
  }
  
  r_obj <- cat_sim(fit_obj=fit_object, data_for_sim=itk_df)
  r_obj_ext <- c(r_obj, list(model_test_info), list(selected_item_s_info))
  return(r_obj_ext)
}

# Wrapper for sim 2.
load_n_cat_sim2 = function(closed_only=FALSE, kept_items_df, fit_object, full_df, closed_item_names) {
  if (closed_only) {
    itk_df <- full_df[c("ID", "true_theta", closed_item_names)]
  } else {
    itk_df <- load_kept_GPT_items(kept_items_df = kept_items_df, full_df = full_df, closed_item_names = closed_item_names)
  }
  r_obj <- cat_sim2(fit_obj=fit_object, data_all=itk_df)
  return(r_obj)
}

# Calculates mean values for plotting.
get_mean_values <- function(df) {
  col_means <- colMeans(df, na.rm = TRUE)
  data.frame(item = 0:(length(col_means)-1), mean_value = col_means)
}

# Generates the 6-panel plot.
plot_subplots_whole_sample <- function(obj_list, color_mapping) {
  plot_list <- list()
  df_indices <- 2:5 # Theta, TSE, Accuracy, Bias
  
  # UPDATED LABELS/TITLES
  y_axis_labels <- c(
    "Mean θ Estimate",
    "Mean θ Estimate Standard Error",
    "Mean Abs. Error |θ_est - θ_true|",
    "Mean Bias (θ_est - θ_true)"
  )
  
  metric_titles <- c(
    "A. Theta Estimation",
    "B. Estimation Precision",
    "C. Accuracy",
    "D. Bias",
    "E. Total Test Information",
    "F. Information From LLM Items vs. Average Closed Item"
  )
  
  linetypes <- setNames(
    ifelse(names(color_mapping) == "Baseline", "dashed", "solid"),
    names(color_mapping)
  )
  
  for (i in seq_along(df_indices)) {
    plot_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
      obj <- obj_list[[cat_name]]
      df <- obj$data[[df_indices[i]]]
      if (!is.null(df)) {
        df_means <- get_mean_values(df)
        df_means$CAT <- obj$label
        return(df_means)
      }
      return(NULL)
    }))
    
    plot_data$CAT <- factor(plot_data$CAT, levels = names(color_mapping))
    
    p <- ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = linetypes) +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_text(size = 12)
      ) +
      labs(
        x = "Closed Items Administered",
        y = y_axis_labels[i],
        title = metric_titles[i],
        color = "Approach",
        linetype = "Approach"
      )
    
    p <- if (i == 2) {
      p + theme(
        legend.position = c(0.78, 0.73),
        legend.background = element_rect(color = "black", fill = NA)
      )
    } else {
      p + theme(legend.position = "none")
    }
    plot_list[[i]] <- p
  }
  
  # Plot 5: model_test_info
  plot5_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
    obj <- obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1), info = obj$data[[6]], CAT = obj$label)
  }))
  plot5_data$CAT <- factor(plot5_data$CAT, levels = names(color_mapping))
  
  p5 <- ggplot(plot5_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) + theme_minimal() +
    labs(title = metric_titles[5], x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA), legend.position = "none")
  plot_list[[5]] <- p5
  
  # Plot 6: selected_item_s_info
  plot6_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
    obj <- obj_list[[cat_name]]
    tibble(theta = seq(-4, 4, 0.1), info = obj$data[[7]], CAT = obj$label)
  }))
  plot6_data$CAT <- factor(plot6_data$CAT, levels = names(color_mapping))
  
  p6 <- ggplot(plot6_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
    geom_line(size = 1) + theme_minimal() +
    labs(title = metric_titles[6], x = expression(theta), y = "Information") +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetypes) +
    theme(panel.border = element_rect(color = "black", fill = NA), legend.position = "none")
  plot_list[[6]] <- p6
  
  # Combine all plots
  grid.arrange(grobs = plot_list, ncol = 2)
}

# Generates plots for theta-based subgroups.
plot_subplots_theta_groups <- function(obj_list, metric = "ttd", color_mapping) {
  metric_dfs <- paste0(metric, "_df")
  
  chunk_titles <- c("A. θ < -1 SD", "B. -1 SD <= θ < 0", "C. 0 <= θ < 1 SD", "D. θ >= 1 SD")
  
  plot_list <- list()
  
  for (i in 1:4) {
    df_name <- paste0(metric_dfs, i)
    
    plot_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
      obj <- obj_list[[cat_name]]
      df <- obj$data[[df_name]]
      if (!is.null(df) && nrow(df) > 0) {
        df_means <- get_mean_values(df)
        df_means$CAT <- obj$label
        return(df_means)
      }
      return(NULL)
    }))
    
    if (is.null(plot_data) || nrow(plot_data) == 0) next
    
    plot_data$CAT <- factor(plot_data$CAT, levels = names(color_mapping))
    
    # UPDATED Y LABELS FOR SI PLOTS
    y_lab <- if (i == 1) {
      if (metric == "ttd") {
        "Mean Abs. Error |θ_est - θ_true|"
      } else {
        "Mean Bias (θ_est - θ_true)"
      }
    } else { "" }
    
    x_lab <- if (i == 3) "Closed Items Administered" else ""
    
    p <- ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
      geom_point() + geom_line(size = 1) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = setNames(
        ifelse(names(color_mapping) == "Baseline", "dashed", "solid"),
        names(color_mapping)
      )) +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_text(size=12)
      ) +
      labs(x = x_lab, y = y_lab, title = chunk_titles[i], color = "Approach", linetype = "Approach")
    
    # Legend positioning logic
    if (i == 2) {
      if (metric == "bias") {
        p <- p + theme(
          legend.position = c(0.78, 0.27),
          legend.background = element_rect(color="black", fill=NA)
        )
      } else {
        p <- p + theme(
          legend.position = c(0.78, 0.73),
          legend.background = element_rect(color="black", fill=NA)
        )
      }
    } else {
      p <- p + theme(legend.position = "none")
    }
    
    plot_list[[i]] <- p
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}

# Generates the divergence plot.
plot_divergence_from_closed <- function(obj_list, color_mapping) {
  plot_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
    obj <- obj_list[[cat_name]]
    df <- obj$data[[8]] # The divergence_df is the 8th element 
    if (!is.null(df)) {
      df_means <- get_mean_values(df)
      df_means$CAT <- obj$label
      return(df_means)
    }
    return(NULL)
  }))
  
  plot_data$CAT <- factor(plot_data$CAT, levels = names(color_mapping))
  
  p <- ggplot(plot_data, aes(y = mean_value, x = item, color = CAT, group = CAT, linetype = CAT)) +
    geom_point() + geom_line(size = 1) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = setNames(
      ifelse(names(color_mapping) == "Baseline", "dashed", "solid"),
      names(color_mapping)
    )) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      legend.position = "right",
      axis.title = element_text(size = 12)
    ) +
    labs(
      # No main title for SI divergence plot
      y = expression(paste("Mean Abs. Divergence |", hat(theta)[est], " - ", hat(theta)["closed-final"], "|")),
      x = "Closed Items Administered",
      color = "Approach",
      linetype = "Approach"
    )
  
  print(p)
}

# Performs ANOVA and post-hoc tests.
perform_anova_analysis <- function(obj_list, data_index, dv_name, color_mapping) {
  all_models_data <- list()
  
  for (model_name in names(obj_list)) {
    model_label <- obj_list[[model_name]]$label
    metric_data <- obj_list[[model_name]]$data[[data_index]]
    
    long_df <- metric_data %>%
      mutate(Id = row_number()) %>%
      pivot_longer(cols = -Id, names_to = "ItemStep", values_to = dv_name) %>%
      mutate(Model = model_label, ItemNumber = as.numeric(gsub("X", "", ItemStep)) - 1)
    
    all_models_data[[model_name]] <- long_df
  }
  
  anova_data <- bind_rows(all_models_data) %>%
    select(Id, Model, ItemNumber, all_of(dv_name))
  
  anova_data$Model <- factor(anova_data$Model, levels = names(color_mapping))
  anova_data$Id <- as.factor(anova_data$Id)
  anova_data_filtered <- anova_data %>% filter(ItemNumber > 0, !is.na(!!sym(dv_name)))
  
  cat(paste("\n--- Running ANOVA for:", dv_name, "---\n"))
  aov_results <- aov_ez(data = anova_data_filtered, dv = dv_name, id = "Id", within = c("Model", "ItemNumber"))
  print(aov_results)
  
  cat(paste("\n--- Running Post-Hoc Comparisons for:", dv_name, "---\n"))
  posthoc_results <- emmeans(aov_results, ~ Model, model = "multivariate") %>%
    pairs(ref = "Baseline", adjust = "bonferroni")
  
  return(posthoc_results)
}


# # 2. Main Execution Block ----

# --- Load Data and Models ---
root <- '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/'
load(paste0(root, "git_repo/output/fit_bciai_incremental_SYNTH.RData")) # loads as fit_bciai
load(paste0(root, "git_repo/output/fit_top5_incremental_SYNTH.RData"))  # loads as fit_top5

all_data <- read.csv('output/scored_essays/DS_sentsOutput_SYNTH_d2.csv')
all_candidate_performance <- read.csv(paste0(root, "git_repo/output/all_candidate_items_performance_SYNTH.csv"))

closed_items <- paste0('q', c(1:15, 17:20), 'p')
train_data_df <- read.csv('output/synth_resps_train_d2UPDATED.csv')
fitc <- mirt(train_data_df[closed_items], model=1, technical=list(NCYCLES=3000))

# --- Prepare Item Sets ---
all_best_versions <- all_candidate_performance %>%
  filter(!is.na(b4)) %>%
  mutate(item_base = gsub(".$", "", item)) %>%
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain, na.rm = TRUE)) %>%
  ungroup()

items_to_keep_bciai <- all_best_versions %>%
  filter(n_value >= (nrow(train_data_df) / 4)) %>%
  filter(a >= 0.3)

items_to_keep_top5 <- items_to_keep_bciai %>%
  arrange(desc(tinfo_gain)) %>%
  slice_head(n = 5)

# --- Run CAT Simulation 1 (Whole Sample) ---
print("--- Running CAT Simulation 1: Closed Items Only ---")
closed_objs_r <- load_n_cat_sim(closed_only = TRUE, fit_object=fitc, full_df=all_data, closed_item_names=closed_items)
print("--- Running CAT Simulation 1: Best All Items Model ---")
bciai_objs_r <- load_n_cat_sim(kept_items_df = items_to_keep_bciai, fit_object = fit_bciai, full_df=all_data, closed_item_names=closed_items)
print("--- Running CAT Simulation 1: Top 5 Items Model ---")
top5_objs_r <- load_n_cat_sim(kept_items_df = items_to_keep_top5, fit_object = fit_top5, full_df=all_data, closed_item_names=closed_items)

# --- Prepare Objects for Plotting and Analysis ---
obj_list <- list(
  closed = list(data = closed_objs_r, label = "Baseline"),
  bciai  = list(data = bciai_objs_r,  label = "All Texts"),
  top5   = list(data = top5_objs_r,   label = "Top 5 Texts")
)

color_map <- c(
  "Baseline"    = "black",
  "All Texts"   = "#e7298a", # Pink/Magenta
  "Top 5 Texts" = "#66a61e"  # Green
)

# --- Calculate and Plot Divergence ---
print("--- Calculating Divergence from Closed-Only Final Thetas ---")
closed_only_final_thetas <- obj_list$closed$data[[2]][, ncol(obj_list$closed$data[[2]])]
for (model_name in names(obj_list)) {
  model_theta_df <- obj_list[[model_name]]$data[[2]]
  divergence_df <- abs(sweep(model_theta_df, 1, closed_only_final_thetas, FUN = "-"))
  obj_list[[model_name]]$data[[8]] <- divergence_df
}
plot_divergence_from_closed(obj_list, color_map)

# --- Plot Main Results (Sim 1) ---
print("--- Plotting Main Simulation Results ---")
plot_subplots_whole_sample(obj_list, color_map)

# --- Run CAT Simulation 2 (by Theta Group) ---
print("--- Running CAT Simulation 2: Closed Items Only ---")
closed_objs_r2 <- load_n_cat_sim2(closed_only = TRUE, fit_object=fitc, full_df=all_data, closed_item_names=closed_items)
print("--- Running CAT Simulation 2: Best All Items Model ---")
bciai_objs_r2 <- load_n_cat_sim2(kept_items_df = items_to_keep_bciai, fit_object = fit_bciai, full_df=all_data, closed_item_names=closed_items)
print("--- Running CAT Simulation 2: Top 5 Items Model ---")
top5_objs_r2 <- load_n_cat_sim2(kept_items_df = items_to_keep_top5, fit_object = fit_top5, full_df=all_data, closed_item_names=closed_items)

# --- Plot Sim 2 Results ---
obj_list2 <- list(
  closed = list(data = closed_objs_r2, label = "Baseline"),
  bciai  = list(data = bciai_objs_r2,  label = "All Texts"),
  top5   = list(data = top5_objs_r2,   label = "Top 5 Texts")
)
print("--- Plotting Theta Subgroup Results (Accuracy) ---")
plot_subplots_theta_groups(obj_list2, metric = "ttd",  color_mapping = color_map)
print("--- Plotting Theta Subgroup Results (Bias) ---")
plot_subplots_theta_groups(obj_list2, metric = "bias", color_mapping = color_map)

# --- Perform ANOVA ---
print("--- Performing ANOVA and Post-Hoc Tests ---")
posthoc_results_precision <- perform_anova_analysis(obj_list, 3, "TSE",      color_map)
posthoc_results_accuracy  <- perform_anova_analysis(obj_list, 4, "Accuracy", color_map)

# --- View and Save ANOVA Results ---
cat("\n\n--- ANOVA FINAL RESULTS: ESTIMATION PRECISION ---\n")
print(posthoc_results_precision)
#write.csv(as.data.frame(posthoc_results_precision), "output/synth_posthoc_precision_results.csv", row.names = FALSE)
cat("\n\n--- ANOVA FINAL RESULTS: ACCURACY ---\n")
print(posthoc_results_accuracy)
#write.csv(as.data.frame(posthoc_results_accuracy), "output/synth_posthoc_accuracy_results.csv", row.names = FALSE)

# --- Calculate Information Equivalence ---
print("--- Calculating Information Equivalence ---")
results_list <- list()
avg_closed_info <- obj_list$closed$data[[7]]
theta_range <- seq(-4, 4, 0.1)
summary_range <- theta_range >= -2 & theta_range <= 2

for (model_name in names(obj_list)) {
  if (model_name == "closed") next
  llm_info <- obj_list[[model_name]]$data[[7]]
  equivalence_ratio <- llm_info / (avg_closed_info + 1e-9)
  avg_equivalence <- mean(equivalence_ratio[summary_range], na.rm = TRUE)
  results_list[[model_name]] <- data.frame(Model = obj_list[[model_name]]$label, AvgEquivalence = avg_equivalence)
}
summary_table <- bind_rows(results_list) %>% arrange(desc(AvgEquivalence))

cat("\n--- Information Equivalence Summary Table ---\n")
print(summary_table)
#write.csv(summary_table, "output/synth_infoEquivSummary.csv", row.names = FALSE)

best_model <- summary_table[1, ]
cat("\n--- Top Performing Model ---\n")
cat(sprintf(
  "Across the theta range of [-2, 2], the top-performing '%s' item set provides information equivalent to approximately %.1f average closed items.\n",
  best_model$Model,
  best_model$AvgEquivalence
))

# ============================================================

# Data summary tables (SYNTHETIC DATA)

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# ---- Utility: return mean by position (k) for a person-by-position matrix/data.frame
mean_by_pos <- function(mat) {
  m <- colMeans(mat, na.rm = TRUE)
  tibble::tibble(k = 0:(length(m)-1), mean = as.numeric(m))
}

# ---- Build per-position tables for each model: mean SE and mean MAE
build_position_tables <- function(obj_list) {
  # In your synth cat_sim return:
  #   data[[3]] = tse_df  (SE)
  #   data[[4]] = ttd_df  (Abs error = MAE at person-level; mean across persons gives MAE)
  se_tbl <- dplyr::bind_rows(lapply(names(obj_list), function(mn) {
    label <- obj_list[[mn]]$label
    tse_df <- obj_list[[mn]]$data[[3]]
    mean_by_pos(tse_df) |> dplyr::mutate(Model = label)
  }))
  
  mae_tbl <- dplyr::bind_rows(lapply(names(obj_list), function(mn) {
    label <- obj_list[[mn]]$label
    ttd_df <- obj_list[[mn]]$data[[4]]
    mean_by_pos(ttd_df) |> dplyr::mutate(Model = label)
  }))
  
  list(se_tbl = se_tbl, mae_tbl = mae_tbl)
}

# ---- Compute deltas vs baseline per position:
#      - SE percent reduction vs baseline
#      - MAE percent reduction vs baseline
build_delta_tables_vs_baseline <- function(se_tbl, mae_tbl, baseline_label = "Baseline") {
  se_w  <- tidyr::pivot_wider(se_tbl,  names_from = Model, values_from = mean)
  mae_w <- tidyr::pivot_wider(mae_tbl, names_from = Model, values_from = mean)
  
  other_models_se  <- setdiff(names(se_w),  c("k", baseline_label))
  other_models_mae <- setdiff(names(mae_w), c("k", baseline_label))
  
  se_delta <- se_w
  for (m in other_models_se) {
    se_delta[[paste0("SE_pct_reduction_vs_baseline__", m)]] <-
      100 * (se_w[[baseline_label]] - se_w[[m]]) / se_w[[baseline_label]]
  }
  
  mae_delta <- mae_w
  for (m in other_models_mae) {
    mae_delta[[paste0("MAE_pct_reduction_vs_baseline__", m)]] <-
      100 * (mae_w[[baseline_label]] - mae_w[[m]]) / mae_w[[baseline_label]]
  }
  
  list(se_delta = se_delta, mae_delta = mae_delta)
}

# ---- Headline summaries from per-position deltas
headline_summaries <- function(se_delta, mae_delta,
                               early_ks = 1:5,
                               all_ks = 1:19) {
  
  mean_over_k <- function(df, ks, cols) {
    df2 <- df[df$k %in% ks, , drop = FALSE]
    sapply(cols, function(cn) mean(df2[[cn]], na.rm = TRUE))
  }
  
  se_pct_cols  <- grep("^SE_pct_reduction_vs_baseline__",  names(se_delta),  value = TRUE)
  mae_pct_cols <- grep("^MAE_pct_reduction_vs_baseline__", names(mae_delta), value = TRUE)
  
  se_all   <- mean_over_k(se_delta,  all_ks,   se_pct_cols)
  se_early <- mean_over_k(se_delta,  early_ks, se_pct_cols)
  
  mae_all   <- mean_over_k(mae_delta, all_ks,   mae_pct_cols)
  mae_early <- mean_over_k(mae_delta, early_ks, mae_pct_cols)
  
  dplyr::bind_rows(
    tibble::tibble(Metric = names(se_all),    Window = "All positions (k=1..19)", Value = as.numeric(se_all)),
    tibble::tibble(Metric = names(se_early),  Window = "Early positions (k=1..5)",  Value = as.numeric(se_early)),
    tibble::tibble(Metric = names(mae_all),   Window = "All positions (k=1..19)", Value = as.numeric(mae_all)),
    tibble::tibble(Metric = names(mae_early), Window = "Early positions (k=1..5)",  Value = as.numeric(mae_early))
  )
}

# ---- Proportion reaching SE threshold by position k (0..19)
threshold_prop_by_position <- function(obj_list,
                                       thresholds = c(0.40, 0.35, 0.30)) {
  
  out <- list()
  
  for (mn in names(obj_list)) {
    label <- obj_list[[mn]]$label
    tse_df <- obj_list[[mn]]$data[[3]]  # rows=people, cols=k=0..19
    
    for (T in thresholds) {
      props <- apply(tse_df, 2, function(colv) mean(colv <= T, na.rm = TRUE))
      
      out[[length(out) + 1]] <- tibble::tibble(
        Model = label,
        Threshold = T,
        k = 0:(length(props)-1),
        Prop_reaching = as.numeric(props)
      )
    }
  }
  
  dplyr::bind_rows(out) |>
    dplyr::arrange(Threshold, Model, k)
}

# RUN THE SUMMARIES (SYNTH)
# =========================

pos_tabs <- build_position_tables(obj_list)
se_tbl  <- pos_tabs$se_tbl
mae_tbl <- pos_tabs$mae_tbl

# Delta tables vs Baseline
deltas <- build_delta_tables_vs_baseline(se_tbl, mae_tbl, baseline_label = "Baseline")
se_delta  <- deltas$se_delta
mae_delta <- deltas$mae_delta

# Round to 2DP for appendix use

# Helper: round all numeric columns except k (and optionally Threshold)
round_numeric_cols <- function(df, digits = 2, exclude = c("k", "Threshold")) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, exclude)
  df[num_cols] <- lapply(df[num_cols], function(x) round(x, digits))
  df
}

se_delta_2dp  <- round_numeric_cols(se_delta,  digits = 2, exclude = c("k"))
mae_delta_2dp <- round_numeric_cols(mae_delta, digits = 2, exclude = c("k"))

# Save deltas by position (2DP)
readr::write_csv(se_delta_2dp,  "output/synth_SE_pctReduction_vsBaseline_by_position_2dp.csv")
readr::write_csv(mae_delta_2dp, "output/synth_MAE_pctReduction_vsBaseline_by_position_2dp.csv")

# Headline summaries: all positions and early positions (2DP)
headline <- headline_summaries(se_delta, mae_delta, early_ks = 1:5, all_ks = 1:19) %>%
  dplyr::mutate(Value = round(Value, 2))

readr::write_csv(headline, "output/synth_headline_SEpct_and_MAEpct_2dp.csv")

# Threshold attainment as PROPORTION by position + wide table (2DP)
thresh_prop_tbl <- threshold_prop_by_position(obj_list, thresholds = c(0.40, 0.35, 0.30))

thresh_prop_wide <- thresh_prop_tbl %>%
  tidyr::pivot_wider(
    id_cols     = c(Threshold, k),
    names_from  = Model,
    values_from = Prop_reaching
  ) %>%
  dplyr::arrange(Threshold, k) %>%
  dplyr::relocate(dplyr::any_of(c("Baseline", "Top 5 Texts", "All Texts")), .after = k) %>%
  round_numeric_cols(digits = 2, exclude = c("k", "Threshold"))

readr::write_csv(thresh_prop_wide, "output/synth_threshold_prop_by_position_WIDE_2dp.csv")

