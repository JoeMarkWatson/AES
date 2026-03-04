# # # Model Comparison Script # # #
# Script compares the performance of the baseline, bciai, and top5 models through a CAT simulation

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# # 1. Load Packages ----

library(catR)
library(dplyr)
library(mirt)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(afex)
library(emmeans)

# # 2. Define Functions ----

# This function prepares the data for a given model.
prepare_model_data <- function(fit_object, all_response_data) {
  item_names_in_model <- colnames(fit_object@Data$data)
  model_df <- all_response_data %>%
    select(all_of(item_names_in_model), ID, fitcF1, suicide_ideation)
  return(model_df)
}

# Helper function to create empty dataframes for results
create_obj <- function(ur_df = data_all, closed_item_names = closed_items) {
  ncols <- length(closed_item_names) + 1
  nrows <- nrow(ur_df)
  df <- data.frame(matrix(nrow = nrows, ncol = ncols))
  return(df)
}

# Main CAT simulation function
cat_sim <- function(fit_obj, data_all) {
  all_items <- names(data_all)
  closed_items <- all_items[grepl("q", all_items)]
  
  params <- coef(fit_obj, IRTpar = TRUE, simplify = TRUE)
  itembank <- params$items
  itembank_closed <- itembank[rownames(itembank) %in% closed_items, ]
  
  q_just_asked_df <- create_obj(data_all)
  theta_df <- create_obj(data_all)
  tse_df <- create_obj(data_all)
  ctd_df <- create_obj(data_all) # Closed-only Theta Distance
  
  open_items <- setdiff(all_items, c(closed_items, 'ID', 'fitcF1', 'suicide_ideation'))
  suicide_ideation_all <- data_all$suicide_ideation
  
  for (r in 1:nrow(data_all)) {
    closed_resps <- rep(NA, length(closed_items))
    outvec <- c(); q_just_asked_vec <- c(); theta_vec <- c(); tse_vec <- c(); ctd_vec <- c()
    
    gptm_only_resp_pat <- as.vector(unlist(data_all[open_items][r, ]))
    
    for (i in 0:length(closed_items)) {
      gptm_resp_pat <- c(closed_resps, gptm_only_resp_pat)
      
      if (sum(!is.na(gptm_resp_pat)) > 0) {
        fso_fm <- fscores(fit_obj, response.pattern = gptm_resp_pat)
        fso_F1 <- fso_fm[, 'F1']
        fso_SE_F1 <- fso_fm[, 'SE_F1']
      } else {
        fso_F1 <- NA; fso_SE_F1 <- NA
      }
      
      q_just_asked_vec <- c(q_just_asked_vec, ifelse(length(outvec) == 0, NA, outvec[length(outvec)]))
      theta_vec <- c(theta_vec, fso_F1)
      tse_vec <- c(tse_vec, fso_SE_F1)
      ctd_vec <- c(ctd_vec, abs(fso_F1 - data_all[r, 'fitcF1']))
      
      if (sum(is.na(closed_resps)) >= 1) {
        
        fso_F1_nextItem = ifelse(is.na(fso_F1), 0, fso_F1) # added 08102025
        
        fso_ni <- nextItem(itemBank = itembank_closed, model = 'GRM', theta = fso_F1_nextItem, out = outvec)
        closed_resps[fso_ni$item] <- data_all[r, fso_ni$name]
        outvec <- c(outvec, fso_ni$item)
      }
    }
    q_just_asked_df[r, ] <- q_just_asked_vec
    theta_df[r, ] <- theta_vec
    tse_df[r, ] <- tse_vec
    ctd_df[r, ] <- ctd_vec
  }
  
  cor_vec <- sapply(1:ncol(theta_df), function(i) {
    if (all(is.na(theta_df[, i]))) NA else cor(theta_df[, i], suicide_ideation_all, use = "complete.obs")^2
  })
  
  return(list(q_just_asked_df, theta_df, tse_df, ctd_df, cor_vec))
}

# Helper function to get column means for plotting
get_mean_values <- function(df) {
  col_means <- colMeans(df, na.rm = TRUE)
  data.frame(item = 0:(length(col_means)-1), mean_value = col_means)
}

# Main simulation wrapper function
load_n_cat_sim <- function(fit_object, closed_only = FALSE) {
  theta_range <- seq(-4, 4, 0.1)
  model_test_info <- testinfo(x = fit_object, Theta = theta_range)
  
  if (closed_only) {
    itk_df <- prepare_model_data(fit_object = fit_object, all_response_data = data_all)
    selected_item_s_info <- model_test_info / length(closed_items)
  } else {
    itk_df <- prepare_model_data(fit_object = fit_object, all_response_data = data_all)
    all_item_names <- colnames(fit_object@Data$data)
    o_item_names <- setdiff(all_item_names, closed_items)
    o_item_indices <- match(o_item_names, all_item_names)
    model_items_info <- testinfo(x = fit_object, Theta = theta_range, which.items = o_item_indices)
    selected_item_s_info <- if (is.null(dim(model_items_info))) model_items_info else rowSums(model_items_info)
  }
  
  r_obj <- cat_sim(fit_obj = fit_object, data_all = itk_df)
  r_obj_ext <- c(r_obj, list(model_test_info = model_test_info, selected_item_s_info = selected_item_s_info))
  
  return(r_obj_ext)
}

# Main plotting function
plot_simulation_results <- function(obj_list, color_mapping) {
  plot_list <- list()
  
  # UPDATED TITLES
  metric_titles <- c(
    "A. Theta Estimation",
    "B. Estimation Precision",
    "C. Divergence from Baseline Estimates",
    "D. Convergent Validity (R²)",
    "E. Total Test Information",
    "F. Information From LLM Items vs. Average Closed Item"
  )
  
  # UPDATED Y-AXIS LABELS (C uses expression)
  y_axis_labels <- list(
    "Mean θ Estimate",
    "Mean θ Estimate SE",
    expression(paste("Mean Abs. Divergence |", hat(theta)[est], " - ", hat(theta)["baseline-final"], "|")),
    "Variance Explained in Suicidality (R²)"
  )
  
  linetypes <- setNames(
    ifelse(names(color_mapping) == "Baseline", "dashed", "solid"),
    names(color_mapping)
  )
  
  # Generate Plots A, B, C, D
  for (i in 1:4) {
    data_index <- i + 1
    
    plot_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
      obj <- obj_list[[cat_name]]
      df <- obj$data[[data_index]]
      
      if (!is.null(df)) {
        if (data_index == 5) {
          df_means <- data.frame(item = 0:(length(df)-1), mean_value = as.numeric(df))
        } else {
          df_means <- get_mean_values(df)
        }
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
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      labs(
        x = "Closed Items Administered",
        y = y_axis_labels[[i]],
        title = metric_titles[i],
        color = "Model",
        linetype = "Model"
      )
    
    p <- if (i == 2) {
      p + theme(legend.position = c(0.77, 0.7),
                legend.background = element_rect(color = "black", fill = NA))
    } else {
      p + theme(legend.position = "none")
    }
    plot_list[[i]] <- p
  }
  
  # Generate Plots E and F
  for (i in 6:7) {
    plot_info_data <- bind_rows(lapply(names(obj_list), function(cat_name) {
      obj <- obj_list[[cat_name]]
      tibble(theta = seq(-4, 4, 0.1), info = obj$data[[i]], CAT = obj$label)
    }))
    plot_info_data$CAT <- factor(plot_info_data$CAT, levels = names(color_mapping))
    
    p_info <- ggplot(plot_info_data, aes(x = theta, y = info, color = CAT, linetype = CAT)) +
      geom_line(size = 1) + theme_minimal() +
      labs(
        title = metric_titles[i-1],
        x = expression(theta),
        y = "Information"
      ) +
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = linetypes) +
      theme(panel.border = element_rect(color = "black", fill = NA),
            legend.position = "none")
    plot_list[[i-1]] <- p_info
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}

# Function for ANOVA
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

# # 3. Execute Analysis ----

root <- '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/'

# Load models and data
load(paste0(root, "git_repo/output/fit_closed_d2_real.RData"))
load(paste0(root, "git_repo/output/fit_bciai_incremental.RData"))
load(paste0(root, "git_repo/output/fit_top5_incremental.RData"))
closed_items_test <- read.csv(paste0(root, 'cdftlm_test_d2.csv'))
incremental_test <- read.csv(paste0(root, 'DS_sentsOutput_4prompts_incremental_d2.csv'))
ideation_df <- read.csv(paste0(root, 'ideation_df_TEMPd2.csv'))
closed_items <- paste0('q', c(1:15, 17:20), 'p')

# Merge and prepare data
data_all <- merge(closed_items_test, incremental_test, by = "ID")
data_all <- merge(data_all, ideation_df, by = "ID")
fitc_ests_out <- fscores(fitc, response.pattern = data_all[, closed_items])
colnames(fitc_ests_out) <- c("fitcF1", "fitcSE_F1")
data_all <- cbind(data_all, fitc_ests_out)

# Run simulations
closed_objs_r <- load_n_cat_sim(fit_object = fitc, closed_only = TRUE)
bciai_objs_r <- load_n_cat_sim(fit_object = fit_bciai)
top5_objs_r <- load_n_cat_sim(fit_object = fit_top5)

# # 4. Plot Simulation Output ----

obj_list <- list(
  closed = list(data = closed_objs_r, label = "Baseline"),
  bciai  = list(data = bciai_objs_r, label = "All Texts"),
  top5   = list(data = top5_objs_r, label = "Top 5 Texts")
)

color_map <- c(
  "All Texts"   = "#e7298a",
  "Top 5 Texts" = "#66a61e",
  "Baseline"    = "black"
)

plot_simulation_results(obj_list, color_mapping = color_map)

# # 5. Perform and Save ANOVA Results ----
print("--- Performing ANOVA and Post-Hoc Tests ---")

# Analysis for Estimation Precision (TSE)
posthoc_results_precision <- perform_anova_analysis(obj_list, 3, "TSE", color_map)

# --- View and Save ANOVA Results ---
cat("\n\n--- ANOVA FINAL RESULTS: ESTIMATION PRECISION ---\n")
print(posthoc_results_precision)
write.csv(as.data.frame(posthoc_results_precision), "output/real_posthoc_precision_results.csv", row.names = FALSE)

# # 6. Calculate Information Equivalence ----
print("--- Calculating Information Equivalence ---")
results_list <- list()

# Extract the baseline average closed item info (7th element of the list)
avg_closed_info <- obj_list$closed$data[[7]]
theta_range <- seq(-4, 4, 0.1)
summary_range <- theta_range >= -2 & theta_range <= 2

for (model_name in names(obj_list)) {
  if (model_name == "closed") next
  
  llm_info <- obj_list[[model_name]]$data[[7]]
  equivalence_ratio <- llm_info / (avg_closed_info + 1e-9)
  avg_equivalence <- mean(equivalence_ratio[summary_range], na.rm = TRUE)
  
  results_list[[model_name]] <- data.frame(
    Model = obj_list[[model_name]]$label,
    AvgEquivalence = avg_equivalence
  )
}

# Combine results into a summary table
summary_table <- bind_rows(results_list) %>% arrange(desc(AvgEquivalence))

cat("\n--- Information Equivalence Summary Table ---\n")
print(summary_table)
write.csv(summary_table, "output/real_infoEquivSummary.csv", row.names = FALSE)

# NEW: print BOTH model values explicitly (All Texts and Top 5 Texts) for manuscript copy/paste
equiv_all_texts <- summary_table$AvgEquivalence[summary_table$Model == "All Texts"]
equiv_top5_texts <- summary_table$AvgEquivalence[summary_table$Model == "Top 5 Texts"]
cat("\n--- Manuscript numbers: Information equivalence (theta -2 to 2) ---\n")
cat(sprintf("All Texts: %.1f average closed items\n", equiv_all_texts))
cat(sprintf("Top 5 Texts: %.1f average closed items\n", equiv_top5_texts))


# # 7. Data summary tables ----

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# ---- Utility: return mean by position (k) for a person-by-position matrix/data.frame
mean_by_pos <- function(mat) {
  m <- colMeans(mat, na.rm = TRUE)
  tibble::tibble(k = 0:(length(m)-1), mean = as.numeric(m))
}

# ---- Build per-position tables for each model: mean SE and mean R2
build_position_tables <- function(obj_list) {
  # In your cat_sim return: data[[3]] is tse_df, data[[5]] is cor_vec (R^2 per k)
  se_tbl <- dplyr::bind_rows(lapply(names(obj_list), function(mn) {
    label <- obj_list[[mn]]$label
    tse_df <- obj_list[[mn]]$data[[3]]
    mean_by_pos(tse_df) |> dplyr::mutate(Model = label)
  }))
  
  r2_tbl <- dplyr::bind_rows(lapply(names(obj_list), function(mn) {
    label <- obj_list[[mn]]$label
    r2_vec <- obj_list[[mn]]$data[[5]]
    tibble::tibble(k = 0:(length(r2_vec)-1), mean = as.numeric(r2_vec), Model = label)
  }))
  
  list(se_tbl = se_tbl, r2_tbl = r2_tbl)
}

# ---- Compute deltas vs baseline per position:
#      - SE percent reduction vs baseline
#      - R2 absolute increase vs baseline
build_delta_tables_vs_baseline <- function(se_tbl, r2_tbl, baseline_label = "Baseline") {
  se_w <- tidyr::pivot_wider(se_tbl, names_from = Model, values_from = mean)
  r2_w <- tidyr::pivot_wider(r2_tbl, names_from = Model, values_from = mean)
  
  # SE % reduction columns for each non-baseline model
  other_models_se <- setdiff(names(se_w), c("k", baseline_label))
  se_delta <- se_w
  for (m in other_models_se) {
    se_delta[[paste0("SE_pct_reduction_vs_baseline__", m)]] <-
      100 * (se_w[[baseline_label]] - se_w[[m]]) / se_w[[baseline_label]]
  }
  
  # R2 absolute delta columns for each non-baseline model
  other_models_r2 <- setdiff(names(r2_w), c("k", baseline_label))
  r2_delta <- r2_w
  for (m in other_models_r2) {
    r2_delta[[paste0("dR2_vs_baseline__", m)]] <- r2_w[[m]] - r2_w[[baseline_label]]
  }
  
  list(se_delta = se_delta, r2_delta = r2_delta)
}

# ---- Headline summaries from per-position deltas
headline_summaries <- function(se_delta, r2_delta,
                               early_ks = 1:10,   # <-- CHANGED: early window is now k=1..10
                               all_ks = 1:19) {
  
  mean_over_k <- function(df, ks, cols) {
    df2 <- df[df$k %in% ks, , drop = FALSE]
    sapply(cols, function(cn) mean(df2[[cn]], na.rm = TRUE))
  }
  
  se_pct_cols <- grep("^SE_pct_reduction_vs_baseline__", names(se_delta), value = TRUE)
  r2_d_cols   <- grep("^dR2_vs_baseline__", names(r2_delta), value = TRUE)
  
  se_all   <- mean_over_k(se_delta, all_ks, se_pct_cols)
  se_early <- mean_over_k(se_delta, early_ks, se_pct_cols)
  
  r2_all   <- mean_over_k(r2_delta, all_ks, r2_d_cols)
  r2_early <- mean_over_k(r2_delta, early_ks, r2_d_cols)
  
  dplyr::bind_rows(
    tibble::tibble(Metric = names(se_all),   Window = "All positions (k=1..19)", Value = as.numeric(se_all)),
    tibble::tibble(Metric = names(se_early), Window = "Early positions (k=1..10)", Value = as.numeric(se_early)), # <-- CHANGED label
    tibble::tibble(Metric = names(r2_all),   Window = "All positions (k=1..19)", Value = as.numeric(r2_all)),
    tibble::tibble(Metric = names(r2_early), Window = "Early positions (k=1..10)", Value = as.numeric(r2_early))  # <-- CHANGED label
  )
}

# ---- NEW: Proportion reaching SE threshold by position k (0..19)
# For each model, each threshold T, and each k:
#   Prop_reaching(T,k) = proportion of people with SE(i,k) <= T
threshold_prop_by_position <- function(obj_list,
                                       thresholds = c(0.40, 0.35, 0.30)) {
  
  out <- list()
  
  for (mn in names(obj_list)) {
    label <- obj_list[[mn]]$label
    tse_df <- obj_list[[mn]]$data[[3]]  # rows=people, cols=k=0..19
    
    # Compute prop at each k for each threshold
    for (T in thresholds) {
      # apply over columns: proportion <= T (ignoring NAs)
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

# =========================
# RUN THE SUMMARIES (REAL) + 2DP EXPORTS
# =========================

# Helper: round all numeric columns except specified keys
round_numeric_cols <- function(df, digits = 2, exclude = c("k", "Threshold")) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, exclude)
  df[num_cols] <- lapply(df[num_cols], function(x) round(x, digits))
  df
}

pos_tabs <- build_position_tables(obj_list)
se_tbl <- pos_tabs$se_tbl
r2_tbl <- pos_tabs$r2_tbl

# Delta tables vs Baseline
deltas <- build_delta_tables_vs_baseline(se_tbl, r2_tbl, baseline_label = "Baseline")
se_delta <- deltas$se_delta
r2_delta <- deltas$r2_delta

# -----------------------------
# Round to 2DP
# -----------------------------
se_delta_2dp <- round_numeric_cols(se_delta, digits = 2, exclude = c("k"))
r2_delta_2dp <- round_numeric_cols(r2_delta, digits = 2, exclude = c("k"))

# Save deltas by position (2DP)
readr::write_csv(se_delta_2dp, "output/real_SE_pctReduction_vsBaseline_by_position_2dp.csv")
readr::write_csv(r2_delta_2dp, "output/real_dR2_vsBaseline_by_position_2dp.csv")

# Headline summaries: all positions and early positions (2DP)
headline <- headline_summaries(se_delta, r2_delta, early_ks = 1:10, all_ks = 1:19) %>%  # <-- CHANGED: 1:10
  dplyr::mutate(Value = round(Value, 2))

readr::write_csv(headline, "output/real_headline_SEpct_and_dR2_2dp.csv")

# Threshold attainment as PROPORTION by position + wide table (2DP)
thresh_prop_tbl <- threshold_prop_by_position(obj_list, thresholds = c(0.40, 0.35, 0.30))

thresh_prop_wide <- thresh_prop_tbl %>%
  tidyr::pivot_wider(
    id_cols     = c(Threshold, k),
    names_from  = Model,
    values_from = Prop_reaching
  ) %>%
  dplyr::arrange(Threshold, k) %>%
  # force column order (Baseline first, then Top 5, then All)
  dplyr::relocate(dplyr::any_of(c("Baseline", "Top 5 Texts", "All Texts")), .after = k) %>%
  round_numeric_cols(digits = 2, exclude = c("k", "Threshold"))

readr::write_csv(thresh_prop_wide, "output/real_threshold_prop_by_position_WIDE_2dp.csv")