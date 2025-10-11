# # # A run for "Best All Items" and "Top 5" models using the incremental scores CSV # # #

# # 1. Load packages and Define Functions ----

library(mirt)
library(dplyr)
library(plyr)

# This helper function creates the parameter structure for new graded items.
make_new_rows <- function(item_name, n_unique) {
  data.frame(
    group = "all",
    item = rep(item_name, n_unique),
    class = "graded",
    name = c('a1', paste0('d', c(1:(n_unique-1)))),
    parnum = NA,
    value = c(n_unique:1),
    lbound = -Inf,
    ubound = Inf,
    est = TRUE,
    prior.type = "none",
    prior_1 = NaN,
    prior_2 = NaN
  )
}

# REVISED FUNCTION: Now only returns the performance of ALL candidate items.
find_all_candidate_performance <- function(df, fitc, fitc_params, item_prefixes, item_suffixes) {
  
  all_versions_list <- list() 
  theta_range <- fscores(fitc)
  
  # Loop over each base item (E, SC1, SC2, etc.)
  for (prefix in item_prefixes) {
    # Loop over the four versions (a, b, c, d)
    for (suffix in item_suffixes) {
      
      item_name <- paste0(prefix, suffix)
      if (!item_name %in% names(df)) next
      
      nunique <- length(unique(na.omit(df[[item_name]])))
      n_value <- sum(!is.na(df[[item_name]]))
      
      if (nunique > 2) {
        new_item_params <- make_new_rows(item_name = item_name, n_unique = nunique)
        all_params <- rbind(
          fitc_params[fitc_params$item != "GROUP", ],
          new_item_params,
          fitc_params[fitc_params$item == "GROUP", ]
        )
        all_params$parnum <- 1:nrow(all_params)
        
        fit <- mirt(df[c(closed_items, item_name)], 
                    model = 1,
                    technical = list(NCYCLES = 3000, warn = FALSE), 
                    pars = all_params,
                    verbose = FALSE)
        
        coefs <- coef(fit, IRTpar = TRUE, simplify = TRUE)
        item_params <- data.frame(coefs$items)
        
        tinfo_gain <- sum(testinfo(fit, theta_range)) - sum(testinfo(fitc, theta_range))
        
        key_item_param <- tail(item_params, 1)
        key_item_param$tinfo_gain <- tinfo_gain
        key_item_param$item <- row.names(key_item_param)
        key_item_param$n_value <- n_value
        
        all_versions_list[[item_name]] <- key_item_param
      }
    }
  }
  
  # Prepare the comprehensive output of all versions
  all_versions_df <- do.call(rbind.fill, all_versions_list) %>%
    mutate(
      b3 = if (!"b3" %in% names(.)) NA else b3,
      b4 = if (!"b4" %in% names(.)) NA else b4
    ) %>%
    select(any_of(c('a', 'b', 'g', 'u', 'b1', 'b2', 'b3', 'b4', 'n_value', 'tinfo_gain', 'item')))
  
  # Return the single, comprehensive dataframe
  return(all_versions_df)
}


# # 2. Setup and Data Loading ----

root <- '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/'

# Load the closed items and the new incremental scores CSV
closed_items_df <- read.csv(paste0(root, 'cdftlm_train_pur_d2UPDATED.csv'))
incremental_df <- read.csv(paste0(root, 'DS_sentsOutput_4prompts_incremental_d2.csv'))

# Define item names
closed_items <- paste0('q', c(1:15, 17:20), 'p')
item_prefixes <- c("E", paste0("SC", 1:12))
item_suffixes <- c("a", "b", "c", "d")

# Prepare closed items data
closed_items_df <- closed_items_df[c('ID', closed_items)]

# Merge closed items with the new incremental data
all_train <- merge(closed_items_df, incremental_df, by = "ID")


# # 3. Run Base Model and Calculate Performance for All Candidates ----

# Run mirt on closed items only to get fixed parameters
fitc <- mirt(closed_items_df[closed_items], model = 1, technical = list(NCYCLES = 3000))
fitc_params <- mod2values(fitc)
fitc_params$est <- FALSE # Fix these parameters

# Call the simplified function to get performance of ALL candidates
all_candidate_items_performance <- find_all_candidate_performance(
  df = all_train, 
  fitc = fitc, 
  fitc_params = fitc_params,
  item_prefixes = item_prefixes,
  item_suffixes = item_suffixes)

# --- Inspect the comprehensive output ---
print("Performance of all candidate item versions (for inspection):")
print(all_candidate_items_performance %>% arrange(gsub(".$", "", item)))

# Optional: Save this comprehensive file for later analysis
write.csv(all_candidate_items_performance, 
          paste0(root, "git_repo/output/all_candidate_items_performance.csv"), 
          row.names = FALSE)


# # 4. Derive Best Versions from All Candidates Using New Rules ----

# Create the all_best_versions dataframe from the comprehensive results
all_best_versions <- all_candidate_items_performance %>%
  # NEW RULE 1: Disqualify any candidate that is not sufficiently graded.
  # We require at least 5 response categories (b4 is the 4th threshold).
  filter(!is.na(b4)) %>%
  
  # Create a base item column for grouping (e.g., 'SC1a' -> 'SC1')
  mutate(item_base = gsub(".$", "", item)) %>%
  
  # Group by the base item
  group_by(item_base) %>%
  
  # NEW RULE 2: Within each group of valid candidates, select the one 
  # with the maximum test information gain.
  filter(tinfo_gain == max(tinfo_gain, na.rm = TRUE)) %>%
  
  # Ungroup for subsequent steps
  ungroup()

print("Best performing version selected for each base item (after applying rules):")
print(all_best_versions)


# # 5. Filter for Best Combination of All Items (bciai) ----

# This part of the script continues using the newly derived `all_best_versions` df
items_to_keep_bciai <- all_best_versions %>%
  # Drop rows where the number of responses is less than 1/4 of the total
  filter(n_value >= (nrow(closed_items_df) / 4)) # %>%
  # Remove rows where discrimination is below a threshold
  # filter(a >= 0.3) # %>%
  # Keep only those that provide a meaningful information gain
  # filter(tinfo_gain > 15)

print("Final set of items selected for the 'Best All Items' model:")
print(items_to_keep_bciai)


# # 6. Build, Check, and Save the `fit_bciai` Model ----

# Prepare the final dataframe with only the columns to be used in the model
final_item_names_bciai <- items_to_keep_bciai$item
final_df_bciai <- all_train %>% select(all_of(closed_items), all_of(final_item_names_bciai))

# Generate the parameter specifications for the new items
new_item_params_bciai <- data.frame()
for (item in final_item_names_bciai) {
  nunique <- length(unique(na.omit(final_df_bciai[[item]])))
  new_item_params_bciai <- rbind(new_item_params_bciai, make_new_rows(item_name = item, n_unique = nunique))
}

# Combine fixed parameters for closed items with free parameters for new items
all_final_params_bciai <- rbind(
  fitc_params[fitc_params$item != "GROUP", ],
  new_item_params_bciai,
  fitc_params[fitc_params$item == "GROUP", ]
)
all_final_params_bciai$parnum <- 1:nrow(all_final_params_bciai)

# Fit the final "Best All Items" model
fit_bciai <- mirt(final_df_bciai, 
                  model = 1,
                  technical = list(NCYCLES = 3000), 
                  pars = all_final_params_bciai)

# --- Model Checking and Saving ---

print("Model Coefficients for fit_bciai:")
print(coef(fit_bciai, IRTpar = TRUE, simplify = TRUE)$items)

theta_range <- fscores(fitc)
fitc_tinfo <- sum(testinfo(fitc, theta_range))
bciai_tinfo_gain <- sum(testinfo(fit_bciai, theta_range)) - fitc_tinfo
print(paste("Total Test Information Gain for fit_bciai:", round(bciai_tinfo_gain, 2)))

plot(fit_bciai, type = 'info', main = 'Test Information for Best All Items Model (bciai)')
lines(theta_range, testinfo(fitc, theta_range), col = 'red')
legend("topright", legend = c("fit_bciai", "Closed Items Only"), col = c("black", "red"), lty = 1)

save(fit_bciai, file = paste0(root, "git_repo/output/fit_bciai_incremental.RData"))
print("Successfully created, checked, and saved fit_bciai_incremental.RData")


# # 7. Build and Save an Alternative "Top 5" Efficiency Model ----

# Filter for the top 5 most informative items from the pool of qualified best versions
items_to_keep_top5 <- all_best_versions %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%
  #filter(a >= 0.3) %>%
  arrange(desc(tinfo_gain)) %>%
  slice_head(n = 5)

print("Final set of items selected for the 'Top 5' model:")
print(items_to_keep_top5)

# Build the model only if there are items to add
if (nrow(items_to_keep_top5) > 0) {
  
  # Prepare the final dataframe
  final_item_names_top5 <- items_to_keep_top5$item
  final_df_top5 <- all_train %>% select(all_of(closed_items), all_of(final_item_names_top5))
  
  # Generate parameter specifications
  new_item_params_top5 <- data.frame()
  for (item in final_item_names_top5) {
    nunique <- length(unique(na.omit(final_df_top5[[item]])))
    new_item_params_top5 <- rbind(new_item_params_top5, make_new_rows(item_name = item, n_unique = nunique))
  }
  
  # Combine parameters
  all_final_params_top5 <- rbind(
    fitc_params[fitc_params$item != "GROUP", ],
    new_item_params_top5,
    fitc_params[fitc_params$item == "GROUP", ]
  )
  all_final_params_top5$parnum <- 1:nrow(all_final_params_top5)
  
  # Fit the "Top 5" model
  fit_top5 <- mirt(final_df_top5, 
                   model = 1,
                   technical = list(NCYCLES = 3000), 
                   pars = all_final_params_top5)
  
  # --- Model Checking and Saving ---
  print("Model Coefficients for fit_top5:")
  print(coef(fit_top5, IRTpar = TRUE, simplify = TRUE)$items)
  
  top5_tinfo_gain <- sum(testinfo(fit_top5, theta_range)) - fitc_tinfo
  print(paste("Total Test Information Gain for fit_top5:", round(top5_tinfo_gain, 2)))
  
  plot(fit_top5, type = 'info', main = 'Test Information for Top 5 Model')
  lines(theta_range, testinfo(fitc, theta_range), col = 'red')
  legend("topright", legend = c("fit_top5", "Closed Items Only"), col = c("black", "red"), lty = 1)
  
  save(fit_top5, file = paste0(root, "git_repo/output/fit_top5_incremental.RData"))
  print("Successfully created, checked, and saved fit_top5_incremental.RData")
  
}


# 8. Local Dependence Check (Q3) ---
check_local_dependence <- function(fit_object, model_name) {
  
  print(paste("--- Q3 Local Dependence Check for:", model_name, "---"))
  q3_matrix <- residuals(fit_object, type = 'Q3')
  q3_df <- as.data.frame(as.table(q3_matrix), stringsAsFactors = FALSE)
  names(q3_df) <- c("Item1", "Item2", "Q3")
  
  # Filter out self-comparisons and duplicate pairs, then sort to find the highest values
  top_q3_pairs <- q3_df %>%
    filter(as.character(Item1) < as.character(Item2)) %>%
    arrange(desc(Q3)) %>%
    slice_head(n = 15)
  
  print(paste("Top 15 item pairs with highest Q3 values for", model_name, ":"))
  print(top_q3_pairs)
  cat("\n")
}

check_local_dependence(fit_bciai, "Best All Items (bciai)")
check_local_dependence(fit_top5, "Top 5 Items")
