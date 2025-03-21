# # # A run for all items (sents and essay)

# # load packages

library(mirt)
library(dplyr)
library(plyr)


# # def fun.s

make_new_rows = function(item_name, n_unique) {
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


output_all_items = function(df, conf_val, fitc=fitc, fitc_params=fitc_params,
                            LLM_items="E", all_GPT_query_versions=T) {
  
  # # initial data manip and set up
  df[df=="False"]<- F
  df[df=="True"]<- T
  
  results_list <- list() 
  theta_range = fscores(fitc)
  
  if (all_GPT_query_versions) {
    com_vals = c("", "com", "_DO", "com_DO")
  } else {
    com_vals = c("")
  }
  
  # # data manip to reflect uncertainty, per LLM
  if (conf_val > 0) {
    
    for (litem in LLM_items) {
      
      for (c in com_vals) {
        
        SxT_col <- paste0(litem, "T", c)
        SxE_col <- paste0(litem, "E", c)
        SxEP_col <- paste0(litem, "EP", c)
        
        if (conf_val >= 50) {  # if required confidence is 50+
          df[[SxT_col]][df[[SxE_col]] == F] <- NA  # remove where most likely resp == F
          if (conf_val > 50) {
            df[[SxT_col]][df[[SxEP_col]] < conf_val] <- NA  # and remove where conf_val falls short
          }
        } else {  # if (conf_val < 50)
          df[[SxT_col]][(df[[SxE_col]] == F) & (df[[SxEP_col]] >= 100-conf_val)] <- NA  # remove only where resp == F is more than N% prob (where N is a number over 50)
        }
        
      }
      
    }
  }
  
  # # make params for each item
  for (l_s in LLM_items) {
    
    for (c in com_vals) {
      
      l = paste0(l_s, "T", c)
      
      new_item_params = data.frame(matrix(nrow=0, ncol=12))
      nunique = nrow(unique(na.omit(df[l])))
      print(nunique)
      
      if (nunique > 2) {
        n_value = nrow(na.omit(df[l]))
        
        new_item_params = rbind(new_item_params, make_new_rows(item_name=l, n_unique = nunique))
        
        all_params = rbind(fitc_params[fitc_params$item != "GROUP", ],
                           new_item_params,
                           fitc_params[fitc_params$item == "GROUP", ])
        all_params$parnum = c(1:nrow(all_params))  # format all_params
        
        fit = mirt(df[c(closed_items, l)], model = 1, 
                   technical=list(NCYCLES=3000), pars=all_params)
        
        coefs <- coef(fit, IRTpar=TRUE, simplify=TRUE)
        item_params = data.frame(coefs$items)
        
        # sum of new testinfo across train set theta range (est using only x items), minus sum of old testinfo across train set theta range
        tinfo_gain = sum(testinfo(fit, theta_range)) - sum(testinfo(fitc, theta_range))
        
        key_item_param = (tail(item_params, 1))  
        key_item_param$tinfo_gain = tinfo_gain
        key_item_param$item = row.names(key_item_param)
        key_item_param$n_value <- n_value
        
        results_list[[l]] <- key_item_param
      }
      
    }
  }
  
  final_results <- do.call(rbind.fill, results_list)
  final_results <- final_results %>%
    mutate(
      b3 = if(!"b3" %in% names(final_results)) NA else b3,
      b4 = if(!"b4" %in% names(final_results)) NA else b4
    )
  
  final_results = final_results[c('a', 'b', 'g', 'u', 'b1', 'b2', 'b3', 'b4', 'n_value', 'tinfo_gain', 'item')]
  
  return(final_results)
}


merge_and_rename <- function(df1, df2) {
  df <- merge(df1, df2, by = "ID")
  names(df) <- LLMdf_names
  df
}


process_LLM_dfs <- function(df) {
  df <- merge(closed_items_df, df, by = "ID")
  df[df == "False"] <- FALSE
  df[df == "True"] <- TRUE
  df
}


alter_data_like_items_to_keep = function(df=all_train, items_to_keep=items_to_keep) {
  # edit the data so that you have NA where conf_val is over 0
  
  df[df=="False"]<- F
  df[df=="True"]<- T
  
  for (litem in items_to_keep$item) {
    conf_val = items_to_keep[items_to_keep$item == litem, ]$conf_val  # conf val for that item
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
  
  df = df %>%  # keep only the columns that you are working with
    select(ID,
           all_of(closed_items),
           all_of(items_to_keep$item))
  
  return(df)
}


add_new_item_params_to_fitc = function(itk=items_to_keep, fixed_closed=fitc, final_df, fitc_params) {
  new_item_params = data.frame(matrix(nrow=0, ncol=12))
  
  for (nis in itk$item) {
    
    nunique = nrow(unique(na.omit(final_df[nis])))
    n_value = nrow(na.omit(final_df[nis]))
    
    new_item_params = rbind(new_item_params, make_new_rows(item_name=nis, n_unique = nunique))
  }
  
  mod2values(fixed_closed)
  
  all_params = rbind(fitc_params[fitc_params$item != "GROUP", ],
                     new_item_params,
                     fitc_params[fitc_params$item == "GROUP", ])
  all_params$parnum = c(1:nrow(all_params))  # format all_params
  
  return(all_params)
}


alter_data_like_items_to_keep = function(df=all_train, items_to_keep=items_to_keep) {
  # edit the data so that you have NA where conf_val is over 0
  
  df[df=="False"]<- F
  df[df=="True"]<- T
  
  for (litem in items_to_keep$item) {
    conf_val = items_to_keep[items_to_keep$item == litem, ]$conf_val  # conf val for that item
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
  
  df = df %>%  # keep only the columns that you are working with
    select(ID,
           all_of(closed_items),
           all_of(items_to_keep$item))
  
  return(df)
}


add_new_item_params_to_fitc = function(itk=items_to_keep, fixed_closed=fitc, final_df, fc_params) {
  new_item_params = data.frame(matrix(nrow=0, ncol=12))
  
  for (nis in itk$item) {
    
    nunique = nrow(unique(na.omit(final_df[nis])))
    n_value = nrow(na.omit(final_df[nis]))
    
    new_item_params = rbind(new_item_params, make_new_rows(item_name=nis, n_unique = nunique))
  }
  
  mod2values(fixed_closed)
  
  all_params = rbind(fc_params[fc_params$item != "GROUP", ],
                     new_item_params,
                     fc_params[fc_params$item == "GROUP", ])
  all_params$parnum = c(1:nrow(all_params))  # format all_params
  
  return(all_params)
}


alter_data_all_params_fit = function(at=all_train, fixed_closed=fitc, 
                                     fcps=fitc_params, itkeep) {
  # itkeep for items to keep
  df_ = alter_data_like_items_to_keep(df=at, items_to_keep=itkeep)
  all_params_ = add_new_item_params_to_fitc(itk=itkeep, final_df=df_, fc_params=fitc_params)
  fit_ = mirt(df_ %>% select(-ID), model = 1, 
              technical=list(NCYCLES=3000), pars=all_params_)
  return(fit_)
}


# # use fun.s

# which_prompt = ('evidence', 'compare', 'either')  # I don't like using this. Instead, I prefer running all. 
# then, through ur dplyr pipe, u choose the items that get used for each model

root = '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/'
file_paths <- list(
  closed_items_df_path = paste0(root, 'cdftlm_train_pur_d2UPDATED.csv'),
  MarksGPT_evidence = paste0(root, 'GPT_sentsOutput_11122024.csv'),
  MarksGPT_compare = paste0(root, 'GPT_sentsOutput2_24122024.csv'),
  MarksGPT_DO_evidence = paste0(root, 'GPT_sentsOutputEvidence_01022025.csv'),
  MarksGPT_DO_compare = paste0(root, 'GPT_sentsOutputCompare_01022025.csv')
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
closed_items_df = datasets$closed_items[c('ID', closed_items)]

# run mirt on closed and 1 LLM-scored item, fixing closed items params
fitc = mirt(closed_items_df[closed_items], model = 1, technical=list(NCYCLES=3000))
fitc_params = mod2values(fitc)
fitc_params$est = F

all_train = merge(closed_items_df, datasets$MarksGPT_evidence, by='ID')
all_train = merge(all_train, datasets$MarksGPT_compare, by='ID')
all_train = merge(all_train, datasets$MarksGPT_DO_evidence, by='ID')
all_train = merge(all_train, datasets$MarksGPT_DO_compare, by='ID')

# Define configurations to loop over
all_outputs = list()
cvs <- c(0, 25, 50, 75)

for (i in c(1:length(cvs))) {
  # Run the function and add an 'llm' and 'conf_val' column to each result
  cv_res <- output_all_items(df=all_train, fitc_params=fitc_params, fitc=fitc,
                             conf_val=cvs[i], 
                             LLM_items=prefixes)
  cv_res$conf_val <- cvs[i]  # Add conf_val as a new column
  
  # Append the result to list
  print(cv_res)  # temp
  all_outputs[[i]] <- cv_res
}


final_output <- do.call(rbind, all_outputs)  # combine all data frames in the list into one
#View(final_output)

# save/load final_output
#my_file_path = 'all_individ_items19022025_d2.csv'
#write.csv(final_output, paste0(root, 'git_repo/output/', my_file_path), row.names = F)
#final_output = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan\ proj/git_repo/output/all_individ_items19022025_d2.csv')

# creating same groupings as for d2SYNTH

# baseline (closed only)
# bsi: best single item from all items (which happens to have noNA)             . 
# bcai_sNA: best combo of items from all items, someNA                          x
# bcai_nNA: best combo of items from all items, noNA                            x
# bcvc_sNA: best combo of items from vary_com, someNA                           x
# bcvc_nNA: best combo of items from vary_com                                   x
# bcve_sNA: best combo of items from vary_evi, someNA                           x
# bcve_nNA: best combo of items from vary_evi                                   x
# bccc_sNA: best combo of items from DOcom, someNA  (same output as bccc_nNA)   x
# bccc_nNA: best combo of items from DOcom (so not saved)                       x
# bcce_sNA: best combo of items from DOevi, someNA (same output as bcce_nNA)    x
# bcce_nNA: best combo of items from DOevi (so not saved)                       x

# bsi
items_to_keep_bsi = final_output %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(!is.na(b4)) %>%
  filter(tinfo_gain == max(tinfo_gain))

# bcai_sNA
items_to_keep_bciai_sNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcai_nNA 
items_to_keep_bciai_nNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(conf_val == 0) %>%  
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcvc_sNA
items_to_keep_bcvc_sNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(!grepl('_DO', item)) %>%
  filter(grepl('com', item)) %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcvc_nNA (giving same output as bcvc_sNA)
items_to_keep_bcvc_nNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(!grepl('_DO', item)) %>%
  filter(grepl('com', item)) %>%
  filter(conf_val == 0) %>%  
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcve_sNA
items_to_keep_bcve_sNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(!grepl('_DO', item)) %>%
  filter(!grepl('com', item)) %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcve_nNA 
items_to_keep_bcve_nNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(!grepl('_DO', item)) %>%
  filter(!grepl('com', item)) %>%
  filter(conf_val == 0) %>%  
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bccc_sNA
items_to_keep_bccc_sNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(grepl('_DO', item)) %>%
  filter(grepl('com', item)) %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bccc_nNA
items_to_keep_bccc_nNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(grepl('_DO', item)) %>%
  filter(grepl('com', item)) %>%
  filter(conf_val == 0) %>%  
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcce_sNA
items_to_keep_bcce_sNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(grepl('_DO', item)) %>%
  filter(!grepl('com', item)) %>%
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# bcce_nNA
items_to_keep_bcce_nNA = final_output %>%
  mutate(item_base = gsub("com|_DO|com_DO", "", item)) %>%
  filter(grepl('_DO', item)) %>%
  filter(!grepl('com', item)) %>%
  filter(conf_val == 0) %>%  
  filter(n_value >= (nrow(closed_items_df) / 4)) %>%  # Drop rows where n_value is less than 1/4 of closed_items_df rows
  filter(a >= 0.3) %>%  # Remove rows where discrimination is below 0.3
  filter(!is.na(b4)) %>%  # Drop rows where b4 is NA
  group_by(item_base) %>%
  filter(tinfo_gain == max(tinfo_gain)) %>% # Select the row with the highest tinfo_gain in each group
  filter(tinfo_gain > 15) %>%  # Keep only those with tinfo_gain > 15
  ungroup()

# save best items, for use in test_script_d2UPDATED
write.csv(items_to_keep_bsi, paste0(root, 'git_repo/output/items_to_keep_bsi_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bciai_sNA, paste0(root, 'git_repo/output/items_to_keep_bciai_sNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bciai_nNA, paste0(root, 'git_repo/output/items_to_keep_bciai_nNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bccc_sNA, paste0(root, 'git_repo/output/items_to_keep_bccc_sNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bccc_nNA, paste0(root, 'git_repo/output/items_to_keep_bccc_nNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bcce_sNA, paste0(root, 'git_repo/output/items_to_keep_bcce_sNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bcce_nNA, paste0(root, 'git_repo/output/items_to_keep_bcce_nNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bcvc_sNA, paste0(root, 'git_repo/output/items_to_keep_bcvc_sNA_REALd2.csv'), row.names = F)
#items_to_keep_bcvc_nNA  # not written, as exactly the same as items_to_keep_bcvc_sNA
write.csv(items_to_keep_bcve_sNA, paste0(root, 'git_repo/output/items_to_keep_bcve_sNA_REALd2.csv'), row.names = F)
write.csv(items_to_keep_bcve_nNA, paste0(root, 'git_repo/output/items_to_keep_bcve_nNA_REALd2.csv'), row.names = F)

# alter data in
fit_bsi = alter_data_all_params_fit(itkeep = items_to_keep_bsi)
fit_bciai_sNA = alter_data_all_params_fit(itkeep = items_to_keep_bciai_sNA)
fit_bciai_nNA = alter_data_all_params_fit(itkeep = items_to_keep_bciai_nNA)
fit_bccc_sNA = alter_data_all_params_fit(itkeep = items_to_keep_bccc_sNA)
fit_bccc_nNA = alter_data_all_params_fit(itkeep = items_to_keep_bccc_nNA)
fit_bcce_sNA = alter_data_all_params_fit(itkeep = items_to_keep_bcce_sNA)
fit_bcce_nNA = alter_data_all_params_fit(itkeep = items_to_keep_bcce_nNA)
fit_bcvc_sNA = alter_data_all_params_fit(itkeep = items_to_keep_bcvc_sNA)
fit_bcve_sNA = alter_data_all_params_fit(itkeep = items_to_keep_bcve_sNA)
fit_bcve_nNA = alter_data_all_params_fit(itkeep = items_to_keep_bcve_nNA)

# check all model resids
data.frame(residuals(fit_bsi, type = "Q3", suppress = 0.2))  # no resids
data.frame(residuals(fit_bciai_sNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.23
data.frame(residuals(fit_bciai_nNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.23
data.frame(residuals(fit_bccc_sNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.241
data.frame(residuals(fit_bccc_nNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.25
data.frame(residuals(fit_bcce_sNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.27
data.frame(residuals(fit_bcce_nNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.27
data.frame(residuals(fit_bcvc_sNA, type = "Q3", suppress = 0.2))  # no resids
data.frame(residuals(fit_bcve_sNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.22
data.frame(residuals(fit_bcve_nNA, type = "Q3", suppress = 0.2))  # no resids exceeding 0.22

# check item coef.s
coef(fit_bsi, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bciai_sNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bciai_nNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bccc_sNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bccc_nNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bcce_sNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bcce_nNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bcvc_sNA, IRTpar=TRUE, simplify=TRUE)$items
#coef(fit_bcvc_nNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bcve_sNA, IRTpar=TRUE, simplify=TRUE)$items
coef(fit_bcve_nNA, IRTpar=TRUE, simplify=TRUE)$items

theta_range = fscores(fitc)

fitc_tinfo = sum(testinfo(fitc, theta_range))  # 19632.6
sum(testinfo(fit_bsi, theta_range)) - fitc_tinfo
sum(testinfo(fit_bciai_sNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bciai_nNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bcvc_sNA, theta_range)) - fitc_tinfo
#sum(testinfo(fit_bcvc_nNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bcve_sNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bcve_nNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bccc_sNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bccc_nNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bcce_sNA, theta_range)) - fitc_tinfo
sum(testinfo(fit_bcce_nNA, theta_range)) - fitc_tinfo

plot(theta_range, testinfo(fitc, theta_range))
plot(theta_range, testinfo(fit_bsi, theta_range))
plot(theta_range, testinfo(fit_bciai_sNA, theta_range))
plot(theta_range, testinfo(fit_bciai_nNA, theta_range))
plot(theta_range, testinfo(fit_bcvc_sNA, theta_range))
#plot(theta_range, testinfo(fit_bcvc_nNA, theta_range))
plot(theta_range, testinfo(fit_bcve_sNA, theta_range))
plot(theta_range, testinfo(fit_bcve_nNA, theta_range))
plot(theta_range, testinfo(fit_bccc_sNA, theta_range))
plot(theta_range, testinfo(fit_bccc_nNA, theta_range))
plot(theta_range, testinfo(fit_bcce_sNA, theta_range))
plot(theta_range, testinfo(fit_bcce_nNA, theta_range))

# save of models
save(fitc, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_closed_d2_real.RData")
save(fit_bsi, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bsi_d2_real.RData")
save(fit_bciai_sNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bciai_sNA_d2_real.RData")
save(fit_bciai_nNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bciai_nNA_d2_real.RData")
save(fit_bcvc_sNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcvc_sNA_d2_real.RData")
#save(fit_bcvc_nNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcvc_nNA_d2_real.RData")
save(fit_bcve_sNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcve_sNA_d2_real.RData")
save(fit_bcve_nNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcve_nNA_d2_real.RData")
save(fit_bccc_sNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bccc_sNA_d2_real.RData")
save(fit_bccc_nNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bccc_nNA_d2_real.RData")
save(fit_bcce_sNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcce_sNA_d2_real.RData")
save(fit_bcce_nNA, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_bcce_nNA_d2_real.RData")

