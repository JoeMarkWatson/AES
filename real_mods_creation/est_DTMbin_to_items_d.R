#est_DTMbin_to_items_d.R

# Estimate params for DTM-binary to items (bi-factor) model

library(catR)
library(dplyr)
library(stringr)
library(mirt)
library(psych)
library(tm)
library(tidytext)
library(mokken)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# 1. load data, retaining only train set info ___________________________________________________/
# 2. make dtm of stemmed words __________________________________________________________________/
# 3. remove v sparse or common words ____________________________________________________________/
# 4. change to binary ___________________________________________________________________________/
# 5. inc these 'dtm cols' as new items __________________________________________________________/
# 6. reverse score any dtm cols items that need it ______________________________________________/
# 7. fit mirt on just closed items, saving the parameters _______________________________________/
# 8. fit mirt on closed and dtm items, fixing closed items params (semi-fixed)___________________/
# 9. do item purification (while still param fixing)_____________________________________________/
# 10. save the param.s of semi-fixed model ______________________________________________________/
# 12. test and item info ________________________________________________________________________/


# i. functions

clean_text <- function(text) {
  # basic manip using baseR
  text <- tolower(text)  # Convert to lowercase
  text <- gsub("<.*?>", "", text)  # Remove HTML tags (e.g., <br>, <p>, etc.)
  text <- gsub("[[:punct:]]", "", text)  # Remove punctuation and other non-alphabetic characters
  #text <- gsub("\\s+", " ", text)  # Remove extra spaces
  
  # remove numbers, stopwords and stem using tm package
  text = tm::removeNumbers(text)
  #text = tm::removeWords(text, stopwords("en"))
  #text = tm::stemDocument(text)
  
  return(text)
}


make_vals_df = function(my_dtm=dtm, rsi=reverse_scored_items, cat_thresh_val=50) {
  # Linacre 2002: https://www.winsteps.com/a/Linacre-optimizing-category.pdf suggests abs min of 10 for poly, 25 or 50 min for dicho
  
  # fun to make vals table for scoring of new items. Can support poly items, even though in this script being applied for just binary
  
  vals_df = data.frame(matrix(nrow=0, ncol=4))
  for (col in names(my_dtm)) {
    vec = my_dtm[[col]]
    
    freq_table = table(vec)
    cats_under_thresh <- names(freq_table)[freq_table < cat_thresh_val]
    
    merge_from_list = c()
    while (length(cats_under_thresh) > 0) {
      merge_from = as.numeric(rev(cats_under_thresh)[1])
      if (merge_from > mean(vec)) {
        merge_to = max(sort(unique(vec))[sort(unique(vec)) != as.numeric(merge_from)])
      } else {
        merge_to = min(sort(unique(vec))[sort(unique(vec)) != as.numeric(merge_from)])
      }
      vec[vec == as.numeric(merge_from)] = as.numeric(merge_to)
      freq_table = table(vec)
      cats_under_thresh <- names(freq_table)[freq_table < cat_thresh_val]
    }
    
    vals = unique(sort(vec))
    needs_rev = ifelse(col %in% rsi, 1, 0)
    for (v in seq_along(vals)) {
      vals_df = rbind(vals_df, c(col, v-1, vals[v], needs_rev))
    }
  }
  names(vals_df) = c('dtm_item', 'cat_no', 'val', 'needs_rev')
  
  # remove any dtm_items that now have only 1 resp category
  freq_table = table(vals_df$dtm_item)
  less_than_twice <- names(freq_table)[freq_table < 2]
  vals_df <- vals_df[!vals_df$dtm_item %in% less_than_twice, ]
  
  return(vals_df)
}


make_new_rows = function(item_name) {
  
  data.frame(
    group = "all",
    item = item_name,
    class = "dich",
    name = c('a1', 'a2', 'd', 'g', 'u'),
    parnum = NA,
    value = c(0.5, 0.5, 0.5, 0, 1),
    lbound = c(-Inf, -Inf, -Inf, 0, 0),
    ubound = c(Inf, Inf, Inf, 1, 1),
    est = c(T, T, T, F, F),
    prior.type = "none",
    prior_1 = NaN,
    prior_2 = NaN
  )
}


create_dropped_rows_df = function() {
  dropped_rows = data.frame(matrix(nrow = 0, ncol = 10))
  colnames(dropped_rows) = c("name", "a1", "a2", "d1", "d2", "d", "g", "u", "class", "why_dropped")
  return(dropped_rows)
}


fit_iter_rem_fixed_params = function(ir=data_all[, names(data_all) %in% all_params$item], dropped_rows=dropped_rows_df,
                                     disc_cut=0.3, resids_cut=0.5, params=all_params, mono_check=T) {
  
  mod_string = paste0("F1=1-", ncol(ir), "\nF2=", length(closed_items)+1, "-", ncol(ir))
  fit = mirt(ir, model = mod_string, pars=params, technical=list(NCYCLES=3000))
  item_coef <- coef(fit, IRTpar=F, simplify=TRUE)
  
  resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
  resids[resids == 1] <- -999  # non NA place holder to facil following max calc
  resids[is.na(resids)] <- -999
  #print(paste0("number of resids over resid_cut pre (further) removal: ", sum(resids > resids_cut)))
  
  while ((min(item_coef$items[,1]) < disc_cut) | (max(resids) > resids_cut) | (mono_check)) {  # [, 1] is loading on primary factor
    
    # remove low discrims
    while (min(item_coef$items[,1]) < disc_cut) {
      
      item_to_drop = data.frame(item_coef$items) %>%
        tibble::rownames_to_column(., "name") %>%  # changed from item_name
        filter(a1 == min(a1)) %>%
        slice(1)  # keep only the top row
      item_to_drop$class = ifelse(is.na(item_to_drop$d1), 'dich', 'graded')
      item_to_drop$why_dropped = ifelse(item_to_drop$a1 < 0, 'neg discrim', 'low discrim')
      dropped_rows[nrow(dropped_rows)+1, ] = item_to_drop
      
      ir = ir[, !(names(ir) %in% item_to_drop$name)]
      params = params[!(params$item %in% dropped_rows$name), ]  # removing any item in params that is in dropped_row
      params$parnum = c(1:nrow(params))  # format all_params
      
      mod_string = paste0("F1=1-", ncol(ir), "\nF2=", length(closed_items)+1, "-", ncol(ir))
      fit = mirt(ir, model = mod_string, technical=list(NCYCLES=3000), pars = params)  # new model with dropped item removed
      item_coef <- coef(fit, IRTpar=F, simplify=TRUE)
      
      print(paste0("item that was just dropped: ", as.character(item_to_drop$name)))
      print("now its dropped, new no. of items and new min discrim (for primary factor):")
      print(nrow(item_coef$items))
      print(min(item_coef$items[,1]))
      print("____________")
      
      mono_check=T
    }
    
    if ("neg discrim" %in% dropped_rows$why_dropped | "low discrim" %in% dropped_rows$why_dropped) {  # as you only want to run resids again if you have to
      resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
      resids[resids == 1] <- -999  # non NA place holder to facil following max calc
      resids[is.na(resids)] <- -999  # non NA place holder to facil following max calc
      
    }
    
    # # Check resids
    
    # no high resids
    print(paste0("max resids: ", max(resids)))
    print(paste0("number of resids over resid_cut pre removal: ", sum(resids > resids_cut)))
    
    # # Check monotonicity
    if (mono_check) {
      
      # to remove, checking df containing only 1 dtm item at a time (to keep comparison against the pre-existing closed item test)
      not_X = sum(!grepl('^q[0-9]+', names(ir)))
      is_X = sum(grepl('^q[0-9]+', names(ir)))
      nonmono_items = c()
      for (k in c(1:not_X)) {
        mono_check_df = ir[, c(grep('^q[0-9]+', names(ir)), (is_X + k))]
        mono_check_obj = check.monotonicity(mono_check_df, minsize = floor(nrow(ir)/4)-7)
        mono_check_sum = data.frame(summary(mono_check_obj))
        names(mono_check_sum) = c('ItemH', 'ac', 'vi', 'vi_ac', 'maxvi', 'sum', 'sum_ac', 'zmax', 'zsig', 'crit')
        nonmono_items = c(nonmono_items, rownames(mono_check_sum[mono_check_sum$zsig > 0, ]))
        print(nonmono_items)
      }
      nonmono_items = unique(nonmono_items)  # in case some X item was always being listed
      nonmono_items = nonmono_items[nonmono_items %in% names(ir[!grepl('^q[0-9]+', names(ir))])]  # consider potential non-monotonicity among dtm items
      
      if (length(nonmono_items) == 0) {
        print("zero non-monotone dtm items")
      } else {
        print("non-monotone dtm items. Edit function to remove.")
      } 
      
      mono_check=F
      
    }
    
  }
  
  return(list(ir=ir, dropped_rows=dropped_rows, resids=resids, item_coef=item_coef, 
              fit=fit))
}




# code

closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv')
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
data = closed_items_df[c('ID', closed_items, 'essays')]
data$essays = clean_text(data$essays)

s <- SimpleCorpus(VectorSource(unlist(lapply(data$essays, as.character))))
dobj = DocumentTermMatrix(s, 
                          control = list(stopwords=T,
                                         stemming=T,
                                         removeNumbers=T))
dtm = as.data.frame(as.matrix(dobj))  # terms used, total (after stemming, removing stopwords and numbers)
binary_dtm = ifelse(dtm>0, 1, 0)  # 1 if anything over 0
binary_dtm = as.data.frame(binary_dtm)
zero_prop <- colMeans(binary_dtm == 0)  # for binary_dtm, get rid of v sparse/non-sparse cols
binary_dtm <- binary_dtm[, !(zero_prop <= 50/nrow(binary_dtm) | zero_prop >= (nrow(binary_dtm)-50)/nrow(binary_dtm))]  # filter out too sparse/common
# Linacre 2002: https://www.winsteps.com/a/Linacre-optimizing-category.pdf suggests abs min of 10 for poly, 25 or 50 min for dicho

item_total_cor <- apply(binary_dtm, 2, function(item) {  
  cor(item, rowSums(data[, closed_items]), use = "complete.obs")  
})
reverse_scored_items <- names(which(item_total_cor < 0))
vals_df = make_vals_df(my_dtm = binary_dtm, rsi=reverse_scored_items)  # cat_no and val are near-enough redundant when
# working with this binary dtm (as consistent across all dtm_item.s)

data_all = cbind(data[, closed_items], binary_dtm)
for (col in reverse_scored_items) {
  data_all[, col] <- 1 - data_all[, col]  # reverse the scoring for the column
  colnames(data_all)[colnames(data_all)==col] <- paste0(colnames(data_all)[colnames(data_all)==col], "_reversed")
}

# 2. fit mirt on just closed items, saving the parameters

fitc = mirt(data_all[closed_items], model = 1, technical=list(NCYCLES=3000))
fitc_params = mod2values(fitc)
fitc_params$est = F

# 3. run mirt on closed and DTMbin items, fixing closed items params (semi-fixed)

# making new partially-to-be-estimated params
up = fitc_params  # up for updated_params
a_rows = up %>% filter(name == "a1") %>%
  mutate(name = "a2", 
         value = 0)
up <- up %>%
  bind_rows(a_rows) %>%
  arrange(parnum) %>% 
  filter(item != "GROUP")

group_rows <- data.frame(
  group = "all",
  item = "GROUP",
  class = "GroupPars",
  name = c("MEAN_1", "MEAN_2", "COV_11", "COV_21", "COV_22"),
  parnum = c(101:105),
  value = c(0, 0, 1, 0, 1),
  lbound = c(-Inf, -Inf, 0.0001, -Inf, 0.0001),
  ubound = Inf,
  est = F,
  prior.type = "none",
  prior_1 = NaN,
  prior_2 = NaN
)

DTM_items = names(data_all[, !(names(data_all) %in% closed_items)])
new_item_params = data.frame(matrix(nrow=0, ncol=12))
for (k in seq(DTM_items)) {
  nunique = nrow(unique(data_all[DTM_items[k]]))
  new_item_params = rbind(new_item_params, make_new_rows(item_name=DTM_items[k]))
}

all_params = rbind(up,
                   new_item_params,
                   group_rows)
all_params$parnum = c(1:nrow(all_params))  # format all_params

n_qs = length(unique(all_params$item))-1

mod_string = paste0("F1=1-", n_qs, "\nF2=", length(closed_items)+1, "-", n_qs)

# fit mirt on closed and dtm items, fixing closed items params (semi-fixed)
# do item purification (while still param fixing)

dropped_rows_df=create_dropped_rows_df()

final_output = fit_iter_rem_fixed_params()


# 10. save the param.s of semi-fixed model
dropped_out = final_output$dropped_rows
item_params = data.frame(final_output$item_coef$items)
fit = final_output$fit
names_out = names(final_output$ir)
names_out_nr = gsub("_reversed", "", names_out)
names_out_nr = names_out_nr[!grepl('^q[0-9]', names_out_nr)]
vals_df_out = vals_df[vals_df$dtm_item %in% names_out_nr, ]  # to be used when working with new/test set data

write.csv(dropped_out, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTMbin_droppedRows_d.csv", row.names = F)
write.csv(vals_df_out, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTMbin_vals_df_kept_d.csv", row.names = F)
write.csv(item_params, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTMbin_ItemParams_d.csv", row.names = T)
save(fit, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTMbin_mirt_d.RData")


# 11. test and item info
Theta <- matrix(seq(-4, 4, length.out = 100))  # can by -10, 10 if wanting to see fuller pattern
Theta2 <- as.matrix(expand.grid(Theta, rep(0, 100)))  # by setting theta2 to 0, when calc.ing test info you therefore multiple a2 by 0 (so only considering theta1)
tinfo <- testinfo(fit, Theta2, degrees = c(0,90))  # https://groups.google.com/g/mirt-package/c/RaSDKV0MPS4?pli=1 this drops the second dimension, and only considers the first
# info.2[c(1, 101, 201, 301, 401, 501, 1001, 2001, 5001, 9001)]  # showing that the values repeat every 100
# info.2[c(5, 105, 205, 305, 405, 505, 1005, 2005, 5005, 9005)]  # showing that the values repeat every 100
total_info = tinfo[c(1:100)]
plot(Theta, total_info, type='l')

# info with only closed items
total_info_c = testinfo(fitc, Theta)
plot(Theta, total_info_c, type='l')

# of GPT items only
plot(Theta, total_info - total_info_c, type='l')  # if -10, 10 when making Theta then shows curve with peak at approx 4 

# shows only slight gain from including DTM items, most pronounced at high (~4) theta levels.



