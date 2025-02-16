library(mirt)
library(mokken)
library(dplyr)

# # # Take all data. Make train and test set. Pur, validate and get item param.s using train set only. Del rel item(s) - if any - from train and test sets.

# # def functions

create_dropped_rows_binary = function(vs_df=data_all) {
  
  vs_df = na.omit(vs_df)
  n_cats = length(unique(vs_df[, 1]))
  grm_cats = paste0('b', c(1:(n_cats-1)))
  dropped_rows = data.frame(matrix(nrow = 0, ncol = 7))
  colnames(dropped_rows) = c("name", "a", "b", "g", "u", "class", "why_dropped")
  as.character(dropped_rows$class)
  return(dropped_rows)
}

fit_iter_rem = function(ir=data_all, dropped_rows=dropped_rows_df,
                        disc_cut=0, resids_cut=0.4) {
  
  fit = mirt(ir, model = 1, technical=list(NCYCLES=3000))
  item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
  #item_coef$items[,1]  # 'a' (or discrim) column of item coefficients
  
  resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
  resids[resids == 1] <- -999  # non NA place holder to facil following max calc
  resids[is.na(resids)] <- -999
  #print(paste0("number of resids over resid_cut pre (further) removal: ", sum(resids > resids_cut)))
  
  while ((min(item_coef$items[,1]) < disc_cut) | (max(resids) > resids_cut)) {
    
    # remove low discrims
    while (min(item_coef$items[,1]) < disc_cut) {
      
      item_to_drop = data.frame(item_coef$items) %>%
        tibble::rownames_to_column(., "name") %>%  # changed from item_name
        filter(a == min(a)) %>%
        slice(1)  # keep only the top row
      item_to_drop$class = ifelse('b1' %in% names(item_to_drop), 'graded', 'dich')
      item_to_drop$why_dropped = ifelse(item_to_drop$a < 0, 'neg discrim', 'low discrim')
      dropped_rows[nrow(dropped_rows)+1, ] = item_to_drop
      
      ir = ir[, !(names(ir) %in% item_to_drop$name)]
      
      fit = mirt(ir, model = 1, technical=list(NCYCLES=3000))  # new model with dropped item removed
      item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
      
      print(paste0("item that was just dropped: ", as.character(item_to_drop$name)))
      print("now its dropped, new no. of items and new min discrim:")
      print(nrow(item_coef$items))
      print(min(item_coef$items[,1]))
      print("____________")
      
    }
    
    if ("neg discrim" %in% dropped_rows$why_dropped | "low discrim" %in% dropped_rows$why_dropped) {  # as you only want to run resids again if you have to
      resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
      resids[resids == 1] <- -999  # non NA place holder to facil following max calc
      resids[is.na(resids)] <- -999  # non NA place holder to facil following max calc
      
    }
    
    # remove high resids
    while (max(resids) > resids_cut) {
      
      print(paste0("number of resids over resid_cut pre removal: ", sum(resids > resids_cut)))
      
      max_dep_items_list = sapply(resids, function(x) row.names(resids)[x == max(resids)])
      max_dep_items_list = do.call(rbind, Filter(function(x) length(x)==1, max_dep_items_list))[,1]  # https://stackoverflow.com/questions/25022511/how-to-subset-a-list-based-on-the-length-of-its-elements-in-r
      q3_2_scores = list()
      i = 1
      for (mdil in max_dep_items_list) {  # listing 2nd highest local dep value for each item
        col = resids[mdil]
        col = col %>%
          tibble::rownames_to_column(., "name")
        col = col[col[, 2] != max(resids), ]
        q3_2 = col[col[, 2] == max(col[, 2]), ][, 2]  # second highest Q3 val for that item
        q3_2_scores[i] = q3_2
        i = i + 1
      }
      if (q3_2_scores[[1]] > q3_2_scores[[2]]) {  # finding which item has highest 2nd highest local dep value
        drop_name = names(max_dep_items_list[1])
        not_drop_name = names(max_dep_items_list[2])
      } else {
        drop_name = names(max_dep_items_list[2])
        not_drop_name = names(max_dep_items_list[1])
      }
      
      # record drop_name info
      item_to_drop = data.frame(item_coef$items) %>%
        tibble::rownames_to_column(., "name") %>%  # changed from 'item_name'
        filter(name == drop_name)
      item_to_drop$class = ifelse('b1' %in% names(item_to_drop), 'graded', 'dich')
      item_to_drop$why_dropped = paste0("local dep with: ", not_drop_name)
      dropped_rows[nrow(dropped_rows)+1, ] = item_to_drop
      
      # drop the drop_name
      ir = ir[, !(names(ir) %in% item_to_drop$name)]  # excluding all dropped items
      
      fit = mirt(ir, model = 1, technical=list(NCYCLES=3000))  # new model with dropped item removed
      item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
      resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
      resids[resids == 1] <- -999  # non NA place holder to facil following max calc
      resids[is.na(resids)] <- -999
      
    }
    
    fit = mirt(ir, model = 1, technical=list(NCYCLES=3000))  # re-run final model
    item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
    
    resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
    resids[resids == 1] <- -999  # non NA place holder to facil following max calc
    resids[is.na(resids)] <- -999
    
  }
  
  return(list(ir=ir, dropped_rows=dropped_rows, resids=resids, item_coef=item_coef, 
              fit=fit))
}


# # use fun.s

#resps = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_d2.csv')  # old file path
resps = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdft_trans4mini_essay_n_sents_d2.csv')

drop_ids = c(20651, 10383)  # both always gave 0 (before any transformation)
resps = resps[!resps$ID %in% drop_ids, ]
closed_item_names = paste0('q', seq(1:20), 'p')

# make train and test sets
train_size = round(2*(nrow(resps)/3), 0)
set.seed(0)
train_ids = sample(resps$ID, train_size, replace = F)  # randomly sample without replacement

cdftlm_train = resps[resps$ID %in% train_ids, ]
cdftlm_test = resps[!resps$ID %in% train_ids, ]

# get all train-set data for x item validation
data_all = cdftlm_train[closed_item_names]

dropped_rows_df = create_dropped_rows_binary()
fit_iter_rem_out = fit_iter_rem()

fit_iter_rem_out$dropped_rows  # only q16p removed
cdftlm_train$q16p <- NULL

# store params made from train set
write.csv(fit_iter_rem_out$item_coef$items, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_params_d2UPDATED.csv')
# store info on removed item(s)
write.csv(fit_iter_rem_out$dropped_rows, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_dropped_rows_d2UPDATED.csv', row.names = F)
# store purified train set test
write.csv(cdftlm_train, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur_d2UPDATED.csv', row.names = F)
# store test set
write.csv(cdftlm_test, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_test_d2UPDATED.csv', row.names = F)

