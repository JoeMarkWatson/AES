library(mirt)
library(mokken)

# # # Take real closed train set. Pur, validate and get item param.s using train set only. Del rel item(s) - if any - from train and test sets.

cdftlm_train = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train.csv')
closed_item_names = paste0('q', seq(1:20), 'p')
data_all = cdftlm_train[closed_item_names]


create_dropped_rows_poly = function(vs_df=data_all) {
  
  vs_df = na.omit(vs_df)
  n_cats = length(unique(vs_df[, 1]))
  grm_cats = paste0('b', c(1:(n_cats-1)))
  dropped_rows = data.frame(matrix(nrow = 0, ncol = 5 + length(grm_cats)))
  colnames(dropped_rows) = c("name", "a", "b", grm_cats, "class", "why_dropped")
  as.character(dropped_rows$class)
  return(dropped_rows)
}


fit_iter_rem = function(ir=data_all, dropped_rows=dropped_rows_df,
                                     disc_cut=0.35, resids_cut=0.5) {
  
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
      item_to_drop$class = ifelse(is.na(item_to_drop$b1), 'dich', 'graded')
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
      item_to_drop$class = ifelse(is.na(item_to_drop$b1), 'dich', 'graded')
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


dropped_rows_df = create_dropped_rows_poly()
fit_iter_rem_out = fit_iter_rem()

fit_iter_rem_out$dropped_rows  # only q16p removed
cdftlm_train$q16p <- NULL
write.csv(cdftlm_train, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv', row.names = F)
write.csv(fit_iter_rem_out$dropped_rows, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_dropped_rows.csv', row.names = F)
write.csv(fit_iter_rem_out$item_coef$items, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_params.csv')


