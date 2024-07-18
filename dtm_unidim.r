# dtm unidim model

# TODO: Work from your old laptop: there, you did not get rid of sparsity. You kept
# everything possible, and just got rid of things based on response cat freq.s.

# So here, you're just making some drafting updates for make_vals_df only. 


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

# 1. load item bank - the one u already saved ___________________________________________________/
# 2. make dtm of stemmed words __________________________________________________________________/
# 3. remove v sparse terms (.95) ________________________________________________________________/
# 4. change to binary or poly, depending on what data permits ___________________________________/
# 5. inc these 'dtm cols' as new items __________________________________________________________/
# 6. reverse score any dtm cols items that need it ______________________________________________/
# 7. run mirt on just closed items, saving the parameters _______________________________________/
# 8. run mirt on closed and dtm items, fixing closed items params _______________________________/
# 9. do item purification (which is gonna take a load of work cos of param fixing)_______________/
# 10. do some sense checks ______________________________________________________________________/
# - item info of the dtm items __________________________________________________________________/
# - test info with/without the dtm items ________________________________________________________/


# load closed items
data = read.csv('output/closed_responses_train.csv')

clean_text <- function(text) {
  # basic manip using baseR
  text <- tolower(text)  # Convert to lowercase
  text <- gsub("<.*?>", "", text)  # Remove HTML tags (e.g., <br>, <p>, etc.)
  text <- gsub("[[:punct:]]", "", text)  # Remove punctuation and other non-alphabetic characters
  #text <- gsub("\\s+", " ", text)  # Remove extra spaces
  
  # remove numbers, stopwords and stem using tm package
  text = tm::removeNumbers(text)
  #text = tm::removeWords(text, stopwords("en"))
  text = tm::stemDocument(text)
  
  return(text)
}

# TODO: make this stemming, removing numbers match other scripts
# make dtm
sents <- data$essays
s <- Corpus(VectorSource(unlist(lapply(sents, clean_text))))
dobj = DocumentTermMatrix(s)  # this puts everything to lower

dtm = as.data.frame(as.matrix(dobj))
#dobjr = removeSparseTerms(x=dobj, sparse=0.9)
#dtm = as.data.frame(as.matrix(dobjr))

item_total_cor <- apply(dtm, 2, function(item) {  # only investigating  
  cor(item, rowSums(data[, grep("^X", names(data))]), use = "complete.obs")
})
reverse_scored_items <- names(which(item_total_cor < 0))

# convert dtm into response cat.s
#binary_dtm = ifelse(dtm>0, 1, 0)  # 1 if anything over 0
#binary_dtm = as.data.frame(binary_dtm)
# the above works (if later reversing cols), but you miss out on too much info

# so, you're converting into a poly dtm wherever possible
make_vals_df = function(my_dtm=dtm, rsi=reverse_scored_items, cat_thresh_val=50) {
  # Linacre 2002: https://www.winsteps.com/a/Linacre-optimizing-category.pdf suggests abs min of 10 for poly, 25 or 50 min for dicho
  
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


make_poly_dtm = function(poly_dtm=dtm, vdf=vals_df) {  # used in model_comparison_joe.R and dtm_unidim_joe.R
  
  poly_dtm = poly_dtm[, names(poly_dtm) %in% unique(vdf$dtm_item)]  # subset poly_dtm so that contains only items in vals_df
  
  for (col in names(poly_dtm)) {  # for each col in poly_dtm:
    
    vdfs = vdf[vdf$dtm_item == col, ]  # subset vdf, choosing rows only where dtm_item == colname
    for (d in seq_along(poly_dtm[[col]])) {  # for each cell in col
      
      cv = poly_dtm[[col]][d]  # cv for cell_value
      if (cv > max(vdfs$val)) {  # meaning (future) value is higher than any prev encountered
        min_val_row <- tail(vdfs, 1)
      } else {  # if value is within or lower than prev encountered range
        min_val_row <- vdfs[vdfs$val >= cv, ]
      }
      max_val_cat_no <- min_val_row$cat_no[which.min(min_val_row$val)]  # cat no of lowest val in vdf that the cell is less than or equal to
      
      poly_dtm[[col]][d] = max_val_cat_no  # replace poly_dtm value
      
    }
    if (vdfs$needs_rev[1] == '1') {
      poly_dtm[col] = lapply(poly_dtm[col], as.numeric)
      poly_dtm[col] = max(as.numeric(vdfs$cat_no)) - poly_dtm[col]  # reversing when needed
      col_index <- which(names(poly_dtm) %in% col)  # and renaming
      names(poly_dtm)[col_index] <- paste0(col, "_reversed")
    }
  }
  
  poly_dtm[] <- lapply(poly_dtm, as.numeric)
  poly_dtm = poly_dtm[, sort(names(poly_dtm))]
  return(poly_dtm)
}


traditional2mirt_list <- function(v_df = vals_df) {
  # turns all dtm items (in vals_df) to a parameters list for use in mirt

  adip = data.frame(matrix(nrow = 0, ncol = 12))  # all_dtm_items_params
  names(adip) = c('group', 'item', 'class', 'name', 'parnum', 'value', 'lbound', 'ubound', 'est',
                  'prior.type', 'prior_1', 'prior_2')

  for (a in unique(v_df$dtm_item)) {
    vd = v_df[v_df$dtm_item == a, ]
    k = nrow(vd)
    i = k-1  # i giving the number of line meeting points

    if (k == 2) {
      vec <- c(a=0.5, b=0, g=0, u=1)  # setting initial discrim guess of .5 and diff guess of 0, both later freely estimated
      dtm_item_rows = data.frame(t(data.frame(traditional2mirt(vec, '2PL', ncat=i))))
      rownames(dtm_item_rows) = NULL
      long_format <- tidyr::pivot_longer(dtm_item_rows, everything(), names_to = "name", values_to = "value")
      long_format$class = "dich"
      long_format$lbound = c(-Inf, -Inf, 0, 0)
      long_format$ubound = c(Inf, Inf, 0, 0)
      long_format$est = c(T, T, F, F)
    } else {
      vec <- c(a=0.5, setNames(seq(-(i-1)/2, (i-1)/2, by=1), paste0("b", 1:(i))))  # set discrim at 1, arbitrarily, it gets freely est later
      dtm_item_rows = data.frame(t(data.frame(traditional2mirt(vec, 'graded', ncat=k))))
      rownames(dtm_item_rows) = NULL
      long_format <- tidyr::pivot_longer(dtm_item_rows, everything(), names_to = "name", values_to = "value")
      long_format$class = "graded"
      long_format$lbound = -Inf
      long_format$ubound = Inf
      long_format$est = T
      #c(T, rep(F, (nrow(long_format)-(k))), rep(T, k-1))  # freely est.ing discrim and diff.s (bar d0)  # if class = 'gpcm'

    }
    long_format$prior.type = 'none'
    long_format$prior_1 = NaN
    long_format$prior_2 = NaN
    long_format$parnum = NA
    long_format$group = "all"
    long_format$item = ifelse(vd$needs_rev[1] == '1', paste0(a, "_reversed"), a)
    
    long_format <- long_format %>%
      dplyr::select(group, item, class, name, parnum, value, lbound, ubound, est, prior.type, prior_1, prior_2)
    adip = rbind(adip, long_format)

  }
  return(adip[order(adip$item),])

}


create_dropped_rows_df = function(vs_df=vals_df) {
  # modded from standard fun, to allow application to grm items
  max_no_cats_min_1 = max(table(vs_df$dtm_item))-1
  grm_cats = paste0('b', c(1:4))

  dropped_rows = data.frame(matrix(nrow = 0, ncol = 7 + length(grm_cats)))
  colnames(dropped_rows) = c("name", "a", "b", "g", "u", grm_cats, "class", "why_dropped")
  as.character(dropped_rows$class)
  return(dropped_rows)
}

fit_iter_rem_fixed_params = function(ir=data_all[, names(data_all) %in% all_params$item], dropped_rows=dropped_rows_df,
                                     disc_cut=0.3, resids_cut=0.5, params=all_params, mono_check=T) {
  as.character(dropped_rows$class)
  fit = mirt(ir, model = 1, technical=list(NCYCLES=3000), pars=params)
  item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
  #item_coef$items[,1]  # 'a' (or discrim) column of item coefficients
  
  resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
  resids[resids == 1] <- -999  # non NA place holder to facil following max calc
  resids[is.na(resids)] <- -999
  #print(paste0("number of resids over resid_cut pre (further) removal: ", sum(resids > resids_cut)))
  
  while ((min(item_coef$items[,1]) < disc_cut) | (max(resids) > resids_cut) | (mono_check)) {

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
      params = params[!(params$item %in% dropped_rows$name), ]  # removing any item in params that is in dropped_row
      params$parnum = c(1:nrow(params))  # format all_params

      fit = mirt(ir, model = 1, technical=list(NCYCLES=3000), pars = params)  # new model with dropped item removed
      item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)

      print(paste0("item that was just dropped: ", as.character(item_to_drop$name)))
      print("now its dropped, new no. of items and new min discrim:")
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
      params = params[!(params$item %in% dropped_rows$name), ]  # removing any item in params that is in dropped_row
      params$parnum = c(1:nrow(params))  # format all_params

      fit = mirt(ir, model = 1, technical=list(NCYCLES=3000), pars = params)  # new model with dropped item removed
      item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
      resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
      resids[resids == 1] <- -999  # non NA place holder to facil following max calc
      resids[is.na(resids)] <- -999
      
      mono_check=T
    }
      
    if(mono_check) {
      
      # to remove, checking df containing only 1 dtm item at a time (to keep comparison against the pre-existing closed item test)
      not_X = sum(!grepl('^X', names(ir)))
      is_X = sum(grepl('^X', names(ir)))
      nonmono_items = c()
      for (k in c(1:not_X)) {
        mono_check_df = ir[, c(grep('^X', names(ir)), (is_X + k))]
        mono_check_obj = check.monotonicity(mono_check_df, minsize = floor(nrow(ir)/4)-7)
        mono_check_sum = data.frame(summary(mono_check_obj))
        names(mono_check_sum) = c('ItemH', 'ac', 'vi', 'vi_ac', 'maxvi', 'sum', 'sum_ac', 'zmax', 'zsig', 'crit')
        nonmono_items = c(nonmono_items, rownames(mono_check_sum[mono_check_sum$zsig > 0, ]))
        print(nonmono_items)
      }
      nonmono_items = unique(nonmono_items)  # in case some X item was always being listed
      
      if (length(nonmono_items) > 0) {
        item_to_drop = data.frame(item_coef$items) %>%
          tibble::rownames_to_column(., "name") %>%
          filter(name %in% nonmono_items)
        item_to_drop$class = ifelse(is.na(item_to_drop$b1), 'dich', 'graded')
        item_to_drop$why_dropped = 'not mono'
        print("here 0")
        print(head(dropped_rows))
        print(head(item_to_drop))
        dropped_rows = bind_rows(dropped_rows, item_to_drop) # rbind doesn't work here
        # item_to_drop has an extra column (b5) compared to dropped_rows
      }
      
      ir = ir[, !(names(ir) %in% dropped_rows$name)]
      mono_check=F
      
    }
    
    params = params[!(params$item %in% dropped_rows$name), ]  # removing any item in params that is in dropped_row
    params$parnum = c(1:nrow(params))  # format all_params
    fit = mirt(ir, model = 1, technical=list(NCYCLES=3000), pars = params)  # new model with dropped item removed
    
    item_coef <- coef(fit, IRTpar=TRUE, simplify=TRUE)
    
    resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
    resids[resids == 1] <- -999  # non NA place holder to facil following max calc
    resids[is.na(resids)] <- -999

  }

  return(list(ir=ir, dropped_rows=dropped_rows, resids=resids, item_coef=item_coef, 
              fit=fit))
}

vals_df = make_vals_df(dtm)
poly_dtm = make_poly_dtm(dtm, vals_df)
data_all = cbind(data, poly_dtm) # check!!

## confirm items successfully reverse scored
#item_total_cor <- apply(data_all, 2, function(item) {
#  cor(item, rowSums(data), use = "complete.obs")
#})
#items_needing_reversing <- which(item_total_cor < 0)

# 7. run mirt on closed items only, saving the parameters
fit_closed = mirt(data[, grep("^X", names(data))], model = 1, technical=list(NCYCLES=3000))
#save(fit_closed, file = "output/all_params/closed_only_mirt.RData")  # saving closed model for use as baseline

parameters = mod2values(fit_closed)
parameters$est = F  # fix all params for closed items

# extend traditional2mirt so that it inc.s all new dtm items
new_dtm_params = traditional2mirt_list(v_df = vals_df)
all_params = rbind(parameters[parameters$item != "GROUP", ],
                   new_dtm_params,
                   parameters[parameters$item == "GROUP", ])
all_params$parnum = c(1:nrow(all_params))  # format all_params

# 8. run mirt on closed and dtm items, using fixed closed items params
# 9. do item purification, always accounting for param fixing
dropped_rows_df=create_dropped_rows_df()
final_output = fit_iter_rem_fixed_params()
#save.image(file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/workspace04052024.RData")
#save.image(file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/workspace06042024.RData")
#load(file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/workspace04052024.RData")

# saving model
out_fit = final_output$fit
#save(out_fit, file = "output/all_params/dtm_unidim_mirt.RData")
# saving params
fit_params = mod2values(final_output$fit)
write.csv(fit_params, 'output/dtm_unidim_params.csv', row.names = F)

# examining output
out_dropped = final_output$dropped_rows
out_ir = final_output$ir
out_item_params = data.frame(final_output$item_coef$items)
out_params = mod2values(final_output$fit)

# test info with/without the dtm items
Theta <- matrix(seq(-4,4,.01))
tinfo_conly = testinfo(fit_closed, Theta)
tinfo_all = testinfo(final_output$fit, Theta)
plot(Theta, tinfo_conly, type='l')
plot(Theta, tinfo_all, type='l')  # info goes up

# item info of the dtm items only
dtm_item_names <- rownames(out_item_params)[!grepl("^X\\d", rownames(out_item_params))]
tinfo_DTMonly = 0
for (nam in dtm_item_names) {
  tinfo_DTMonly = tinfo_DTMonly + iteminfo(extract.item(final_output$fit, nam), Theta)
}
plot(Theta, tinfo_DTMonly, type='l')

dtmdf = data.frame(matrix(nrow=length(dtm_item_names), ncol=0))
dtmdf$dtm_item_names = dtm_item_names
dtmdf$dtm_item_names_orig = gsub('_reversed', '', dtm_item_names)
vals_df_kept = vals_df[vals_df$dtm_item %in% dtmdf$dtm_item_names_orig, ]
write.csv(vals_df_kept, file='output/vals_df_kept.csv', row.names = F)
