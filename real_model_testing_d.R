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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# 1. load closed and open items ________________________________________________/
# 2. add GPT scores for essays _________________________________________________/
# 3. add DTMbin values for essays ______________________________________________x leave for a bit
# 4. add DTMcov (continuous) values for essays _________________________________x leave for a bit
# 5. do the temp sim
  # i. load all the models  /x well, just the GPT models for now

# 1. load test essays
closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_test.csv')
closed_items_df$q16p <- NULL
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
data_c = closed_items_df[c('ID', closed_items, 'essays')]

# 2. attach GPT output
# i. zero-shot (which provides the multi scores through GPT1-10, and single score through GPT1 only)
GPT_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/real_scores4mini_d.csv')
GPT_df = GPT_df[GPT_df$rater == 1, ]  # standard (not lenient or severe) rater
GPT_df$rater <- NULL
GPT_items = paste0('GPT', c(1:10))
names(GPT_df) = c(GPT_items, 'ID')
data_all = merge(data_c, GPT_df, by = 'ID')  # retaining only train set rows

# ii. in context
GPT_ic_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/real_scores_incontext_4mini_d.csv')
GPT_ic_df = GPT_ic_df[GPT_ic_df$rater == 1, ]  # standard (not lenient or severe) rater
GPT_ic_df$rater <- NULL
GPT_ic_items = paste0('GPT', c(1:10), 'ic')
names(GPT_ic_df) = c(GPT_ic_items, 'ID')
data_all = merge(data_all, GPT_ic_df, by = 'ID')  # retaining only train set rows

# for DTM approaches, you need to call clean_essays fun


# 5. do the sim
# i. Loading all mod.s, as not poss from the model parameters without any re-run
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/closed_only_mirt_d.RData")  # loads as fitc - the closed only baseline model
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_mirt_d.RData")
fit_multi = fit  # fit multi zero-shot
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_ic_mirt_d.RData")
fit_multi_ic = fit
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_single_mirt_d.RData")
fit_single = fit
rm(fit)

# ii. actual sim, start of a lot of work :)
library(catR)

# (ur output will inc 3 objects: 1 showing the order of items, 2 showing the theta est after each item, 3 showing the theta_SE after each item)
create_obj = function(no_E = F) {
  if (no_E) {
    ncols = 19
    df = data.frame(matrix(nrow=220, ncol=ncols))
    names(df) = paste0('A', c(1:19))
  } else {
    ncols = 20
    df = data.frame(matrix(nrow=220, ncol=ncols))
    names(df) = c('E', paste0('A', c(1:19)))
  }
  return(df)
}

cat_sim = function(fit_obj=fit_multi) {
  
  params = coef(fit_obj, IRTpar=T, simplify=TRUE)
  itembank = params$items
  if ('d1' %in% names(data.frame(itembank))) {
    itembank = itembank[closed_items, c('a1', 'b1', 'b2')]
    GPTi = rownames(data.frame(params$items))[grepl('GPT', rownames(data.frame(params$items)))]
  } else {
    itembank = itembank[rownames(itembank) != 'GPT1', ]
    GPTi = 'GPT1'
  }
  
  q_just_asked_df = create_obj()
  theta_df = create_obj()
  tse_df = create_obj()
  
  for (r in c(1:nrow(data_all))) {
    print(r)
    
    if ('d1' %in% names(data.frame(params$items))) {
      gptm_only_resp_pat = as.vector(unlist(data_all[GPTi][r, ]))
    } else {
      gptm_only_resp_pat = as.vector(unlist(data_all[GPTi][r, 1]))
    }
    
    closed_resps = rep(NA, length(closed_items))
    outvec = c()
    q_just_asked_vec = c()
    theta_vec = c()
    tse_vec = c()
    
    for (i in c(0:19)) {
      gptm_resp_pat = c(closed_resps, gptm_only_resp_pat)
      fso_fm = fscores(fit_obj, response.pattern=gptm_resp_pat)  # factor score obj, fit closed - baseline
      fso_F1 = fso_fm[colnames(fso_fm) == 'F1']
      
      q_just_asked_vec = c(q_just_asked_vec, ifelse(length(outvec)==0, NA, outvec[length(outvec)]))
      theta_vec = c(theta_vec, fso_F1)
      tse_vec = c(tse_vec, fso_fm[colnames(fso_fm) == 'SE_F1'])
      
      if (sum(is.na(closed_resps)) >= 1) {
        fso_ni = nextItem(itemBank = itembank, model = 'GRM', theta = fso_F1, out=outvec)
        fso_niName = fso_ni$name
        fso_niNum = fso_ni$item
        closed_resps[fso_niNum] = data_all[r, fso_niName]
        outvec = c(outvec, fso_niNum)
      }
    }
    
    q_just_asked_df[r, ] = q_just_asked_vec
    theta_df[r, ] = theta_vec
    tse_df[r, ] = tse_vec
    
  }
  return(list(q_just_asked_df, theta_df, tse_df))
}


cat_sim_no_GPT = function(fit_obj=fitc) {
  
  params = coef(fit_obj, IRTpar=T, simplify=TRUE)
  itembank = params$items
  
  q_just_asked_df = create_obj(no_E = T)
  theta_df = create_obj(no_E = T)
  tse_df = create_obj(no_E = T)
  
  for (r in c(1:nrow(data_all))) {
    print(r)
    
    closed_resps = rep(NA, length(closed_items))
    outvec = c()
    q_just_asked_vec = c()
    theta_vec = c()
    tse_vec = c()
    
    fso_F1 = 0  # initial guess
    for (i in c(1:19)) {
      if (sum(is.na(closed_resps)) >= 1) {
        fso_ni = nextItem(itemBank = itembank, model = 'GRM', theta = fso_F1, out=outvec)
        fso_niName = fso_ni$name
        fso_niNum = fso_ni$item
        closed_resps[fso_niNum] = data_all[r, fso_niName]
        outvec = c(outvec, fso_niNum)
      }
      
      fso_fm = fscores(fit_obj, response.pattern=closed_resps)  # factor score obj, fit closed - baseline
      fso_F1 = fso_fm[colnames(fso_fm) == 'F1']
      
      q_just_asked_vec = c(q_just_asked_vec, ifelse(length(outvec)==0, NA, outvec[length(outvec)]))
      theta_vec = c(theta_vec, fso_F1)
      tse_vec = c(tse_vec, fso_fm[colnames(fso_fm) == 'SE_F1'])
      
    }
    
    q_just_asked_df[r, ] = q_just_asked_vec
    theta_df[r, ] = theta_vec
    tse_df[r, ] = tse_vec
    
  }
  return(list(q_just_asked_df, theta_df, tse_df))
}


closed_objs = cat_sim_no_GPT(fit_obj = fitc)
gpts_objs = cat_sim(fit_obj = fit_single)
multi_objs = cat_sim(fit_obj = fit_multi)
multi_ic_objs = cat_sim(fit_obj = fit_multi_ic)








# # # # # # # # # OLD INITIAL CHECK. 

# ii. temp sim, that gives you some confidence that this is going to work
results_df = data.frame(matrix(nrow=0, ncol=11))
values_to_check = c(1, 2, 5, 10, 19)

for (r in c(1:nrow(data_all))) {
  print(r)
  true_theta = NA  # new_essays$ntt[r]
  
  for (i in values_to_check) {  # if after every item, then: "for (i in c(1:19))"
    #print(i)
    
    # # calc baseline model
    closed_resp_pat = data_all[r, c(2:(1+i))]
    num_nas <- length(closed_items) - length(closed_resp_pat)
    closed_resp_pat <- as.vector(unlist(c(closed_resp_pat, rep(NA, num_nas))))  # extend the vector with NAs
    fso_fc = fscores(fitc, response.pattern=closed_resp_pat)  # factor score obj, fit closed - baseline
    
    # # calc gpt single
    gpts_resp_pat = c(closed_resp_pat, data_all$GPT1[r])
    fso_fs = fscores(fit_single, response.pattern=gpts_resp_pat)  # factor score obj, fit closed - baseline
    
    # # calc gpt multi
    gptm_resp_pat = as.vector(c(closed_resp_pat, unlist(data_all[GPT_items][r, ])))
    fso_fm = fscores(fit_multi, response.pattern=gptm_resp_pat)  # factor score obj, fit closed - baseline
    # fso_fm[colnames(fso_fm) == 'F1']  # NOTE - THIS LANG WILL WORK FOR ANY, INC THIS WHICH HAS F2 AND SE_F2 VALUES
    
    # # calc gpt multi ic
    gptic_resp_pat = as.vector(c(closed_resp_pat, unlist(data_all[GPT_ic_items][r, ])))
    fso_fic = fscores(fit_multi_ic, response.pattern=gptic_resp_pat)
    
    new_row = c(r, true_theta, i, 
                fso_fc[1], fso_fs[1], fso_fm[colnames(fso_fm) == 'F1'], fso_fic[colnames(fso_fic) == 'F1'],
                fso_fc[2], fso_fs[2], fso_fm[colnames(fso_fm) == 'SE_F1'], fso_fic[colnames(fso_fic) == 'SE_F1'])
    
    results_df = rbind(results_df, new_row)  # add new_row to bottom of df
  }
  names(results_df) = c('respondent_id', 'true_theta', 'no_closed_items', 
                        'th_closed', 'th_single', 'th_multi', 'th_multi_ic',
                        'se_closed', 'se_single', 'se_multi', 'se_multi_ic')
}


calc_means <- function(df) {
  df %>%
    summarise(
      mean_se_closed = mean(se_closed),
      mean_se_single = mean(se_single),
      mean_se_multi = mean(se_multi),
      mean_se_multi_ic = mean(se_multi_ic)
    )
}

subset1 <- subset(results_df, th_closed < -1)  # Subset where th_closed is between -2 and -1
subset2 <- subset(results_df, th_closed >= -1 & th_closed < 0)  # Subset where th_closed is between -1 and 0
subset3 <- subset(results_df, th_closed >= 0 & th_closed < 1)  # Subset where th_closed is between 0 and 1
subset4 <- subset(results_df, th_closed >= 1)  # Subset where th_closed is between 1 and 2

calc_means(results_df[results_df$no_closed_items ==2, ])
calc_means(results_df[results_df$no_closed_items ==5, ])
calc_means(results_df[results_df$no_closed_items ==10, ])
calc_means(results_df[results_df$no_closed_items ==19, ])

calc_means(subset1[subset1$no_closed_items ==19, ])
calc_means(subset2[subset2$no_closed_items ==19, ])
calc_means(subset3[subset3$no_closed_items ==19, ])
calc_means(subset4[subset4$no_closed_items ==19, ])
calc_means(subset4[subset4$no_closed_items ==10, ])



