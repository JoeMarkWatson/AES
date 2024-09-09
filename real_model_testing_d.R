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


# 5. do the temp sim
# i. Loading all mod.s, as not poss from the model parameters without any re-run
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/closed_only_mirt_d.RData")  # loads as fitc - the closed only baseline model
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_mirt_d.RData")
fit_multi = fit  # fit multi zero-shot
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_ic_mirt_d.RData")
fit_multi_ic = fit
load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_single_mirt_d.RData")
fit_single = fit
rm(fit)

# ii. 
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



params = coef(fit_multi, IRTpar=T, simplify=TRUE)
itembank = params$items
itembank = itembank[c(1:19), c(1, 3, 4)]
library(catR)
nextItem(itemBank = itembank, model = 'GRM', theta = -0.3)
