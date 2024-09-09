# est_GPT_multi_to_covars_d

library(catR)
library(dplyr)
library(stringr)
library(mirt)
library(psych)
library(tm)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# TO DO 
# "-" is yet to start; "." is in progress; "/" is done.

# 1. load data, retaining only train set info ___________________________________________________/
# 2. fit mirt on closed and gpt multi (not fixed) _______________________________________________/
# 3. save the param.s of model __________________________________________________________________/
# 4. test and item info _________________________________________________________________________/


# 1. load data, retaining only train set info

GPT_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/real_scores4mini_d.csv')
GPT_items_df = GPT_items_df[GPT_items_df$rater == 1, ]  # standard (not lenient or severe) rater
GPT_items = paste0('GPT', c(1:10))
closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv')
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
closed_items_df = closed_items_df[c('ID', closed_items)]
data = merge(closed_items_df, GPT_items_df, by = 'ID')  # retaining only train set rows


# 2. fit mirt on closed and gpt multi
# prelim model - mirt on closed only
fitc = mirt(data[closed_items], model = 1, technical=list(NCYCLES=3000))

# actual model
covars_mixed = T
GPT_items_sel = GPT_items

while (covars_mixed) {
  formula = as.formula(paste("~", paste(GPT_items_sel, collapse = ' + ')))
  fit = mirt(data[closed_items], model = 1, technical=list(NCYCLES=3000), 
             covdata = data[GPT_items_sel], formula = formula)
  coefs <- coef(fit, IRTpar=TRUE, simplify=TRUE)  
  covars = data.frame(coefs$lr.betas)
  covars$item = rownames(covars)
  if (min(covars$F1) < 0) {
    nc = covars[covars$F1!=min(covars$F1), ]
    GPT_items_sel = GPT_items_sel[GPT_items_sel %in% nc$item]
    #print(nc)
  } else {
    covars_mixed = F
  }
}

m2v = mod2values(fit)


# 3. save the param.s of model
write.csv(coefs$items, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/GPT_cov_ItemParams_d.csv", row.names = T)
write.csv(coefs$lr.betas, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/GPT_cov_Covars_d.csv", row.names = T)
save(fit, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/GPT_cov_mirt_d.RData")


# 4. test and item info
Theta <- matrix(seq(-4, 4, length.out = 100))
total_info_c = testinfo(fitc, Theta)
plot(Theta, total_info_c, type='l')

total_info = testinfo(fit, Theta)
plot(Theta, total_info, type='l')  # lower info from items only, because now modelled in conj with covar.s



