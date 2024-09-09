# Est params and info for GPT single item (unidimensional)

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
# 2. fit mirt on just closed items, saving the parameters _______________________________________/
# 3. run mirt on closed and GPT-scored item, fixing closed items params (semi-fixed)_____________/
# 4. save the param.s of semi-fixed model _______________________________________________________/
# 5. test and item info _________________________________________________________________________/


# i. def fun.s

make_new_rows = function(item_name, n_unique) {
  data.frame(
    group = "all",
    item = rep(GPT_items[k], n_unique),
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


# 1. load data, retaining only train set info

GPT_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/real_scores4mini_d.csv')
GPT_items_df = GPT_items_df[GPT_items_df$rater == 1, ]  # standard (not lenient or severe) rater
GPT_items = 'GPT1'
closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv')
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
closed_items_df = closed_items_df[c('ID', closed_items)]
data = merge(closed_items_df, GPT_items_df[c('ID', GPT_items)], by = 'ID')  # retaining only train set rows


# 2. fit mirt on just closed items, saving the parameters

fitc = mirt(data[closed_items], model = 1, technical=list(NCYCLES=3000))
fitc_params = mod2values(fitc)
fitc_params$est = F


# 3. run mirt on closed and GPT-scored item, fixing closed items params (semi-fixed)

new_item_params = data.frame(matrix(nrow=0, ncol=12))
for (k in seq(GPT_items)) {
  nunique = nrow(unique(data[GPT_items[k]]))
  new_item_params = rbind(new_item_params, make_new_rows(item_name=GPT_items[k], n_unique = nunique))
}
all_params = rbind(fitc_params[fitc_params$item != "GROUP", ],
                   new_item_params,
                   fitc_params[fitc_params$item == "GROUP", ])
all_params$parnum = c(1:nrow(all_params))  # format all_params

fit = mirt(data[c(closed_items, GPT_items)], model = 1, 
           technical=list(NCYCLES=3000), pars=all_params)

coefs <- coef(fit, IRTpar=TRUE, simplify=TRUE)
item_params = data.frame(coefs$items)


# 4. save the param.s of semi-fixed model

write.csv(item_params, "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_single_params_d.csv")
write.csv(mod2values(fit), "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_single_params_mirt_d.csv")
save(fit, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_single_mirt_d.RData")


# 5. test and item info

Theta <- matrix(seq(-4, 4, length.out = 100))
tinfo <- testinfo(fit, Theta)
plot(Theta, tinfo, type='l')

# info with only closed items
total_info_c = testinfo(fitc, Theta)
plot(Theta, total_info_c, type='l')

# of GPT items only
plot(Theta, tinfo - total_info_c, type='l')


