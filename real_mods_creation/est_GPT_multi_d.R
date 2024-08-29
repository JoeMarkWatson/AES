# Est params and info for GPT multi (bi-factor)

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
# 3. run mirt on closed and GPT-scored items, fixing closed items params (semi-fixed)____________/
# 3.b check no low discrim/multi-coll ___________________________________________________________/
# 4. save the param.s of semi-fixed model _______________________________________________________/
# 5. test and item info _________________________________________________________________________/


# i. def fun.s

make_GPT_rows = function(item_name, n_unique) {
  data.frame(
    group = "all",
    item = rep(GPT_items[k], n_unique+1),
    class = "graded",
    name = c('a1', 'a2', paste0('d', c(1:(n_unique-1)))),
    parnum = NA,
    value = c((n_unique+1):1),
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
GPT_items = paste0('GPT', c(1:10))
closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv')
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
closed_items_df = closed_items_df[c('ID', closed_items)]
data = merge(closed_items_df, GPT_items_df, by = 'ID')  # retaining only train set rows


# 2. fit mirt on just closed items, saving the parameters

fitc = mirt(data[closed_items], model = 1, technical=list(NCYCLES=3000))
fitc_params = mod2values(fitc)
fitc_params$est = F


# 3. run mirt on closed and GPT-scored item, fixing closed items params (semi-fixed)

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

new_item_params = data.frame(matrix(nrow=0, ncol=12))
for (k in seq(GPT_items)) {
  nunique = nrow(unique(data[GPT_items[k]]))
  new_item_params = rbind(new_item_params, make_GPT_rows(item_name=GPT_items[k], n_unique = nunique))
}
all_params = rbind(up,
                   new_item_params,
                   group_rows)
all_params$parnum = c(1:nrow(all_params))  # format all_params

n_qs = length(unique(all_params$item))-1

mod_string = paste0("F1=1-", n_qs, "\nF2=", length(closed_items)+1, "-", n_qs)

# fix params
fit = mirt(data[c(closed_items, GPT_items)], model = mod_string, 
           technical=list(NCYCLES=3000), pars=all_params)
coefs <- coef(fit, IRTpar=T, simplify=TRUE)
# Note mirt documentation: 
# "IRTpars: logical; convert slope intercept parameters into traditional IRT parameters? Only
# applicable to unidimensional models or models with simple structure (i.e., only one non-zero slope)."
item_params = data.frame(coefs$items)
print(tail(item_params, 16))
#View(item_params)

## no fixed params
#fit_noFix = mirt(data[c(closed_items, GPT_items)], model = mod_string, 
#                 technical=list(NCYCLES=3000)) 
#coefs_noFix <- coef(fit_noFix, IRTpar=TRUE, simplify=TRUE)
#item_params_noFix = data.frame(coefs_noFix$items)
#print(tail(item_params_noFix, 16))  # near identical to fixed solution, above


# 3.b check no low discrim/multi-coll

min(item_params$a1)  # all discrim onto primary factor above 0.35 (min of 0.564 for all, and of 0.615 for GPT items)
min(item_params[item_params$a2 > 0, ]$a2)  # of GPT items all discrim onto secondary factoor above 0.35 (min of 2.175)
# no local dep
resids = data.frame(residuals(fit, type = "Q3"))  # , suppress = 0.2 removed
resids[resids == 1] <- -999  # non NA place holder to facil following max calc
resids[is.na(resids)] <- -999
max(resids)
#print(paste0("number of resids over resid_cut pre (further) removal: ", sum(resids > resids_cut)))


# 4. save the param.s of semi-fixed model
write.csv(item_params, "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_params_d.csv")
write.csv(mod2values(fit), "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/gpt_multi_params_mirt_d.csv")


# 5. test info
  # info with all (inc) GPT items
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
plot(Theta, total_info - total_info_c, type='l')


