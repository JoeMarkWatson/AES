# gen_synth_closed_d using real item bank parameters, and combine with synth essays

library(mirt)
library(catR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set wd

load("/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/fit_closed19022025_d2.RData")  # loads as fitc
true_theta <- sort(rnorm(3000, 0, 1))
itemBank <- data.frame(coef(fitc, IRTpar=TRUE, simplify=TRUE)$items)
names(itemBank) = c('a', 'b', 'c', 'd')

resps = data.frame(genPattern(th=true_theta, it=itemBank, seed=1))
names(resps) = itemBank$item
resps$ID = c(1:nrow(resps))
resps$true_theta = true_theta
names(resps) = c(row.names(itemBank), 'ID', 'true_theta')


# sample 2000 resps for train, 1000 for test
set.seed(1)
sampled_IDs <- sample(resps$ID, 2000, replace = FALSE)
resps_train = resps[resps$ID %in% sampled_IDs, ]
resps_test = resps[!resps$ID %in% sampled_IDs, ]


# train fitc_synth model on train set closed items, saving est thetas
fitc_synth = mirt(resps_train[c(row.names(itemBank))], model = 1, technical=list(NCYCLES=3000))
resps_train$theta_hat = c(fscores(fitc_synth, "EAP"))


# write csv for train and test sets
#write.csv(resps_train, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/synth_resps_train_d2UPDATED.csv', row.names = F)
#write.csv(resps_test, '/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/synth_resps_test_d2UPDATED.csv', row.names = F)
