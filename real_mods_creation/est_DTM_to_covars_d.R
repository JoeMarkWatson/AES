# dtm to covariates model

library(catR)
library(dplyr)
library(stringr)
library(mirt)
library(psych)
library(tm)
library(tidytext)
library(mokken)
library(MASS)
library(tidyverse)
library(caret)
library(leaps)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# 1. load data, retaining only train set info ___________________________________________________/
# 2. make dtm of stemmed words __________________________________________________________________/
# 3. remove v sparse words from dtm _____________________________________________________________/
# 4. retain only dtm terms that are good predictors of total score from all closed items ________/
# 5. fit mirt on closed and dtm covars (not fixed) ______________________________________________/
# 6. save the param.s of model __________________________________________________________________/
# 7. test and item info _________________________________________________________________________/


# # FUN.S

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


make_new_rows = function(item_name) {
  # THIS IS GOING TO BE A TRICKY ONE - UR GOING TO NEED TO WORK BACK FROM THE COVAR MODEL
  # THAT U END UP WITH
}


# # CODE

# 1. load data, retaining only train set info
closed_items_df = read.csv('/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/cdftlm_train_pur.csv')
closed_items = c(paste0('q', c(1:15), 'p'), paste0('q', c(17:20), 'p'))
data = closed_items_df[c('ID', closed_items, 'essays')]
data$essays = clean_text(data$essays)

# 2. make dtm of stemmed words
s <- SimpleCorpus(VectorSource(unlist(lapply(data$essays, as.character))))
dobj = DocumentTermMatrix(s, 
                          control = list(stopwords=T,
                                         stemming=T,
                                         removeNumbers=T))
dtm = as.data.frame(as.matrix(dobj))  # 3087 terms used, total (after stemming, removing stopwords and numbers)

# 3. remove v sparse words from dtm
dtm = dtm[, colSums(dtm)>50]

# 4. retain only dtm terms that are good predictors of total score from all closed items, using stepwise prediction
# following: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/ 
closed_sum = rowSums(data[, grepl('^q', names(data))])
reg_vars = cbind(closed_sum, dtm)
train.control <- trainControl(method = "cv", number = 5)
# Train the model
step.model <- train(closed_sum ~., data = reg_vars,
                    method = "lmStepAIC",  # from MASS package (backwards: stepAIC, https://cran.r-project.org/web/packages/MASS/MASS.pdf; https://cran.r-project.org/web/packages/caret/caret.pdf) 
                    # To use leaps package, then method='leapForward', 'leapBackward', or 'leapSeq', and unhash the following line:
                    #tuneGrid = data.frame(nvmax = 1:(ncol(reg_vars)-1)),
                    trControl = train.control
)
step.model$results
#step.model$bestTune
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)
regs_int = names(step.model[["finalModel"]][["coefficients"]])
cols = regs_int[regs_int!='(Intercept)']


# 5. fit mirt on closed and dtm covars (non-fixed)
# 5.i. run mirt on closed items - to facil (uninformative) testinfo comparison
fitc = mirt(data[, grepl('^q', names(data))], model = 1, technical=list(NCYCLES=3000))

# 5.ii. run mirt on closed and dtm covars
formula = as.formula(paste("~", paste(cols, collapse = ' + ')))
fit = mirt(data[, grepl('^q', names(data))], model = 1, technical=list(NCYCLES=3000), 
           covdata = dtm[cols], formula = formula)
coefs <- coef(fit, IRTpar=TRUE, simplify=TRUE)
covars = coefs$lr.betas  # coef.s of covariates. (All contained in fit_params, although ready-cleaned here)
m2v = mod2values(fit)

# 6. save the param.s of semi-fixed model
write.csv(coefs$items, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTM_cov_ItemParams_d.csv", row.names = T)
write.csv(covars, file="/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTM_cov_Covars_d.csv", row.names = T)
save(fit, file = "/Users/jw/Desktop/dt/jbs_work/Psychometrician_position/ivan proj/git_repo/output/DTM_cov_mirt_d.RData")

# 7. test and item info
Theta <- matrix(seq(-4, 4, length.out = 100))
total_info_c = testinfo(fitc, Theta)
plot(Theta, total_info_c, type='l')

total_info = testinfo(fit, Theta)
plot(Theta, total_info, type='l')  # lower info from items only, because now modelled in conj with covar.s

