library(boot)
library(kernlab)
library(caret)
data("spam")
tibble::as.tibble(spam)
#Code given to us below 
set.seed(42)
spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

set.seed(1)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]
# Most Underfit - > Most Overfit using K=5 = Fit_cpas , Fit_selected, Fit_over, Fit_additive
# Most Underfit - > Most Overfit using K=100 = Fit_cpas , Fit_selected, Fit_over, Fit_additive
# My conclusion did not chnage 

spam_prediction_additive = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_prediction_caps = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                         "spam",
                         "nonspam")
spam_prediction_selected = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                         "spam",
                         "nonspam")
spam_prediction_over = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                         "spam",
                         "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_prediction_additive, actual = spam_tst$type))
(conf_mat_50 = make_conf_mat(predicted = spam_prediction_over, actual = spam_tst$type)) #most accurate
(conf_mat_50 = make_conf_mat(predicted = spam_prediction_selected, actual = spam_tst$type))
(conf_mat_50 = make_conf_mat(predicted = spam_prediction_caps, actual = spam_tst$type))


table(spam_tst$type) / nrow(spam_tst)


newspam = spam

set.seed(3456)
trainIndex <- createDataPartition(newspam$make, p = .5,
                                  list = FALSE,
                                  times = 1)
Train <- newspam[ trainIndex,]
Valid <- newspam[-trainIndex,]

model <- train(
  make ~ ., 
  spam,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)
coef(summary(model))


