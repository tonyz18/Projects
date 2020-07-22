# STAT 8561 Final Project
# Appendix (R Code):
library(ggplot2)
library(Rmisc)
library(lmtest)
library(caret)
library(pscl)
library(survey)
library(ROCR)

# Read in and clean the data
setwd("C:/Users/Tony/Google Drive/GSU Graduate School/STAT 8561 Linear Statistical Analysis I/Project")
credit <- read.table("crx.data", sep=",", na.strings = "?")
names(credit) <- c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","Y")
not_missing <- (apply(is.na(credit), 1, sum) == 0)
credit <- credit[not_missing,]
str(credit)

# Data Exploration

# Response variable:
ggplot(credit, aes(x = Y)) + geom_bar(stat = "count", color="black", fill="gray")

#Recode response variable into a binary class
credit$Y <- as.character(credit$Y)
credit$Y[credit$Y=="-"] <- 0
credit$Y[credit$Y=="+"] <- 1
credit$Y <- as.factor(credit$Y)
str(credit)
nrow(credit[credit$Y==0,]) #357 applications rejected
nrow(credit[credit$Y==1,]) #296 applications accepted

# Categorical variables
xtabs(~Y + A1,credit)
xtabs(~Y + A4,credit)
xtabs(~Y + A5,credit)
xtabs(~Y + A6,credit)
xtabs(~Y + A7,credit)
xtabs(~Y + A9,credit)
xtabs(~Y + A10,credit)
xtabs(~Y + A12,credit)
xtabs(~Y + A13,credit)

p1 <- ggplot(credit, aes(x = A1)) + geom_bar(colour="black", fill="gray")
p4 <- ggplot(credit, aes(x = A4)) + geom_bar(colour="black", fill="gray")
p5 <- ggplot(credit, aes(x = A5)) + geom_bar(colour="black", fill="gray")
p6 <- ggplot(credit, aes(x = A6)) + geom_bar(colour="black", fill="gray")
p7 <- ggplot(credit, aes(x = A7)) + geom_bar(colour="black", fill="gray")
p9 <- ggplot(credit, aes(x = A9)) + geom_bar(colour="black", fill="gray")
p10 <- ggplot(credit, aes(x = A10)) + geom_bar(colour="black", fill="gray")
p12 <- ggplot(credit, aes(x = A12)) + geom_bar(colour="black", fill="gray")
p13 <- ggplot(credit, aes(x = A13)) + geom_bar(colour="black", fill="gray")

multiplot(p1, p4, p5, p6, p7, p9, p10, p12, p13, layout = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow=T))

# Continuous variables
p2 <- ggplot(credit, aes(x = A2)) + geom_histogram(binwidth = 1, colour="black", fill="gray")
p3 <- ggplot(credit, aes(x = A3)) + geom_histogram(binwidth = 1, colour="black", fill="gray")
p8 <- ggplot(credit, aes(x = A8)) + geom_histogram(binwidth = 1, colour="black", fill="gray")
p11 <- ggplot(credit, aes(x = A11)) + geom_histogram(binwidth = 5, colour="black", fill="gray")
p14 <- ggplot(credit, aes(x = A14)) + geom_histogram(binwidth = 100, colour="black", fill="gray")
p15 <- ggplot(credit, aes(x = A15)) + geom_histogram(binwidth = 5000, colour="black", fill="gray")

multiplot(p2, p3, p8, layout = matrix(c(1,2,3), nrow = 1, byrow=T))
multiplot(p11, p14, p15, layout = matrix(c(1,2,3), nrow = 1, byrow=T))

# Remove observations in the categorical variables with low number of values in some levels
credit <- credit[c(-which(credit$A4=="l")),] # These 2 obs that were dropped also had level of 1 for "gg" for A5
credit$A4 <- droplevels(credit$A4)
credit <- credit[c(-which(credit$A6=="r")),] # 3 obs dropped
credit$A6 <- droplevels(credit$A6)
credit <- credit[c(-which(credit$A7=="n"|credit$A7=="o")),] # 3 obs dropped
credit$A7 <- droplevels(credit$A7)
credit <- credit[c(-which(credit$A13=="p")),] # 1 obs dropped, total of 9 obs dropped, n = 644
credit$A13 <- droplevels(credit$A13)
str(credit)

#Separate into train and test data
set.seed(8561)
train_ind <- sample(nrow(credit), 644-150)

train <- credit[train_ind,]
test <- credit[-train_ind,]
nrow(train[train$Y==0,]) #270 applications rejected in training data
nrow(train[train$Y==1,]) #224 applications accepted in training data

nrow(test[test$Y==0,]) #83 applications rejected in testing data
nrow(test[test$Y==1,]) #67 applications accepted in testing data

# Model selection:
# Stepwise Model Selection
fit.null <- glm(Y ~ 1, data = train, family = binomial)
fit.full <- glm(Y ~ ., data = train, family = binomial)
select <- step(fit.null, scope = list(lower = fit.null, upper = fit.full), direction = "both")

fit <- glm(Y ~ A4 + A7 + A8 + A9 + A10 + A11 + A15, data = train, family = binomial)
summary(fit)
exp(coef(fit)) # Odds ratios
exp(cbind("Odds ratio"=coef(fit), confint(fit)))

# Goodness of fit
# LRT:
lrtest(fit.full, fit)
# McFadden's pseudo R^2:
pR2(fit)
# Wald Test:
regTermTest(fit, "A4")
regTermTest(fit, "A7")
regTermTest(fit, "A8")
regTermTest(fit, "A9")
regTermTest(fit, "A10")
regTermTest(fit, "A11")
regTermTest(fit, "A15")

# Cross Validation
pred  <-  predict(fit, newdata=test, type="response")
Y.pred <- NULL
for (i in 1:length(pred)){ # Classify as accepted if >=0.5, and rejected if <0.5
  if(pred[i] >= 0.5){
    Y.pred[i] <- 1}
  else{
    Y.pred[i] <- 0}
}
Y.pred <- as.factor(Y.pred)
confusionMatrix(data=Y.pred, test$Y)

# ROC curve and AUC
pred_ROC <- prediction(pred, test$Y)
perf <- performance(pred_ROC, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")

auc <- performance(pred_ROC, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.8806869

# 10-fold CV
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
test_fit <- train(Y ~ A4 + A7 + A8 + A9 + A10 + A11 + A15,  data=credit, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred <-  predict(test_fit, newdata=test)
confusionMatrix(data=pred, test$Y)