library(lubridate)
library(ggplot2)
library(Rmisc)
library(lmtest)
library(caret)
library(pscl)
library(survey)
library(ROCR)
library(psych)
library(class)
library(dplyr)
library(gmodels)
library(e1071)

# Read in and clean the data
setwd("C:/Users/tzhan/Google Drive/GSU Graduate School/STAT 8820 Research")
raw_data <- read.csv("carInsurance_train.csv")
raw_data$Id <- NULL
raw_data$LastContactDay <- NULL
raw_data$LastContactMonth <- NULL

x1 <- ggplot(raw_data, aes(x = Communication)) + ggtitle("Contact Communication Type") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
x2 <- ggplot(raw_data, aes(x = Outcome)) + ggtitle("Outcome of the previous marketing campaign") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
multiplot(x1, x2, layout = matrix(c(1,2), nrow = 1))

# "Outcome" variable
raw_data$Outcome <- addNA(raw_data$Outcome)
levels(raw_data$Outcome) <- c('failure', 'other', 'success', 'noPrevious')
raw_data[raw_data['PrevAttempts']==0,'Outcome']='noPrevious'
sum(raw_data['DaysPassed']==-1) == sum(raw_data['Outcome']=='noPrevious') 

raw_data$Communication <- NULL

# Remove missing data
not_missing <- (apply(is.na(raw_data), 1, sum) == 0)
new_data <- raw_data[not_missing,]

# Combine time of Call Start & End into one variable - Total Length of the Call (in minutes):
CallStart <- hms(as.character(new_data$CallStart))
CallEnd <- hms(as.character(new_data$CallEnd))
new_data$CallLength <- as.numeric(as.duration(CallEnd - CallStart), "minutes")
new_data$CallEnd <- NULL
new_data$CallStart <- NULL

# Reorder the data so that the response variable is the last column
new_data <- new_data[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,13)]

# Data Exploration:

# Response Variable:
new_data$CarInsurance[new_data$CarInsurance==0] <- "No"
new_data$CarInsurance[new_data$CarInsurance==1] <- "Yes"
ggplot(new_data, aes(x = CarInsurance)) + ggtitle("Did the Customer buy Car Insurance?") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(stat = "count", color="black", fill="gray") 

# Recode response variable into a binary class
new_data$CarInsurance[new_data$CarInsurance=='No'] <- 0
new_data$CarInsurance[new_data$CarInsurance=='Yes'] <- 1
new_data$CarInsurance <- as.factor(new_data$CarInsurance)
str(new_data)
nrow(new_data[new_data$CarInsurance==0,]) #2299 customers did not purchase
nrow(new_data[new_data$CarInsurance==1,]) #1521 customers purchased

# Categorical variables

# Convert from integer to factor:
new_data$Default <- as.factor(new_data$Default)
new_data$HHInsurance <- as.factor(new_data$HHInsurance)
new_data$CarLoan <- as.factor(new_data$CarLoan)

# Relevel Job, Marital, and Outcome variables :
new_data$Job <- relevel(new_data$Job,'student')
new_data$Marital <- relevel(new_data$Marital,'single')
new_data$Outcome <- relevel(new_data$Outcome,'noPrevious')
str(new_data)

xtabs(~CarInsurance + Job,new_data)
xtabs(~CarInsurance + Marital,new_data)
xtabs(~CarInsurance + Education,new_data)
xtabs(~CarInsurance + Default,new_data)
xtabs(~CarInsurance + HHInsurance,new_data)
xtabs(~CarInsurance + CarLoan,new_data)
xtabs(~CarInsurance + Outcome,new_data)

p2 <- ggplot(new_data, aes(x = Job)) + ggtitle("Job of the Client") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p3 <- ggplot(new_data, aes(x = Marital)) + ggtitle("Marital Status of the Client") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p4 <- ggplot(new_data, aes(x = Education)) + ggtitle("Education Level of the Client") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p5 <- ggplot(new_data, aes(x = Default)) + ggtitle("Has credit in default?") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p7 <- ggplot(new_data, aes(x = HHInsurance)) + ggtitle("Is household insured?") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p8 <- ggplot(new_data, aes(x = CarLoan)) + ggtitle("Does client have a car loan?") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
p9 <- ggplot(raw_data, aes(x = Outcome)) + ggtitle("Outcome of previous campaign") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_bar(colour="black", fill="gray")
multiplot(p3, p4, p5, p7, p8, p9, layout = matrix(c(1,2,3,4,5,6), nrow = 2, byrow=T))

barplot(table(CarInsurance, Job),horiz=T,las=1,cex.names=0.55,col=c("gray", "lightskyblue"), 
        main='Car Insurance Purchase Rate by Job')
legend(600,9.5, legend = c("Did Not Purchase", "Purchased"), fill=c("gray", "lightskyblue"))

barplot(table(CarInsurance, Marital),horiz=T,las=1,cex.names=0.95,col=c("gray", "lightskyblue"), 
        main='Car Insurance Purchase Rate by Marital Status',xlim=c(0, 2500))
legend(1750,3.5, legend = c("Did Not Purchase", "Purchased"), fill=c("gray", "lightskyblue"))

barplot(table(CarInsurance, Education),horiz=T,las=1,cex.names=0.75,col=c("gray", "lightskyblue"), 
        main='Car Insurance Purchase Rate by Education level',xlim=c(0, 2000))
legend(1500,3.5, legend = c("Did Not Purchase", "Purchased"), fill=c("gray", "lightskyblue"))

barplot(table(CarInsurance, HHInsurance),horiz=T,las=0,names.arg = c('No HHInsurance', 'Has HHInsurance'),
        col=c("gray", "lightskyblue"),main='Car Insurance Purchase Rate by Household Insurance',xlim=c(0, 2000))

barplot(table(CarInsurance, Outcome),horiz=T,las=0,cex.names=0.9,col=c("gray", "lightskyblue"), 
        main='Car Insurance Purchase Rate by Outcome in Previous Campaign',xlim=c(0, 3000))
legend(2000,2.5, legend = c("Did Not Purchase", "Purchased"), fill=c("gray", "lightskyblue"))

# Continuous variables
p1 <- ggplot(new_data, aes(x = Age)) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(colour="black", fill="gray")
p6 <- ggplot(new_data, aes(x = Balance))  + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_histogram(colour="black", fill="gray")
p10 <- ggplot(new_data, aes(x = NoOfContacts)) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(colour="black", fill="gray")
p11 <- ggplot(new_data, aes(x = DaysPassed)) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(colour="black", fill="gray")
p12 <- ggplot(new_data, aes(x = PrevAttempts)) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(colour="black", fill="gray")
p13 <- ggplot(new_data, aes(x = CallLength)) + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_histogram(colour="black", fill="gray")
multiplot(p1, p6, p10, p11, p12, p13, layout = matrix(c(1,2,3,4,5,6), nrow = 2, byrow=T))

attach(new_data)
# Look at Age Groups in more detail:
AgeGroup <- 0 # 0 = 18-27, 1 = 28-39, 2 = 40-49, 3 = 50-59, 4 = >=60
for (i in 1:dim(new_data)[1]){
  if (Age[i] <= 27)
    AgeGroup[i] <- 0
  else if (Age[i] > 27 & Age[i] <= 39)
    AgeGroup[i] <- 1
  else if (Age[i] > 39 & Age[i] <= 49)
    AgeGroup[i] <- 2
  else if (Age[i] > 49 & Age[i] <= 59)
    AgeGroup[i] <- 3
  else if (Age[i] > 59) 
    AgeGroup[i] <- 4
}
barplot(table(CarInsurance, AgeGroup),horiz=T,las=1,col=c("gray", "lightskyblue"), main='Car Insurance Purchase Rate by Age Group', 
        names.arg=c('18-27', '28-39', '40-49', '50-59', '>=60'),xlim = c(0,2000))
legend(1500,5, legend = c("Did Not Purchase", "Purchased"), fill=c("gray", "lightskyblue"))

# Data Analysis:
# Separate into train and test data

#set.seed(8820)
train_ind <- sample(nrow(new_data), 3056)
set.seed(8820)

train <- new_data[train_ind,]
test <- new_data[-train_ind,]
nrow(train[train$CarInsurance==0,]) #1856 cold calls failed in training data
nrow(train[train$CarInsurance==1,]) #1200 cold calls succeeded in training data

nrow(test[test$CarInsurance==0,]) #443 cold calls failed in testing data
nrow(test[test$CarInsurance==1,]) #321 cold calls accepted in testing data

str(train)
str(test)
# ---Model selection---
# Stepwise Model Selection / Logistic Regression
fit.null <- glm(CarInsurance ~ 1, data = train, family = binomial)
fit.full <- glm(CarInsurance ~ ., data = train, family = binomial)
select <- step(fit.null, scope = list(lower = fit.null, upper = fit.full), direction = "both")

# Removed "Default", "DaysPassed", "Balance" & "Age"
fit <- glm(CarInsurance ~ CallLength + Outcome + HHInsurance + Job + CarLoan + NoOfContacts + Education + Marital +
             PrevAttempts, data = train, family = binomial)
fit_withAge <- glm(CarInsurance ~ CallLength + Outcome + HHInsurance + Job + CarLoan + NoOfContacts + Education + Marital +
             PrevAttempts + Age, data = train, family = binomial)
summary(fit)
# Convert to odds ratios:
exp(coef(fit))
exp(cbind("Odds ratio"=coef(fit), confint(fit)))

# Goodness of Fit

# Wald Test:
regTermTest(fit, "CallLength")
regTermTest(fit, "Outcome")
regTermTest(fit, "HHInsurance")
regTermTest(fit, "Job")
regTermTest(fit, "CarLoan")
regTermTest(fit, "NoOfContacts")
regTermTest(fit, "Education")
regTermTest(fit, "Marital")
regTermTest(fit, "PrevAttempts")
regTermTest(fit_withAge, "Age") # p-value of 0.9911

# McFadden's pseudo R^2: = 0.3768885
pR2(fit)[4]

# Cross Validation
pred  <-  predict(fit, newdata=test, type="response")
CarInsurance.pred <- NULL
for (i in 1:length(pred)){ # Classify as success if >=0.5, and failure if <0.5
  if(pred[i] >= 0.5){
    CarInsurance.pred[i] <- 1}
  else{
    CarInsurance.pred[i] <- 0}
}
CarInsurance.pred <- as.factor(CarInsurance.pred)
#confusionMatrix(data=CarInsurance.pred, test$CarInsurance)
CrossTable(x = CarInsurance.pred, y = test$CarInsurance, 
           prop.chisq=F, prop.c = F, prop.r = F, prop.t = F) 
# Accuracy = (393+215)/764 = 0.7958, Sensitivity = 215/321 = 0.6698, Specificity = 393/443 = 0.8871

# ROC curve and AUC
pred_ROC <- prediction(pred, test$CarInsurance)
perf <- performance(pred_ROC, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")

auc <- performance(pred_ROC, measure = "auc")
auc <- auc@y.values[[1]]
auc # = 0.884032

# ---K-nearest neighbors---
data_class <- new_data
data_class$Age <- NULL
data_class$Default <- NULL
data_class$DaysPassed <- NULL
data_class$Balance <- NULL
data_class$CarInsurance <- as.factor(ifelse(data_class$CarInsurance == 0, "no", "yes"))

CarInsurance_class <- data_class %>% select(CarInsurance) # Extract CarInsurance variable from dataset, then remove it
data_class$CarInsurance <- NULL

# Normalize Continuous Variables:
data_class[, c('NoOfContacts','PrevAttempts','CallLength')] <- scale(data_class[, c('NoOfContacts','PrevAttempts','CallLength')])

# Create Dummy Variables for all Categorical Variables not already coded as 0s & 1s:
data_class <- cbind(data_class, as.data.frame(dummy.code(data_class$Job)), as.data.frame(dummy.code(data_class$Marital)), 
                    as.data.frame(dummy.code(data_class$Education)), as.data.frame(dummy.code(data_class$Outcome)))

# Remove original variables that had to be dummy coded:
data_class$Job <- NULL
data_class$Marital <- NULL
data_class$Education <- NULL
data_class$Outcome <- NULL

# Split into training and testing sets
train_class <- data_class[train_ind,]
test_class <- data_class[-train_ind,]

# Split CarInsurance into training and test sets using the same partition as above:
CarInsurance_train <- CarInsurance_class[train_ind, ]
CarInsurance_test <- CarInsurance_class[-train_ind, ]

train_class %>% nrow %>% sqrt %>% ceiling # k = 56
CarInsurance_pred_knn <- knn(train = train_class, test = test_class, cl = CarInsurance_train, k=56)

CarInsurance_test <- data.frame(CarInsurance_test)
class_comparison <- data.frame(CarInsurance_pred_knn, CarInsurance_test)
names(class_comparison) <- c("KNN Prediction", "Observed")
CrossTable(x = class_comparison$`KNN Prediction`, y = class_comparison$Observed, prop.chisq=F, prop.c = F, prop.r = F, prop.t = F)
# Accuracy = (400+184)/764 = 0.7644, Sensitivity = 184/321 = 0.5732, Specificity = 400/443 = 0.9029
