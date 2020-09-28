

###   decision tree\n
if (!require(C50)) {
    	install.packages("C50")
    }
###   ROC curve\n
 if (!require(pROC)) {
    	install.packages("pROC")
    }
library(C50)
library(pROC)
        
setwd(" ")
getwd()
affair <- read.csv("affairs.csv")
str(affair)
head(affair)     
nrow(affair)
nrow(!complete.cases(affair))
###   partition dataset for training (80%) and testing (20%)
size <- floor(0.8 * nrow(affair))
size
 ###   randomly decide which ones for training
training_index <- sample(nrow(affair), size = size, replace = FALSE)
        
train <- affair[training_index,]
test <- affair[-training_index,]
        
###   names of variables that used for prediction
var_names <- names(affair)[-5]  #this gives column titles of first 4 variables (so everything except #5)
var_names

 # fit the model
dt <- C5.0(x = train[, var_names], y = train$if_affair)

# see the summary of model
summary(dt)
        
###   now, validate test
##    predict() method returns a vector of result
dt_pred <- predict(dt, newdata = test)
plot(dt)
###   merger dt_prediction value to test dataset
dt_evaluation <- cbind(test, dt_pred)
head(test)
head(dt_evaluation)
        
### compare dt_prediction result to actual value
dt_evaluation$correct <- ifelse(dt_evaluation$if_affair == dt_evaluation$dt_pred, 1, 0)
        
head(dt_evaluation)
        
###    accuracy rate
dt_accuracy = sum(dt_evaluation$correct) / nrow(dt_evaluation)
dt_accuracy
        
###   confusion matrix
table(dt_evaluation$if_affair, dt_evaluation$dt_pred)
        

###   True Positive Rate (Sensitivity)  TPR = TP / P
###   = count of true positive dt_prediction divided by total positive truth
TPR <- sum(dt_evaluation$dt_pred == 'yes' & dt_evaluation$if_affair == 'yes') / sum(dt_evaluation$if_affair == 'yes')
TPR
        
        
###   True Negative Rate (Specificity)  TNR = TN / N
###   = count of true negative dt_prediction divided by total negative truth
TNR <- sum(dt_evaluation$dt_pred == 'no' & dt_evaluation$if_affair == 'no') / sum(dt_evaluation$if_affair == 'no')
TNR
        
###   False Positive Rate  (1 - Specificity)  FPR = FP / N
###   = count of false positive dt_prediction divided by total negative truth
###   = sum(dt_evaluation$dt_pred == 'yes'& dt_evaluation$if_affair == 'no') / sum(dt_evaluation$if_affair_50K == 'no')
FPR <- 1 - TNR 
FPR
        
###   False Negative Rate FNR (1 - Sensitivity)  FNR = FN / P
###   = count of false negative dt_prediction divided by total positive truth
###   = sum(dt_evaluation$dt_pred == 'no'& dt_evaluation$if_affair == 'yes') / sum(dt_evaluation$if_affair == 'yes')
FNR <- 1 - TPR
FNR
        
###   dt_precision equals 
###   = number of true positive dt_prediction  / total positive dt_prediction
        
dt_precision <- sum(dt_evaluation$if_affair == 'yes' & dt_evaluation$dt_pred == 'yes') / sum(dt_evaluation$dt_pred == 'yes')
dt_precision
        
###   dt_recall equals = TPR
###   = true positive dt_prediction / total true positive
        
dt_recall <- sum(dt_evaluation$if_affair == 'yes' & dt_evaluation$dt_pred == 'yes') / sum(dt_evaluation$if_affair == 'yes')
dt_recall
        
 ###  F score
F <- 2 * dt_precision * dt_recall / (dt_precision + dt_recall)
F
   
###########################################################################
###        ROC Curve: Receiver Operating Characteristic Curve           ###
###########################################################################
       
###   randomly decide which ones for training
training_index2 <- sample(nrow(affair), size = size, replace = FALSE)
        
train2 <- affair[training_index2,]
test2 <- affair[-training_index2,]


####################################
###         fitting model        ###
####################################
affair$rating = as.factor(affair$rating)
affair$religiousness = as.factor(affair$religiousness)

###   fitting regression model
reg <- glm(if_affair ~ . , data = train2, family = binomial() )
        
 ###  model detail
summary(reg)
plot(reg)
        
###   validate test dataset
evaluation <- test2
        
###   return risk instead of classification
evaluation$prob <- predict(reg, newdata = evaluation, type = "response")
        
#   Calculate baseline 
count_affair <- nrow(subset(affair, affair$if_affair == "yes") )
baseline <- count_affair / nrow(affair)
baseline   #this is the affair rate which you want to beat with prediction
        
 ####################################
 ###     create roc graph         ###
###S################################
#  sensitive   Specificity
g <- roc(evaluation$if_affair ~ evaluation$prob, data = evaluation)
        
#   ROC curve
plot(g)
summary(g)
        
#   Show Area Under the Curve (AUC)
g
        


