library(caret) 
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)


clean_dataset <- read.csv("./Models/clean_dataset.csv", header=TRUE)


clean_dataset$o_bullied = as.factor(clean_dataset$o_bullied)

set.seed(31)
split <- initial_split(clean_dataset, prop = 0.70, strata = o_bullied)
train <- training(split)
test <- testing(split)

# Model Change here
table(train$o_bullied)

fit_rf <- randomForest(o_bullied ~ ., data=train, ntree=120)

#7 Plotting the Tree
print(fit_rf)

pred <- predict(fit_rf, newdata = test, type = "raw")
performance_measures  <- confusionMatrix(data=pred,
                                         reference = test$o_bullied)
#9 Printing the metrics
performance_measures

#####
#Class 0 TPR = 87.91%
#Class 1 TPR = 91.86%
#Accuracy = 89.9%
#####

#####
#3 fold with 5 times
#Class 0 TPR = 88.86%
#Class 1 TPR = 88.92%
#Accuracy = 88.89%
#####

