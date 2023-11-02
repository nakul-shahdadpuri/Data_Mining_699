library(caret) 
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)


clean_dataset <- read.csv("C:/Users/nakul/Desktop/Boston University/CS699 - Data Mining/Project/clean_dataset.csv", header=TRUE)

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

pred <- predict(fit_rf, newdata = test, type = "class")
performance_measures  <- confusionMatrix(data=pred,
                                         reference = test$o_bullied)
#9 Printing the metrics
performance_measures

