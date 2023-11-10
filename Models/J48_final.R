library(caret)
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)
library(C50)

# Load the preprocessed datasets
preprocessed_train_data <- read.csv("./Datasets/initial_train.csv", header=TRUE)
preprocessed_test_data <- read.csv("./Datasets/initial_test.csv", header=TRUE)

# Ensure the target variable is a factor
preprocessed_train_data$o_bullied <- as.factor(preprocessed_train_data$o_bullied)
preprocessed_test_data$o_bullied <- as.factor(preprocessed_test_data$o_bullied)

library(RWeka)
modelLookup("J48")

set.seed(31)
# Repeat 5-fold cross-validation 5 times
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(o_bullied ~ ., data = preprocessed_train_data, method = "J48", trControl = train_control)
model
plot(model)

# Test
test_pred <- predict(model, newdata = preprocessed_test_data)
confusionMatrix(test_pred, preprocessed_test_data$o_bullied)


#####
#Class 0 = 83.79
#Class 1 = 77.27
#####
