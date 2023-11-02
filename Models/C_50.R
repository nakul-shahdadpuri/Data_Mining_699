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

# Read the preprocessed dataset
df <- read.csv("./Models/clean_dataset.csv", header = TRUE)

# Convert the class attribute into a factor
df$o_bullied <- as.factor(df$o_bullied)

# Split the dataset into training and testing sets (you can customize the split ratio)
set.seed(123)  # for reproducibility
# Split the dataset into training and testing sets (e.g., 80% training, 20% testing)
splitIndex <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[splitIndex, ]
test_data <- df[-splitIndex, ]

library(C50)

# repeat 5-fold cross-validation 5 times
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                              summaryFunction = defaultSummary)
C5.tree <- train(o_bullied ~ ., data = train_data, method = "C5.0", trControl = train_control)

#Without cross validation
#C5.tree <- C5.0(o_bullied ~ ., data = train_data)

plot(C5.tree)
# test 
pred <- predict(C5.tree, newdata = test_data, type = "raw")  #change type to class when not using cross validation
performance_measures <- confusionMatrix(data=pred,  
                                        reference = test_data$o_bullied)
performance_measures

#####
#Without cross validation
#Class 0 TPR = 75.55%
#Class 1 TPR = 82.28%
#Accuracy = 78.89%
#####

#####
#With cross validation (5 folds, 5 times)
#Class 0 TPR = 85.97%
#Class 1 TPR = 92.87%
#Accuracy = 89.39%
#####
