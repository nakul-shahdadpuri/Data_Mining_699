# Load necessary libraries
library(caret)
library("caret")
library("ROSE")

library(Boruta)
library(caret)
library(rsample)
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)
library(neuralnet)  # Add the neuralnet library

# Load the dataset and prepare it
clean_dataset <- read.csv("./Datasets/preprocessed_dataset.csv", header=TRUE)
clean_dataset$o_bullied = as.factor(clean_dataset$o_bullied)

# Split the data
set.seed(123)
split <- initial_split(clean_dataset, prop = 0.80, strata = o_bullied)
train <- training(split)

test <- testing(split)
table(train$o_bullied)

# Model Change here

dim(train)
dim(test)

# Build a neural network model
# You can modify the parameters as needed

train_params <- trainControl(method = "repeatedcv", number = 10, repeats = 5)


# Build a neural network model with fixed parameters (5 hidden layers and decay of 0.01)
nnet_model <- train(train[,-38], train$o_bullied,
                    method = "nnet",
                    trControl = train_params,
                    preProcess = c("scale"),
                    na.action = na.omit,
                    tuneGrid = data.frame(size = 5, decay = 0.01)
)

prop.table(table(train$o_bullied))   #Baseline Accuracy

# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, train)

# Confusion matrix on training data
table(train$o_bullied, nnet_predictions_train)


#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)

# Confusion matrix on test set
table(test$o_bullied, nnet_predictions_test)

