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
preprocessed_train_data <- read.csv("./Models/initial_train.csv", header=TRUE)
preprocessed_test_data <- read.csv("./Models/initial_test.csv", header=TRUE)

# Ensure the target variable is a factor
preprocessed_train_data$o_bullied <- as.factor(preprocessed_train_data$o_bullied)
preprocessed_test_data$o_bullied <- as.factor(preprocessed_test_data$o_bullied)

# Create the Naive Bayes model
nb_model <- naiveBayes(o_bullied ~ ., data = preprocessed_train_data)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = preprocessed_test_data)

# Calculate accuracy or other performance metrics (you can customize this part)
confusion_matrix <- table(Actual = preprocessed_test_data$o_bullied, Predicted = predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the confusion matrix and accuracy
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
