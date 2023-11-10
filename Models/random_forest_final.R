# Assuming your preprocessing steps are done before this point

# Load necessary libraries
library(caret)
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)
library(ROSE)

# Load your preprocessed and oversampled training set
train_data <- read.csv("./Models/initial_train.csv", header=TRUE)

# Load your preprocessed testing set
test_data <- read.csv("./Models/initial_test.csv", header=TRUE)


# Check class distribution in the training set
table(train_data$o_bullied)

# Check class distribution in the testing set
table(test_data$o_bullied)

# Convert the target variable to a factor
train_data$o_bullied <- as.factor(train_data$o_bullied)
test_data$o_bullied <- as.factor(test_data$o_bullied)

# Set the seed for reproducibility
set.seed(123)

# Train the Random Forest model on the preprocessed and oversampled training set
rf_model <- randomForest(o_bullied ~ ., data = train_data, ntree = 500, importance = TRUE)

# Make predictions on the preprocessed testing set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
performance_measures <- confusionMatrix(data = predictions, reference = test_data$o_bullied)

# Display performance measures
performance_measures

# Display feature importance
feature_importance <- importance(rf_model)
