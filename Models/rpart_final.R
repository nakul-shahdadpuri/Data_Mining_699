library(rpart)
library(rpart.plot)

# Load the preprocessed datasets
preprocessed_train_data <- read.csv("./Datasets/initial_train.csv", header=TRUE)
preprocessed_test_data <- read.csv("./Datasets/initial_test.csv", header=TRUE)

# Ensure the target variable is a factor
preprocessed_train_data$o_bullied <- as.factor(preprocessed_train_data$o_bullied)
preprocessed_test_data$o_bullied <- as.factor(preprocessed_test_data$o_bullied)

# Train the decision tree model
decision_tree_model <- rpart(o_bullied ~ ., data = preprocessed_train_data, method = "class",
                             control = rpart.control(maxdepth = 20))

# Plot the decision tree
rpart.plot(decision_tree_model)

# Make predictions on the test data
predictions <- predict(decision_tree_model, newdata = preprocessed_test_data, type = "class")

# Evaluate the model's performance
confusion_matrix <- table(predictions, preprocessed_test_data$o_bullied)
confusion_matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the accuracy
print(paste("Accuracy: ", accuracy))
