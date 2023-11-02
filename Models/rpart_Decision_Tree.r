library(rpart)
library(rpart.plot)

clean_dataset <- read.csv("./Models/clean_dataset.csv", header=TRUE)


clean_dataset$o_bullied = as.factor(clean_dataset$o_bullied)

# Split the dataset into training and testing sets (e.g., 70% training and 30% testing)
set.seed(123)  # Set a seed for reproducibility
sample_indices <- sample(nrow(clean_dataset), 0.7 * nrow(clean_dataset))
train_data <- clean_dataset[sample_indices, ]
test_data <- clean_dataset[-sample_indices, ]

# Train the decision tree model
decision_tree_model <- rpart(o_bullied ~ ., data = train_data, method = "class",
                             control = rpart.control(maxdepth = 20))

# Plot the decision tree
rpart.plot(decision_tree_model)

# Make predictions on the test data
predictions <- predict(decision_tree_model, newdata = test_data, type = "class")

# Evaluate the model's performance
confusion_matrix <- table(predictions, test_data$o_bullied)
confusion_matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the accuracy
print(paste("Accuracy: ", accuracy))


#####
#Class 0 TPR = 48.94%
#Class 1 TPR = 80.98%
#Accuracy = 64.71%
#####
