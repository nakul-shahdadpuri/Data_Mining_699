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
clean_dataset <- read.csv("./Models/clean_dataset.csv", header=TRUE)
table(clean_dataset$o_bullied)

# Convert the class attribute into a factor
clean_dataset$o_bullied = as.factor(clean_dataset$o_bullied)

# Split the dataset into training and testing sets (you can customize the split ratio)
set.seed(123)  # Set a seed for reproducibility
sample_indices <- sample(nrow(clean_dataset), 0.7 * nrow(clean_dataset))
train_data <- clean_dataset[sample_indices, ]
test_data <- clean_dataset[-sample_indices, ]

# Create the Naive Bayes model
nb_model <- naiveBayes(clean_dataset[, -which(names(clean_dataset) == "o_bullied")], clean_dataset[, "o_bullied"])

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data[, -which(names(test_data) == "o_bullied")])

# Calculate accuracy or other performance metrics (you can customize this part)
confusion_matrix <- table(Actual = test_data[, "o_bullied"], Predicted = predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the confusion matrix and accuracy
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

#####
#Class 0 TPR = 66.67%
#Class 1 TPR = 49.86%
#Accuracy = 50.5%
#####

