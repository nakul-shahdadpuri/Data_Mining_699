library(data.table)
library(xgboost)
library(caret)
library(e1071)
require(caTools)

# Load the preprocessed datasets
preprocessed_train_data <- read.csv("./Datasets/initial_train.csv", header=TRUE)
preprocessed_test_data <- read.csv("./Datasets/initial_test.csv", header=TRUE)

# Ensure the target variable is a factor
preprocessed_train_data$o_bullied <- as.factor(preprocessed_train_data$o_bullied)
preprocessed_test_data$o_bullied <- as.factor(preprocessed_test_data$o_bullied)
nrow(preprocessed_train_data)
nrow(preprocessed_test_data)
# Set seed for reproducibility
set.seed(123) 

# Create the training and testing sets
#trainIndex <- createDataPartition(preprocessed_train_data$o_bullied, p = 0.75, list = FALSE)
trainSet <- preprocessed_train_data
testSet <- preprocessed_test_data

# Separate features and labels
train_data <- trainSet[, !(names(trainSet) == "o_bullied")]
train_label <- as.factor(ifelse(trainSet$o_bullied == 0, "Class0", "Class1"))

test_label_char <- ifelse(testSet$o_bullied == 0, "Class0", "Class1")  # For prediction comparison
testSet <- testSet[, !(names(testSet) == "o_bullied")]
test_label <- preprocessed_test_data$o_bullied
length(test_label)
nrow(testSet)

# Hyperparameter Tuning
grid <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.01, 0.1),
  max_depth = c(4, 6, 8),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = c(1, 5),
  subsample = c(0.7, 0.9)
)

# Train control
control <- trainControl(
  method = "cv", 
  number = 3, 
  verboseIter = TRUE, 
  returnData = FALSE, 
  returnResamp = 'all', 
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# Hyperparameter tuning with caret
model_caret <- train(
  x = train_data, 
  y = train_label,
  trControl = control,
  tuneGrid = grid,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

# Predicting with Tuned Model
preds <- predict(model_caret, newdata = testSet)
predicted_labels <- ifelse(preds == "Class0", 0, 1)

# Testing and Confusion Matrix
length(test_label)
length(predicted_labels)
conf_matrix <- table(Predicted = predicted_labels, Actual = test_label)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")
