library(data.table)
library(xgboost)
library(caret)
library(e1071)
require(caTools)

<<<<<<< HEAD
clean_dataset <- read.csv("./Models/clean_dataset.csv", header=TRUE)
=======
clean_dataset <- read.csv("C:/Users/nakul/Desktop/Boston University/CS699 - Data Mining/Project/clean_dataset.csv", header=TRUE)
>>>>>>> 960c1d4452ccd9893ba2710c8775ee544d1f634b
table(clean_dataset$o_bullied)

set.seed(123) 
trainIndex <- createDataPartition(clean_dataset$o_bullied, p = 0.75, list = FALSE)
trainSet <- df[trainIndex, ]
testSet <- df[-trainIndex, ]

train_data <- trainSet[, !(names(trainSet) == "o_bullied")]

<<<<<<< HEAD
train_label <- as.factor(ifelse(trainSet$o_bullied == 1, "Class1", "Class0"))
test_label_char <- ifelse(testSet$o_bullied == 1, "Class1", "Class0")  # For prediction comparison
test_data <- testSet[, -13]
=======
train_label <- as.factor(ifelse(trainSet$o_bullied == 0, "Class0", "Class1"))
test_label_char <- ifelse(testSet$o_bullied == 0, "Class0", "Class1")  # For prediction comparison
testSet <- testSet[, !(names(trainSet) == "o_bullied")]
>>>>>>> 960c1d4452ccd9893ba2710c8775ee544d1f634b
test_label <- testSet$o_bullied  


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
  number = 5, 
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
preds <- predict(model_caret, newdata = test_data)
predicted_labels <- ifelse(preds == "Class0", 0, 1)

# Testing and Confusion Matrix
conf_matrix <- table(Predicted = predicted_labels, Actual = test_label)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

