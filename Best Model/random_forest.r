library(caret)
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)
library(randomForest)
library(ROSE)


# Load your dataset (replace 'clean_dataset.csv' with the actual file path)
clean_dataset <- read.csv("./Models/clean_dataset.csv", header=TRUE)
table(clean_dataset$o_bullied)

clean_dataset$o_bullied = as.factor(clean_dataset$o_bullied)

str(clean_dataset)
summary(clean_dataset)

# Set the seed for reproducibility
set.seed(123)

# Split the dataset into training and testing sets (e.g., 80% training, 20% testing)
splitIndex <- sample(1:nrow(clean_dataset), 0.8 * nrow(clean_dataset))
train_data <- clean_dataset[splitIndex, ]
test_data <- clean_dataset[-splitIndex, ]

# Check class distribution in the training set
table(train_data$o_bullied)


rf_model <- randomForest(o_bullied ~ ., data = train_data, ntree = 500, importance = TRUE)

predictions <- predict(rf_model, newdata = test_data)

performance_measures  <- confusionMatrix(data=predictions,
                                         reference = test_data$o_bullied)

performance_measures
feature_importance <- importance(rf_model)



#####
#Class 0 TPR = 88%
#Class 1 TPR = 92%
#Accuracy = 90.81%
#####

#####
#3 fold with 5 times
#Class 0 TPR = 88.18%
#Class 1 TPR = 93.28%
#Accuracy = 90.71%
#####
