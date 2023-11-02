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


library(RWeka)
modelLookup("J48")

set.seed(31)
# repeat 10-fold cross-validation 5 times
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(o_bullied ~ ., data = train_data, method = "J48", trControl = train_control)
model
plot(model)
test_pred <- predict(model, newdata = test_data)
confusionMatrix(test_pred, test_data$o_bullied)

#####
# 1 fold and 5 times
#Class 0 TPR = 71.75%
#Class 1 TPR = 80.98%
#Accuracy = 76.30%
#####

#####
# 5 fold and 5 times
#Class 0 TPR = 79.84%
#Class 1 TPR = 86.59%
#Accuracy = 83.16%
#####
