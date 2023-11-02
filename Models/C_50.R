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
df <- read.csv("clean_dataset.csv", header = TRUE)

# Convert the class attribute into a factor
df$o_bullied <- as.factor(df$o_bullied)

# Split the dataset into training and testing sets (you can customize the split ratio)
set.seed(123)  # for reproducibility
# Split the dataset into training and testing sets (e.g., 80% training, 20% testing)
splitIndex <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[splitIndex, ]
test_data <- df[-splitIndex, ]

library(C50)
C5.tree <- C5.0(o_bullied ~ ., data = train_data)
plot(C5.tree)
# test 
pred <- predict(C5.tree, newdata = test_data, type = "class")
performance_measures <- confusionMatrix(data=pred,  
                                        reference = test_data$o_bullied)
performance_measures
