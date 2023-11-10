library(C50)

# Load the preprocessed datasets
preprocessed_train_data <- read.csv("./Models/initial_train.csv", header=TRUE)
preprocessed_test_data <- read.csv("./Models/initial_test.csv", header=TRUE)

# Ensure the target variable is a factor
preprocessed_train_data$o_bullied <- as.factor(preprocessed_train_data$o_bullied)
preprocessed_test_data$o_bullied <- as.factor(preprocessed_test_data$o_bullied)

# Repeat 5-fold cross-validation 5 times
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                              summaryFunction = defaultSummary)
C50_tree <- train(o_bullied ~ ., data = preprocessed_train_data, method = "C5.0", trControl = train_control)

# Without cross-validation
# C50_tree <- C5.0(o_bullied ~ ., data = preprocessed_train_data)

plot(C50_tree)
# Test 
pred <- predict(C50_tree, newdata = preprocessed_test_data, type = "raw")  # Change type to "class" when not using cross-validation
performance_measures <- confusionMatrix(data = pred,  
                                        reference = preprocessed_test_data$o_bullied)
performance_measures


#####
#Class 0 = 88.82%
#Class 1 = 80.88
#####
