# Load necessary libraries
library(caret)
library("caret")
library("ROSE")

library(Boruta)
library(caret)
library(rsample)
library(e1071)
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)
library(neuralnet)  # Add the neuralnet library

project_dataset <- read.csv("./Preprocess/project_dataset.csv", header=TRUE)
colnames(project_dataset)

project_dataset$o_bullied = as.factor(project_dataset$o_bullied)

# Split the dataset into training and testing sets
set.seed(123)  # Set seed for reproducibility
split <- initial_split(project_dataset, prop = 0.8, strata = o_bullied)  # 80% training, 20% testing
train_data <- training(split)
test_data <- testing(split)
# Removing all columns which are duplicates


nrow(test_data)

# 5 columns were duplicates and thus were removed.


# Removing columns which are highly correlated with each other

# running NearzeroVar function to identify irrelevant columns which have variance in data close to 0

columns_with_xero_var_train <- nearZeroVar(train_data, names = TRUE)
columns_with_xero_var_train
train_data <- train_data[, !(names(train_data) %in% columns_with_xero_var_train)]

#columns_with_xero_var_test <- nearZeroVar(test_data, names = TRUE)
#columns_with_xero_var_test
test_data <- test_data[, !(names(test_data) %in% columns_with_xero_var_train)]
nrow(test_data)
# Finding Correlation and removing columns which are highly correlated. Threshold is 0.8.

cor_matrix_train <- cor(train_data[, !(names(train_data) == "o_bullied")])
threshold <- 0.8

high_cor_train <- which(abs(cor_matrix_train) > threshold & cor_matrix_train != 1, arr.ind = TRUE)
highly_correlated_vars_train <- unique(c(names(train_data)[!(names(train_data) == "o_bullied")][high_cor_train[, 1]], names(train_data)[!(names(train_data) == "o_bullied")][high_cor_train[, 2]]))
length(highly_correlated_vars_train)

# Drop the columns which are highly correlated
train_data <- train_data[ , !(names(train_data) %in% highly_correlated_vars_train)]

# Finding Correlation and removing columns which are highly correlated. Threshold is 0.8.

#cor_matrix_test <- cor(test_data[, !(names(test_data) == "o_bullied")])
#threshold <- 0.8

#high_cor_test <- which(abs(cor_matrix_test) > threshold & cor_matrix_test != 1, arr.ind = TRUE)
#highly_correlated_vars_test <- unique(c(names(test_data)[!(names(test_data) == "o_bullied")][high_cor_test[, 1]], names(test_data)[!(names(test_data) == "o_bullied")][high_cor_test[, 2]]))
#length(highly_correlated_vars_test)

# Drop the columns which are highly correlated
test_data <- test_data[ , !(names(test_data) %in% highly_correlated_vars_train)]



# Using Data Exploration and dropped the following columns which had a disproptionate number of null values

columns_to_drop_train = c("V2025","V2120","V2121","VS0021","V2022","V2023","V2125","V2126B","V2128B","V3062","V3064", "VS0028")
train_data <- train_data[, !(names(train_data) %in% columns_to_drop_train)]

columns_to_drop_test = c("V2025","V2120","V2121","VS0021","V2022","V2023","V2125","V2126B","V2128B","V3062","V3064", "VS0028")
test_data <- test_data[, !(names(test_data) %in% columns_to_drop_test)]

#target_variable <- "o_bullied"
#df_features_train <- train_data[, !(names(train_data) == target_variable)]
#target_train <- train_data[[target_variable]]
#boruta_obj_train <- Boruta(df_features_train, target_train)
#x_train <- boruta_obj_train$finalDecision
#train_data <- train_data[, x_train == "Tentative" | x_train == "Confirmed"]
#colnames(train_data)

#df_features_test <- test_data[, !(names(test_data) == target_variable)]
#target_test <- test_data[[target_variable]]
#boruta_obj_test <- Boruta(df_features_test, target_test)
#x <- boruta_obj_test$finalDecision 
#test_data <- test_data[, x_train == "Tentative" | x_train == "Confirmed"]
#colnames(test_data)


reject_list <- c("V2025A","V2036", "V2078","V2124","V2127B", "V2129", "V3023A", "V3061", "VS0019", "VS0020", "VS0036", "VS0037", "VS0038", "VS0039", "VS0041", "VS0042", "VS0043", "VS0045")
train_data <- train_data[, !(names(train_data) %in% reject_list)]
test_data <- test_data[, !(names(test_data) %in% reject_list)]
nrow(test_data)
write.csv(train_data,"./Models/initial_train_wo_sample.csv", row.names=FALSE)
colnames(train_data)

# Perform oversampling of the minority class (SMOTE) and random undersampling of the majority class
balanced_train_data <- ovun.sample(o_bullied ~ ., data = train_data, method = "both", p = 0.5)
nrow(balanced_train_data)
# Check class distribution in the balanced training set
table(balanced_train_data$data$o_bullied)

colnames(balanced_train_data$data)
View(balanced_train_data$data)

table(balanced_train_data$data$o_bullied)
# Clean Pre proccessed Dataset with 63 columns (reduced from 204)
write.csv(balanced_train_data$data,"./Models/initial_train.csv", row.names=FALSE)

write.csv(test_data, "./Models/initial_test.csv", row.names = FALSE)
