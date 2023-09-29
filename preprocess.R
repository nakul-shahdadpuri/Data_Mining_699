library("caret")
library("Boruta")
library("randomForest")



project_dataset <- read.csv("C:/Users/nakul/Desktop/Boston University/CS699 - Data Mining/Project/project_dataset.csv", header=TRUE)
colnames(project_dataset)
View(project_dataset)


# Removing all columns which are duplicates

df <- project_dataset[, !duplicated(t(project_dataset))]


# No Columns were removed as they were no duplicates
# Removing columns which are highly correlated with each other

# running NearzeroVar function to identify irrelevant columns

columns_with_xero_var <- nearZeroVar(df, names = TRUE)
columns_with_xero_var
new_df <- df[, !(names(df) %in% columns_with_xero_var)]


# Finding Correlation and removing columns which are highly correlated

cor_matrix <- cor(df[, !(names(df) == "o_bullied")])
threshold <- 0.9

high_cor <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
highly_correlated_vars <- unique(c(names(df)[!(names(df) == "o_bullied")][high_cor[, 1]], names(df)[!(names(df) == "o_bullied")][high_cor[, 2]]))
length(highly_correlated_vars)

# Drop the columns
clean_df <- new_df[ , !(names(new_df) %in% highly_correlated_vars)]

# Clean Preproccessed Dataset with 87 columns (reduced from 204)
write.csv(clean_df,"C:/Users/nakul/Desktop/Boston University/CS699 - Data Mining/Project/clean_dataset.csv", row.names=FALSE)

# Using Recursive Attribute Elimination for identifying important attribute

ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)  
results <- rfe(df[, -which(names(df) == "o_bullied")], df$o_bullied, sizes=c(1:ncol(df)-1), rfeControl=ctrl)


