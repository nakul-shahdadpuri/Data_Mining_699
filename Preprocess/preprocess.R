library("caret")
library("ROSE")

set.seed(123)
project_dataset <- read.csv("./Preprocess/project_dataset.csv", header=TRUE)

colnames(project_dataset)

# Removing all columns which are duplicates

df <- project_dataset[, !duplicated(t(project_dataset))]

# 5 columns were duplicates and thus were removed.


# Removing columns which are highly correlated with each other

# running NearzeroVar function to identify irrelevant columns which have variance in data close to 0

columns_with_xero_var <- nearZeroVar(df, names = TRUE)
columns_with_xero_var
new_df <- df[, !(names(df) %in% columns_with_xero_var)]

# Finding Correlation and removing columns which are highly correlated. Threshold is 0.8.

cor_matrix <- cor(df[, !(names(df) == "o_bullied")])
threshold <- 0.8

high_cor <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
highly_correlated_vars <- unique(c(names(df)[!(names(df) == "o_bullied")][high_cor[, 1]], names(df)[!(names(df) == "o_bullied")][high_cor[, 2]]))
length(highly_correlated_vars)

# Drop the columns which are highly correlated
clean_df <- new_df[ , !(names(new_df) %in% highly_correlated_vars)]


# Using Data Exploration and dropped the following columns which had a disproptionate number of null values
columns_to_drop = c("V2025","V2120","V2121","VS0021","V2022","V2023","V2125","V2126B","V2128B","V3062","V3064", "VS0028")
df_new <- clean_df[, !(names(clean_df) %in% columns_to_drop)]

# Boruta Selection done to determine the columns to remove
reject_list <- c("V2025A","V2036", "V2078","V2124","V2127B", "V2129", "V3023A", "V3061", "VS0019", "VS0020", "VS0036", "VS0037", "VS0038", "VS0039", "VS0041", "VS0042", "VS0043", "VS0045")
df_new <- df_new[, !(names(df_new) %in% reject_list)]


# Perform oversampling of the minority class (SMOTE) and random undersampling of the majority class
balanced_train_data <- ovun.sample(o_bullied ~ ., data = df_new, method = "both", p = 0.4)

# Check class distribution in the balanced training set
table(balanced_train_data$data$o_bullied)

length(colnames(df_new))


# Sampling

# Clean Pre proccessed Dataset with 38 columns (reduced from 204)
write.csv(balanced_train_data$data,"./Datasets/preprocessed_dataset.csv", row.names=FALSE)



