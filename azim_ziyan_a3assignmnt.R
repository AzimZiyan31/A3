# Install necessary packages if not already installed
if (!require(caTools)) install.packages('caTools', dependencies=TRUE)
if (!require(MLmetrics)) install.packages('MLmetrics', dependencies=TRUE)
if (!require(pROC)) install.packages('pROC', dependencies=TRUE)
if (!require(rpart)) install.packages('rpart', dependencies=TRUE)
if (!require(rpart.plot)) install.packages('rpart.plot', dependencies=TRUE)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(caTools)
library(MLmetrics)
library(pROC)
library(rpart)
library(rpart.plot)

# Read the dataset
data <- read.csv("D:\\SCMA\\Data\\loan_data.csv")

# Display the column names and structure of the dataset
names(data)
str(data)

# Check unique values in the Loan_Status column
unique(data$Loan_Status)

# Assuming Loan_Status is not binary, convert it to binary (e.g., 'Y' -> 1, 'N' -> 0)
data <- data %>%
  mutate(Loan_Status = ifelse(loan_status == 'Y', 1, 0))

# Replace missing values with the mean for numeric columns
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check for remaining missing values
missing_info <- colSums(is.na(data))
cat("Missing Values Information:\n")
print(missing_info)

# Select only numeric columns for correlation matrix
numeric_columns <- data %>%
  select(where(is.numeric))

# Calculate and print correlation matrix
cor_matrix <- cor(numeric_columns)
print(cor_matrix)
heatmap(cor_matrix)

# Boxplot for numeric columns against Loan_Status
# Replace the following line with actual column names from your dataset
boxplot(Applicant_Income + Coapplicant_Income + Loan_Amount + Loan_Amount_Term + Credit_History ~ Loan_Status, data=data)

# Logistic Regression
set.seed(123)
split <- sample.split(data$Loan_Status, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)
model <- glm(Loan_Status ~ ., data = train, family = binomial)
pred_prob <- predict(model, newdata=test, type="response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

# Confusion Matrix
confusion <- ConfusionMatrix(factor(pred_class), factor(test$Loan_Status))
print(confusion)
roc_obj <- roc(test$Loan_Status, pred_prob)
auc <- auc(roc_obj)
print(paste("AUC-ROC:", auc))
plot(roc_obj, main="ROC Curve", print.auc=TRUE)

# Decision tree analysis
set.seed(123)
split <- sample.split(data$Loan_Status, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)
model <- rpart(Loan_Status ~ ., data=train, method="class")
pred_prob <- predict(model, newdata=test, type="prob")
pred_class <- ifelse(pred_prob[,2] >= 0.5, 1, 0)

# Confusion Matrix for decision tree
confusion <- ConfusionMatrix(factor(pred_class), factor(test$Loan_Status))
print(confusion)
roc_obj <- roc(test$Loan_Status, pred_prob[,2])
auc <- auc(roc_obj)
print(paste("AUC-ROC:", auc))
plot(roc_obj, main="ROC Curve", print.auc=TRUE)

