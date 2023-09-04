# Load necessary libraries
library(caret)
library(class)
library(corrplot)
library(caTools)

# Load the dataset
dataset <- read.csv("winequality-red.csv")
winedataset <- dataset

# Check for missing values
is.na(winedataset)
sum(is.na(winedataset))

# Explore the dataset
str(winedataset)

# Number of instances
num_instances <- nrow(winedataset)
cat("Number of instances:", num_instances, "\n")

# Number of unique classes
class_no <- length(unique(winedataset$quality))
print(class_no)

# Convert quality column to numeric
winedataset$quality <- as.numeric(winedataset$quality)

# Normalize attributes
n_attributes <- winedataset[,-12]
min_max_apply <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  scaled <- (x - min_val) / (max_val - min_val)
  return(scaled)
}
n_done_attributes <- apply(n_attributes, 2 , min_max_apply)
normalized_mydataset <- cbind.data.frame(n_done_attributes, quality = winedataset$quality)

# Calculate correlation matrix
correlation <- cor(winedataset)
print(correlation)

# Plot correlation matrix
corrplot(correlation, method = "square")

# Split the dataset into training and testing sets
set.seed(123)  # for reproducibility
sample <- sample.split(normalized_mydataset, SplitRatio = 0.7)
trainDataset <- subset(normalized_mydataset, sample == TRUE)
testDataset <- subset(normalized_mydataset, sample == FALSE)

# Perform 10-fold cross-validation
set.seed(123)  # for reproducibility
folds <- createFolds(trainDataset$quality, k = 10, list = TRUE)

# Initialize vectors to store results
accuracy_vector <- numeric(10)
precision_matrix <- matrix(0, nrow = 6, ncol = 10)
recall_matrix <- matrix(0, nrow = 6, ncol = 10)

# Perform cross-validation
for (i in 1:10) {
  # Get training and testing folds
  train_indices <- unlist(folds[i])
  trainFold <- trainDataset[train_indices, ]
  testFold <- trainDataset[-train_indices, ]
  
  # Train the KNN model on the training fold
  trainKnn <- trainFold[,-12]
  testKnn <- testFold[,-12]
  
  knn <- knn(train = trainKnn, test = testKnn, cl = trainFold$quality, k = 10)
  
  # Calculate accuracy
  accuracy_vector[i] <- 100 * sum(testFold$quality == knn) / nrow(testFold)
  
  # Calculate confusion matrix for precision and recall
  confusion_Matrix <- table(testFold$quality, knn)
  
  for (j in 1:6) {
    precision_matrix[j, i] <- calculate_precision(confusion_Matrix, j)
    recall_matrix[j, i] <- calculate_recall(confusion_Matrix, j)
  }
}

# Calculate average accuracy
avg_accuracy <- mean(accuracy_vector)
cat("Average Accuracy:", avg_accuracy, "\n")

# Calculate average precision and recall
avg_precision <- colMeans(precision_matrix)
avg_recall <- colMeans(recall_matrix)

precision_results <- data.frame(Class = class_labels, Precision = avg_precision)
recall_results <- data.frame(Class = class_labels, Recall = avg_recall)

print(precision_results)
print(recall_results)

# Function to calculate precision
calculate_precision <- function(conf_matrix, class_index) {
  TP <- conf_matrix[class_index, class_index]
  FP <- sum(conf_matrix[, class_index]) - TP
  
  if (TP + FP == 0) {
    precision <- 0  
  } else {
    precision <- TP / (TP + FP)
  }
  
  return(precision)
}

# Function to calculate recall
calculate_recall <- function(conf_matrix, class_index) {
  TP <- conf_matrix[class_index, class_index]
  FN <- sum(conf_matrix[class_index, ]) - TP
  
  if (TP + FN == 0) {
    recall <- 0  
  } else {
    recall <- TP / (TP + FN)
  }
  
  return(recall)
}

# Define class labels
class_labels <- c(3, 4, 5, 6, 7, 8)

# Print final results
cat("Average Precision:\n")
print(precision_results)

cat("Average Recall:\n")
print(recall_results)
