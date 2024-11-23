#Load and Preprocess Data
# Load dataset
file_path <- "data/optdigits.csv"
optdigits_data <- read.csv(file_path)
colnames(optdigits_data) <- c(paste0("pixel_", 1:64), "label")
optdigits_data$label <- as.factor(optdigits_data$label)


# Split into training and test sets
set.seed(123)
n <- nrow(optdigits_data)
# Generate indices
train_indices <- sample(1:n, size = 0.5 * n)  # 50% for training
remaining_indices <- setdiff(1:n, train_indices)  # Remaining 50%
test_indices <- sample(remaining_indices, size = 0.25 * n)  # 25% for testing
validation_indices <- setdiff(remaining_indices, test_indices)  # Remaining 25% for validation

# Subset the data
train_data <- optdigits_data[train_indices, ]
test_data <- optdigits_data[test_indices, ]
validation_data <- optdigits_data[validation_indices, ]

# Train the KNN classifier
knn_model <- kknn(label ~ ., train = train_data, test = train_data, k = 30, kernel = "rectangular")


# Predictions and performance
train_predictions <- fitted(knn_model)
conf_matrix_train <- table(Actual = train_data$label, Predicted = train_predictions)
print("Confusion Matrix (Training):")
print(conf_matrix_train)

# Predict on test data
test_predictions <- fitted(kknn(label ~ ., train = train_data, test = test_data, k = 30, kernel = "rectangular"))
conf_matrix_test <- table(Actual = test_data$label, Predicted = test_predictions)
print("Confusion Matrix (Test):")
print(conf_matrix_test)


# Misclassification error for training data
train_total <- sum(conf_matrix_train)  # Total instances in training data
train_correct <- sum(diag(conf_matrix_train))  # Correctly classified instances (sum of diagonal)
train_misclassification_error <- 1 - train_correct / train_total

cat("Training Misclassification Error:", train_misclassification_error, "\n")

# Misclassification error for test data
test_total <- sum(conf_matrix_test)  # Total instances in training data
test_correct <- sum(diag(conf_matrix_test))  # Correctly classified instances (sum of diagonal)
test_misclassification_error <- 1 - test_correct / test_total

cat("Test Misclassification Error:", test_misclassification_error, "\n")


# Get predicted probabilities for training data
train_probabilities <- knn_model$prob  # Matrix of probabilities
train_actual <- train_data$label  # Actual labels for training data

# Extract rows corresponding to digit "8" in the training data
digit_8_indices <- which(train_actual == "8")
digit_8_probs <- train_probabilities[digit_8_indices, ]  # Probabilities for digit "8"

digit_8_predicted_probs <- digit_8_probs[, "8"]

easiest_indices <- digit_8_indices[order(digit_8_predicted_probs, decreasing = TRUE)[1:2]]
hardest_indices <- digit_8_indices[order(digit_8_predicted_probs, decreasing = FALSE)[1:3]]

# Extract feature data (pixels) for easiest and hardest cases
easiest_features <- train_data[easiest_indices, 1:64]  # Features for easiest cases
hardest_features <- train_data[hardest_indices, 1:64]  # Features for hardest cases

# Reshape into 8x8 matrices
# Reshape and store as numeric matrices
easiest_matrices <- lapply(1:nrow(easiest_features), function(i) {
  matrix(as.numeric(easiest_features[i, ]), nrow = 8, byrow = TRUE)
})

hardest_matrices <- lapply(1:nrow(hardest_features), function(i) {
  matrix(as.numeric(hardest_features[i, ]), nrow = 8, byrow = TRUE)
})

# Visualize easiest cases
heatmap(easiest_matrices[[1]], Colv = NA, Rowv = NA, scale = "none", main = paste("Easiest Case", i), xlab = "", ylab = "")
heatmap(easiest_matrices[[2]], Colv = NA, Rowv = NA, scale = "none", main = paste("Easiest Case", i), xlab = "", ylab = "")

# Visualize hardest cases
heatmap(hardest_matrices[[1]], Colv = NA, Rowv = NA, scale = "none", main = paste("Hardest Case", i), xlab = "", ylab = "")
heatmap(hardest_matrices[[2]], Colv = NA, Rowv = NA, scale = "none", main = paste("Hardest Case", i), xlab = "", ylab = "")
heatmap(hardest_matrices[[3]], Colv = NA, Rowv = NA, scale = "none", main = paste("Hardest Case", i), xlab = "", ylab = "")







# Initialize vectors to store errors
k_values <- 1:30  # Values of K to test
train_errors <- numeric(length(k_values))  # Training errors
validation_errors <- numeric(length(k_values))  # Validation errors

# Loop through each value of K
for (k in k_values) {
  # Train KNN model on training data
  knn_model <- kknn(label ~ ., train = train_data, test = train_data, k = k, kernel = "rectangular")
  train_predictions <- fitted(knn_model)
  
  # Calculate training misclassification error
  conf_matrix_train <- table(Actual = train_data$label, Predicted = train_predictions)
  train_correct <- sum(diag(conf_matrix_train))
  train_total <- sum(conf_matrix_train)
  train_errors[k] <- 1 - train_correct / train_total
  
  # Validate on validation data
  validation_predictions <- fitted(kknn(label ~ ., train = train_data, test = validation_data, k = k, kernel = "rectangular"))
  conf_matrix_validation <- table(Actual = validation_data$label, Predicted = validation_predictions)
  validation_correct <- sum(diag(conf_matrix_validation))
  validation_total <- sum(conf_matrix_validation)
  validation_errors[k] <- 1 - validation_correct / validation_total
}

# Plot training and validation errors with improvements
plot(k_values, train_errors, type = "b", col = "blue", pch = 19, 
     ylim = c(0, max(c(train_errors, validation_errors)) * 1.1), 
     xlab = "Number of Neighbors (K)", 
     ylab = "Misclassification Error", 
     main = "Training and Validation Errors vs. K in KNN")

lines(k_values, validation_errors, type = "b", col = "red", pch = 19)

# Add gridlines
abline(h = seq(0, max(c(train_errors, validation_errors)), by = 0.01), col = "gray", lty = 2)
abline(v = seq(1, 30, by = 1), col = "gray", lty = 2)

# Highlight optimal K
optimal_k <- which.min(validation_errors)
points(optimal_k, validation_errors[optimal_k], col = "darkgreen", pch = 19, cex = 1.5)
text(optimal_k, validation_errors[optimal_k], labels = paste("Optimal K =", optimal_k), pos = 3)

# Add legend
legend("topright", legend = c("Training Error", "Validation Error"), 
       col = c("blue", "red"), pch = 19, lty = 1, bty = "n")



# Train the model with optimal K
optimal_k <- 3
knn_model_optimal <- kknn(label ~ ., train = train_data, test = test_data, k = optimal_k, kernel = "rectangular")

# Predictions and confusion matrix
test_predictions <- fitted(knn_model_optimal)
conf_matrix_test <- table(Actual = test_data$label, Predicted = test_predictions)

# Calculate test misclassification error
test_total <- sum(conf_matrix_test)
test_correct <- sum(diag(conf_matrix_test))
test_misclassification_error <- 1 - test_correct / test_total

cat("Test Misclassification Error for K =", optimal_k, ":", test_misclassification_error, "\n")



# Initialize vector to store cross-entropy errors
cross_entropy_errors <- numeric(length(k_values))

# Loop through each value of K
for (k in k_values) {
  # Train KNN model and get predicted probabilities for validation data
  knn_model <- kknn(label ~ ., train = train_data, test = validation_data, k = k, kernel = "rectangular")
  predicted_probs <- knn_model$prob  # Predicted probabilities
  
  # Ensure probabilities are numeric (to avoid issues)
  predicted_probs <- as.matrix(predicted_probs)
  
  # Actual labels for the validation data (one-hot encoded)
  actual_labels <- model.matrix(~ label - 1, data = validation_data)
  
  # Add a small constant to avoid log(0) issues
  predicted_probs <- predicted_probs + 1e-15
  
  # Compute cross-entropy loss
  cross_entropy_loss <- -sum(actual_labels * log(predicted_probs)) / nrow(validation_data)
  
  # Store the cross-entropy loss
  cross_entropy_errors[k] <- cross_entropy_loss
}

# Plot cross-entropy errors for validation data
plot(k_values, cross_entropy_errors, type = "b", col = "blue", pch = 19,
     xlab = "Number of Neighbors (K)", ylab = "Cross-Entropy Error",
     main = "Validation Cross-Entropy Error vs. K")

# Highlight optimal K
optimal_k_cross_entropy <- which.min(cross_entropy_errors)
optimal_cross_entropy_value <- cross_entropy_errors[optimal_k_cross_entropy]

# Mark the optimal K and cross-entropy value
points(optimal_k_cross_entropy, optimal_cross_entropy_value, col = "darkgreen", pch = 19, cex = 1.5)
text(optimal_k_cross_entropy, optimal_cross_entropy_value, 
     labels = paste("Optimal K =", optimal_k_cross_entropy, "\nError =", round(optimal_cross_entropy_value, 4)), 
     pos = 3)

# Add gridlines for better visualization
abline(h = seq(0, max(cross_entropy_errors), by = 0.01), col = "gray", lty = 2)
abline(v = seq(1, 30, by = 1), col = "gray", lty = 2)

