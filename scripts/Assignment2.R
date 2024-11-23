scale_features <- function(data) {
  data_scaled <- data
  data_scaled[ , -which(names(data) == "motor_UPDRS")] <- scale(data[ , -which(names(data) == "motor_UPDRS")])
  return(data_scaled)
}

# Define the Loglikelihood function
Loglikelihood <- function(theta, sigma, X, y) {
  n <- nrow(X)
  y_pred <- X %*% theta
  residual_sum_squares <- sum((y - y_pred)^2)
  
  log_likelihood <- - (n / 2) * log(2 * pi) - (n / 2) * log(sigma^2) - 
    (1 / (2 * sigma^2)) * residual_sum_squares
  return(log_likelihood)
}

# Define Ridge function
Ridge <- function(theta, sigma, X, y, lambda) {
  log_likelihood <- Loglikelihood(theta, sigma, X, y)
  ridge_penalty <- lambda * sum(theta[-1]^2)  # Exclude intercept
  return(log_likelihood - ridge_penalty)
}

# Define Ridge optimization function
RidgeOpt <- function(X, y, lambda) {
  initial_theta <- rep(0, ncol(X))  # Initialize theta to zeros
  initial_sigma <- 1               # Initial guess for sigma
  initial_values <- c(initial_theta, initial_sigma)
  
  objective_function <- function(params) {
    theta <- params[1:(length(params) - 1)]
    sigma <- params[length(params)]
    return(-Ridge(theta, sigma, X, y, lambda))
  }
  
  # Minimize the negative Ridge log-likelihood
  result <- optim(par = initial_values, fn = objective_function, method = "BFGS")
  
  optimized_theta <- result$par[1:(length(result$par) - 1)]
  optimized_sigma <- result$par[length(result$par)]
  
  return(list(theta = optimized_theta, sigma = optimized_sigma, value = result$value))
}

# Function to calculate degrees of freedom for Ridge regression
DF <- function(X, lambda) {
  p <- ncol(X)
  I <- diag(p)
  XtX <- t(X) %*% X
  ridge_matrix <- solve(XtX + lambda * I)
  H <- X %*% ridge_matrix %*% t(X)
  return(sum(diag(H)))
}


#Load and Preprocess Data
# Load dataset
file_path <- "data/parkinsons.csv"
parkinson_data <- read.csv(file_path)

# Split into training and test sets
set.seed(123)
n <- nrow(parkinson_data)
train_indices <- sample(1:n, size = 0.6 * n)
train_data <- parkinson_data[train_indices, ]
test_data <- parkinson_data[-train_indices, ]

# Scale features
train_data_scaled <- scale_features(train_data)
test_data_scaled <- scale_features(test_data)


#Train Linear Model
# Fit linear model
linear_model <- lm(motor_UPDRS ~ ., data = train_data_scaled)
# Predictions
train_predictions <- predict(linear_model, train_data_scaled)
test_predictions <- predict(linear_model, test_data_scaled)

# Calculate MSE
train_mse <- mean((train_data_scaled$motor_UPDRS - train_predictions)^2)
test_mse <- mean((test_data_scaled$motor_UPDRS - test_predictions)^2)

# Display MSE
cat("Train MSE:", train_mse, "\n") 
cat("Test MSE:", test_mse, "\n") 



#Ridge Regression
# Prepare data for Ridge regression
X <- as.matrix(cbind(1, train_data_scaled[, -which(names(train_data_scaled) == "motor_UPDRS")]))
y <- train_data_scaled$motor_UPDRS

# Initial log-likelihood
theta_initial <- coef(linear_model)
sigma_initial <- summary(linear_model)$sigma
log_likelihood_value <- Loglikelihood(theta_initial, sigma_initial, X, y)
cat("Log-Likelihood (Linear Model):", log_likelihood_value, "\n")

# Ridge optimization with λ = 0.001
lambda <- 0.001
ridge_optimization_result <- RidgeOpt(X, y, lambda)
cat("Optimized Theta (coefficients):\n", ridge_optimization_result$theta, "\n")
cat("Optimized Sigma (residual standard error):\n", ridge_optimization_result$sigma, "\n")
cat("Minimized Negative Log-Likelihood:\n", ridge_optimization_result$value, "\n")


#Degrees of Freedom Analysis
# Degrees of Freedom for Ridge regression
lambda <- 10
degrees_of_freedom <- DF(X, lambda)
#cat("Degrees of Freedom for Ridge Regression (λ =", lambda, "):", degrees_of_freedom, "\n")

# Compute predictions and MSE
predict_ridge <- function(X, y, theta) {
  y_pred <- X %*% theta
  mse <- mean((y - y_pred)^2)
  return(list(predictions = y_pred, mse = mse))
}

# Perform Ridge regression and compute metrics for given lambda values
lambda_values <- c(1, 100, 1000)
results <- list()

for (lambda in lambda_values) {
  # Optimize Ridge parameters
  ridge_result <- RidgeOpt(X, y, lambda)
  
  # Extract optimized parameters
  optimized_theta <- ridge_result$theta
  optimized_sigma <- ridge_result$sigma
  
  # Predict on training and test data
  train_result <- predict_ridge(X, y, optimized_theta)
  test_result <- predict_ridge(as.matrix(cbind(1, test_data_scaled[, -which(names(test_data_scaled) == "motor_UPDRS")])), 
                               test_data_scaled$motor_UPDRS, optimized_theta)
  
  # Compute Degrees of Freedom
  degrees_of_freedom <- DF(X, lambda)
  
  # Store results
  results[[as.character(lambda)]] <- list(
    lambda = lambda,
    optimized_theta = optimized_theta,
    optimized_sigma = optimized_sigma,
    train_mse = train_result$mse,
    test_mse = test_result$mse,
    degrees_of_freedom = degrees_of_freedom
  )
}

#Display results
for (lambda in names(results)) {
  result <- results[[lambda]]
  at("Lambda =", result$lambda, "\n")
  cat("Train MSE:", result$train_mse, "\n")
  cat("Test MSE:", result$test_mse, "\n")
  cat("Degrees of Freedom:", result$degrees_of_freedom, "\n")
  cat("Optimized Sigma:", result$optimized_sigma, "\n")
  cat("\n")
}