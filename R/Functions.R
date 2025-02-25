#' Best Loess Span Selecting Function
#'
#' This function selects the best loess span based on the minimum mean squared error (MSE).
#' @param data A data frame containing the data.
#' @param x The name of the predictor variable (as a string).
#' @param y The name of the response variable (as a string).
#' @param span_seq A numeric vector of span values to test.
#' @return The best span value.
#' @export
Best_Loess_Span <- function(data, x, y, span_seq) {
  # Check if the specified columns exist in the data frame
  if (!all(c(x, y) %in% names(data))) {
    stop("One or both of the specified columns do not exist in the data frame.")
  }

  # Check if span_seq is empty
  if (length(span_seq) == 0) {
    stop("span_seq cannot be empty.")
  }

  # Check if the data has enough rows
  if (nrow(data) < 2) {
    stop("Data must have at least two rows.")
  }

  # Create storage vector for comparison of MSE values
  MSE <- numeric(length(span_seq))

  # Extract the specified columns and return as a new data frame
  result <- data[, c(x, y), drop = FALSE]

  # Create the training set (80% of the data)
  set.seed(123)  # For reproducibility
  train_indices <- 1:floor(nrow(result) * 0.8)
  training_data <- result[train_indices, ]

  # Create the testing set (remaining 20% of the data)
  testing_data <- result[-train_indices, ]

  # Set formula for the loess model
  formula <- as.formula(paste(y, "~", x))

  # For loop for checking all of the span values against their MSEs per Loess model
  for(i in seq_along(span_seq)){
    loess_model <- loess(formula, data = training_data, span = span_seq[i]) # Loess model

    # Find the predicted values using the testing data
    pred <- predict(loess_model, testing_data)

    # Isolate the actual y variable in the testing data set
    actual <- testing_data[[y]]

    # Find and save the MSE values to their respective i index value
    MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
  }

  # Create a data frame that connects each span value with its respective MSE value
  best_span <- data.frame(span = span_seq, MSE = MSE)

  # Return the best Span value
  return(best_span$span[best_span$MSE == min(best_span$MSE)])
}

#' Best Smooth Spline df Selecting Function
#'
#' This function selects the best degrees of freedom for a smooth spline based on the minimum mean squared error (MSE).
#' @param data A data frame containing the data.
#' @param x The name of the predictor variable (as a string).
#' @param y The name of the response variable (as a string).
#' @param df_seq A numeric vector of degrees of freedom to test.
#' @return The best degrees of freedom value.
#' @export
Best_Smooth_Spline_df <- function(data, x, y, df_seq) {
  # Check if the specified columns exist in the data frame
  if (!all(c(x, y) %in% names(data))) {
    stop("One or both of the specified columns do not exist in the data frame.")
  }

  # Check if df_seq is empty
  if (length(df_seq) == 0) {
    stop("df_seq cannot be empty.")
  }

  # Check if the data has enough rows
  if (nrow(data) < 2) {
    stop("Data must have at least two rows.")
  }

  # Create storage vector for comparison of MSE values
  MSE <- numeric(length(df_seq))

  # Extract the specified columns and return as a new data frame
  result <- data[, c(x, y), drop = FALSE]

  # Create the training set (80% of the data)
  set.seed(123)  # For reproducibility
  train_indices <- 1:floor(nrow(result) * 0.8)
  training_data <- result[train_indices, ]

  # Create the testing set (remaining 20% of the data)
  testing_data <- result[-train_indices, ]

  #For loop for checking all of the span values against their MSEs per Loess model
  for(i in seq_along(df_seq)){
    smooth_spling_curve <- smooth.spline(x = training_data[[x]], y = training_data[[y]],
                                         df = df_seq[i]) #Smooth spline model

    # Find the predicted values using the testing data
    pred <- predict(smooth_spling_curve, newdata = testing_data[[x]])$y

    # Isolate the actual y variable in the testing data set
    actual <- testing_data[[y]]

    # Find and save the MSE values to their respective i index value
    MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
  }

  #Created a data frame that connects each span value with its respective MSE value
  best_df <- data.frame(df = df_seq, MSE = MSE)

  #Return the best Span value
  return(best_df$df[best_df$MSE == min(best_df$MSE)])
}
