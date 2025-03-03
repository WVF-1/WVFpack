Best_Loess_Span <- function(data, x, y, span_seq = seq(1,0.5,-0.05), valid_method = "Standard") {
  # The default method for creating a validation and training set is "Standard", meaning an     80-20 split.

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

  # Checks for too small a span value
  if (min(span_seq) < 0.2) {
    stop("Smallest span_seq value is too small.")
  }

  # Defualt validation method
  if (valid_method == "Standard"){
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

    #For loop for checking all of the span values against their MSEs per Loess model
    for(i in seq_along(span_seq)){
      loess_model <- loess(formula, data = training_data, span = span_seq[i]) #Loess model

      # Find the predicted values using the testing data
      pred <- predict(loess_model, testing_data)

      # Isolate the actual y variable in the testing data set
      actual <- testing_data[[y]]

      # Find and save the MSE values to their respective i index value
      MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
    }
  }

  # Small data set 50-50 validation method
  if (valid_method == "Valid"){
    # Create storage vector for comparison of MSE values
    MSE <- numeric(length(span_seq))

    # Extract the specified columns and return as a new data frame
    result <- data[, c(x, y), drop = FALSE]

    # Create the training set (50% of the data)
    set.seed(123)  # For reproducibility
    train_indices <- 1:floor(nrow(result) * 0.5)
    training_data <- result[train_indices, ]

    # Create the testing set (remaining 50% of the data)
    testing_data <- result[-train_indices, ]

    # Set formula for the loess model
    formula <- as.formula(paste(y, "~", x))

    #For loop for checking all of the span values against their MSEs per Loess model
    for(i in seq_along(span_seq)){
      loess_model <- loess(formula, data = training_data, span = span_seq[i]) #Loess model

      # Find the predicted values using the testing data
      pred <- predict(loess_model, testing_data)

      # Isolate the actual y variable in the testing data set
      actual <- testing_data[[y]]

      # Find and save the MSE values to their respective i index value
      MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
    }
  }

  # Leave-One-Out Cross Validation method
  if (valid_method == "LOOCV"){
    # Create storage vector for comparison of MSE values
    MSE <- numeric(length(span_seq))

    # Extract the specified columns and return as a new data frame
    result <- data[, c(x, y), drop = FALSE]

    # Create for loop for systematically leaving on row out
    for(i in (1:floor(nrow(result)))){
      testing_data <- result[i,]
      training_data <- result[-i,]

    # Set formula for the loess model
    formula <- as.formula(paste(y, "~", x))

    #For loop for checking all of the span values against their MSEs per Loess model
    for(i in seq_along(span_seq)){
      loess_model <- loess(formula, data = training_data, span = span_seq[i]) #Loess model

      # Find the predicted values using the testing data
      pred <- predict(loess_model, testing_data)

      # Isolate the actual y variable in the testing data set
      actual <- testing_data[[y]]

      # Find and save the MSE values to their respective i index value
      MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
      }
    }
  }

  # Leave-One-Out Cross Validation method
  if (valid_method == "LOOCV"){
    # Create storage vector for comparison of MSE values
    MSE <- numeric(length(span_seq))

    # Extract the specified columns and return as a new data frame
    result <- data[, c(x, y), drop = FALSE]

    # Create for loop for systematically leaving on row out
    for(i in (1:floor(nrow(result)))){
      testing_data <- result[i,]
      training_data <- result[-i,]

    # Set formula for the loess model
    formula <- as.formula(paste(y, "~", x))

    #For loop for checking all of the span values against their MSEs per Loess model
    for(i in seq_along(span_seq)){
      loess_model <- loess(formula, data = training_data, span = span_seq[i]) #Loess model

      # Find the predicted values using the testing data
      pred <- predict(loess_model, testing_data)

      # Isolate the actual y variable in the testing data set
      actual <- testing_data[[y]]

      # Find and save the MSE values to their respective i index value
      MSE[i] <- mean((actual - pred)^2, na.rm = TRUE)
      }
    }
  }

  #Created a data frame that connects each span value with its respective MSE value
  best_span <- data.frame(span = span_seq, MSE = MSE)

  #Return the best Span value
  return(best_span$span[best_span$MSE == min(best_span$MSE)])
}

Best_Loess_Span(data = mtcars, x = "mpg", y = "hp", valid_method = "Valid")

Best_Smooth_Spline_df <- function(data, x, y, df_seq = seq(100,50,-10), valid_method = "Standard") {
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

  # Checks for too small a df value
  if (min(df_seq) < 20) {
    stop("Smallest df_seq value is too small.")
  }

  #Default validation of 80-20 split
  if (valid_method == "Standard") {
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
  }
  # Small data set 50-50 validation method
  if (valid_method == "Valid"){
    # Create storage vector for comparison of MSE values
    MSE <- numeric(length(df_seq))

    # Extract the specified columns and return as a new data frame
    result <- data[, c(x, y), drop = FALSE]

    # Create the training set (50% of the data)
    set.seed(123)  # For reproducibility
    train_indices <- 1:floor(nrow(result) * 0.5)
    training_data <- result[train_indices, ]

    # Create the testing set (remaining 50% of the data)
    testing_data <- result[-train_indices, ]

    # Set formula for the loess model
    formula <- as.formula(paste(y, "~", x))

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
  }

  # Leave-One-Out Cross Validation method
  if (valid_method == "LOOCV"){
    # Create storage vector for comparison of MSE values
    MSE <- numeric(length(df_seq))

    # Extract the specified columns and return as a new data frame
    result <- data[, c(x, y), drop = FALSE]

    # Create for loop for systematically leaving on row out
    for(i in (1:floor(nrow(result)))){
      testing_data <- result[i,]
      training_data <- result[-i,]

      # Set formula for the loess model
      formula <- as.formula(paste(y, "~", x))

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
    }
  }

  #Created a data frame that connects each span value with its respective MSE value
  best_df <- data.frame(df = df_seq, MSE = MSE)

  #Return the best Span value
  return(best_df$df[best_df$MSE == min(best_df$MSE)])
}

Best_Smooth_Spline_df(cars, "mpg", "hp", valid_method = "LOOCV")
