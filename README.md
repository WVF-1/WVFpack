## Overview
WVFpack is an R package designed to assist data scientists and statisticians in selecting optimal parameters for non-parametric regression models. Specifically, it provides tools to determine the best span for LOESS models and the best degrees of freedom (df) for smoothing spline models. The package automates the process of parameter tuning by evaluating model performance across a range of parameter values and selecting the one that minimizes the Mean Squared Error (MSE) on a validation set.

## Features
- **Best_Loess_Span:**  Automatically identifies the optimal span parameter for LOESS models by evaluating MSE across a specified range of span values.
- **Best_Smooth_Spline_df:**  Determines the optimal degrees of freedom for smoothing spline models by assessing MSE over a defined range of df values.
- **Flexible Validation Methods:**  Supports various validation techniques, including k-fold cross-validation and leave one out cross validation (LOOCV), to assess model performance.
- **Customizable Parameter Sequences:**  Allows users to define custom sequences of span or df values to explore during the optimization process.

## Usage
Easy to understand and reproducible examples of each function within the package.

# Best_Loess_Span
```r
library(WVFpack)

# Example usage
optimal_span <- Best_Loess_Span(data = my_data,
                                x = "predictor_variable",
                                y = "response_variable",
                                span_seq = seq(1, 0.5, -0.05),
                                valid_method = "LOOCV")

# Create a Loess model with the optimal span parameter
loess.mod <- loess(response_variable ~ predictor_variable, data = my_data, span = optimal_span)
summary(loess.mod)
```

# Best_Smooth_Spline_df
```r
library(WVFpack)

# Example usage
optimal_df <- Best_Smooth_Spline_df(data = my_data,
                                    x = "predictor_variable",
                                    y = "response_variable",
                                    df_seq = seq(100, 50, -10),
                                    valid_method = "kfold")

# Create a smooth spline model with the optimal df parameter
smooth.mod <- smooth.spline(predictor_variable, response_variable, data = my_data, df = optimal_df)
summary(smooth.mod)
```
