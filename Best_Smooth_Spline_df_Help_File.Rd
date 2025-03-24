\name{Best_Smooth_Spline_df}
\alias{Best Smooth Spline df}
\title{
Best Smooth.Spline df
}
\description{
In the best smooth.spline df function, in the WVFpack package, a dataset with two defined variables, a sequence of df values, and a preferred validation method will be taken in. Then, an automatic function will run all possible models, under the values of the sequence as well as the validation method, in order to determine the best df value to use. This is determined by using the least Mean Square Error (MSE) methodology. The logic behing this it to determine what span value is most statistically important to use, when creating a smooth.spline model.
}
\usage{
Best_Smooth_Spline_df(data = df, x = X, y = Y, df_seq = seq(100,50,-10), valid_method = "Standard")
}

\arguments{
  \item{data}{
    This argument takes in a predefined data frame, to use for the calcutions.
}
  \item{x}{
    This argument takes in a specified x variable from the data frame, defined in data. This will be used as the x variable in the simulated loess models. This should be a numeric variable, such as cost, or a ratio. 
  }
  \item{y}{
  This argument takes in a specified y varible from the data frame, defined in data. This will be used as the y variable in the simulated loess models. This should be a numeric variable, so nothing data wise of categorical. For example, May 13th is a date and not an accepted variable.
  }
  \item{df_seq}{
  This argument uses a sequence vector of df values you wish to test, in decending order. For example: c(100, 75, 50, 25). Alternatively, the argument can be defined as follows: seq(100,25,-25). The more values you try to test over, the more precise your best smooth.spline model df can become, but this could lead to errors or signifcantly longer run times. This function has a minimum df sequence value of 20, as anything lower will result in errors in the predictions. The same maximum in the smooth.spline function of 100, holds here as well. 
  }
  \item{valid_method}{
  This arugment takes in what validation testing method you'd like to use, for the analysis of the span values. Your options are: "Standard", "Valid", or "LOOCV". The defualt method used for this is "Standard", which is also recommended for this process, as it tends to result in the most accurate analysis of the data. The differnece between these three is the manner in whihc the data is split for validating the predictions. "Standard" breaks it into an 80-20 split, "Valid" breaks it into a 50-50 split (This is best for very small data sets of less than 100 values), and "LOOCV" which does not split the data, rather it looks at each row of values individually (This is best for large dat sets, especially when biased is present). To give a concrete example, when dealing with the population of Poland, you would want to use "LOOCV". In the same vain, when dealing with the number of trains which depart from a Sanderson Staion TX, you would want to use "Valid". For everything in between these, we'd look to simply use the defaul method "Standard".
  }
}

\details{
From the useage section:
In this use case, we can see a data frame "df" being define, as well as "X" and "Y" being the two variables we want to model over. Additionally, we're using a standard range of df values from 20 to 100. The function will return the df value in the span_seq vector, which corresponds with the lowest MSE value.
}
\value{
The value that is returned from this function, provided an error is not returned, is the best value from the df_seq vector. This is the df value which should be utilized when modeling with a        smooth.spline model.
}
\references{
I'll put this in APA in a minute, please be patient. 

An Introduction to Statistical Learning: with Applications in R
Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani
}
\author{
William V. Fullerton, loveable math nerd who took the comment "don't worry about that" way too far, and worried about it so much, he made an entire R package about it.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Best_Smooth_Spline_df(data = df, x = X, y = Y, df_seq = seq(100,40,-10), valid_method = "Valid")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
