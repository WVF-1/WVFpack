\name{Best_Loess_Span}
\alias{Best Loess Span}
\title{
Best Loess Span
}
\description{
In the best loess span function, in the WVFpack package, a data set with two defined variables, a sequence of span values, and a preferred validation method will be taken in. Then, an automatic function will run all possible models, under the values of the sequence as well as the validation method, in order to determine the best span value to use. This is determined by using the least Mean Square Error (MSE) methodology. The logic behing this it to determine what span value is most statistically important to use, when creating a loess model.
}
\usage{
Best_Loess_Span(data, x, y, span_seq = seq(1,0.5,-0.05), valid_method = "Standard")
}

\arguments{
  \item{data}{
    This argument takes in a predefined data frame, to use for the calcutions.
}
  \item{x}{
    This argument takes in a specified x variable from the data frame, defined in data. This will be used as the x variable in the simulated loess models. This should be a numeric variable, such as the number of planes in an stock pile. 
  }
  \item{y}{
  This argument takes in a specified y varible from the data frame, defined in data. This will be used as the y variable in the simulated loess models. This should be a numeric variable, and not something categorical. As, the quality of someone's love for Poland is not something you should use in this case.
  }
  \item{span_seq}{
  This argument uses a sequence vector of span values you wish to test, between 0.2 to 1, in decending order. For example: c(1, 0.8, 0.6, 0.4, 0.2). Alternatively, the argument can be defined as follows: seq(1,0.2,-0.2). The more values you try to test over, the more precise your best loess model span can become, but this could lead to errors or signifcantly longer run times. For this function, the defualt case is a sequence which follows: seq(1,0.5,-0.05). If you leave this argument blank, these sequence values will be used instead. Additionally, the minimum span value the function will take in is 0.2, as anything lower will result in prediction errors later in your modeling process. The same maximum of 1, as in the typical loess function holds here as well. 
  }
  \item{valid_method}{
  This arugment takes in what validation testing method you'd like to use, for the analysis of the span values. Your options are: "Standard", "Valid", or "LOOCV". The defualt method used for this is "Standard", which is also recommended for this process, as it tends to result in the most accurate analysis of the data. The differnece between these three is the manner in whihc the data is split for validating the predictions. "Standard" breaks it into an 80-20 split, "Valid" breaks it into a 50-50 split (This is best for very small data sets of less than 100 values), and "LOOCV" which does not split the data, rather it looks at each row of values individually (This is best for large dat sets, especially when biased is present). To give a concrete example, when dealing with the population of Poland, you would want to use "LOOCV". In the same vain, when dealing with the number of trains which depart from a Sanderson Staion TX, you would want to use "Valid". For everything in between these, we'd look to simply use the defaul method "Standard".
  }
}
\details{
It is most efficient to have the span_seq variable defined in descending order. Meaning, that rather from going 0.2 to 1, you will want to go from 1 to 0.2. This makes it easier for the function to crawl through the MSE values of their respective span values, in order to give the actual lowest variabel.

From the useage section:
In this use case, we can see a data frame "df" being define, as well as "X" and "Y" being the two variables we want to model over. Additionally, we're not using a standard range of span values from 0.1 to 1. This is because, while the span function will take in values lower than 0.2, they almost always lead to errors in the data. The function will return the span value in the span_seq vector, which corresponds with the lowest MSE value. 

}
\value{
The value that is returned from this function, provided an error is not returned, is the best value from the span_seq vector. This is the span value which should be utilized when modeling with a Loess model.

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
Best_Loess_Span(data = df, x = X, y = Y, span_seq = seq(1,0.2,-0.5), valid_method = "LOOCV") 
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
