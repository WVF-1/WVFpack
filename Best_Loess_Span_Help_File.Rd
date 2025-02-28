\name{Best_Loess_Span}
\alias{Best Loess Span}
\title{
Best Loess Span
}
\description{
In the best loess span function, in the WVFpack package, a data set with two defined variables, a sequence of span values, and a preferred validation method will be taken in. Then, an automatic function will run all possible models, under the values of the sequence as well as the validation method, in order to determine the best span value to use. This is determined by using the least Mean Square Error (MSE) methodology. The logic behing this it to determine what span value is most statistically important to use, when creating a loess model.
}
\usage{
Best_Loess_Span(data = df, x = X, y = Y, span_seq = seq(1,0.1,-0.5))
}

\arguments{
  \item{data}{
    This argument takes in a predefined data frame, to use for the calcutions.
}
  \item{x}{
    This argument takes in a specified x variable from the data frame, defined in data. This will be used as the x variable in the simulated loess models.
  }
  \item{y}{
  This argument takes in a specified y varible from the data frame, defined in data. This will be used as the y variable in the simulated loess models.
  }
  \item{span_seq}{
  This argument uses a sequence vector of span values you wish to test, between 0 to 1, in decending order. For example: c(1, 0.75, 0.5, 0.25, 0). Alternatively, the argument can be defined as follows: seq(1,0,-0.25). The more values you try to test over, the more precise your best loess model span can become, but this could lead to errors or signifcantly longer run times. 
  }
}
\details{
It is most efficient to have the span_seq variable defined in descending order. Meaning, that rather from going 0 to 1, you will want to go from 1 to 0. This makes it easier for the function to crawl through the MSE values of their respective span values, in order to give the actual lowest variabel.

From the useage section:
In this use case, we can see a data frame "df" being define, as well as "X" and "Y" being the two variables we want to model over. Additionally, we're using a standard range of span values from 0.1 to 1. The function will return the span value in the span_seq vector, which corresponds with the lowest MSE value. 

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
Best_Loess_Span(data = df, x = X, y = Y, span_seq = seq(1,0.1,-0.5)) 
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
