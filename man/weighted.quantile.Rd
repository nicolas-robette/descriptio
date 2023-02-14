\name{weighted.quantile}
\alias{weighted.quantile}

\title{Weighted quantiles}

\description{
Computes the weighted quantiles of a distribution.
}
\usage{
weighted.quantile(x, weights = NULL, probs = seq(0, 1, 0.25),
                  na.rm = FALSE, names = FALSE)
}
\arguments{
  \item{x}{numeric vector whose sample quantiles are wanted}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{probs}{numeric vector of probabilities with values in [0,1] }
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
  \item{names}{logical. if TRUE, the result has a names attribute. Default is FALSE.}
}

\value{
A numeric vector of the same length as probs.
}

\note{
This function is taken from https://stackoverflow.com/questions/2748725/is-there-a-weighted-median-function
}

\seealso{
\code{\link{weighted.mad}}
}

\examples{
data(Movies)
weighted.quantile(Movies$Critics, weights = rep(c(.8,1.2), 500), names = TRUE)
}

\keyword{ multivariate }
\keyword{ misc }
