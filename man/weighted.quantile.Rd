\name{weighted.quantile}
\alias{weighted.quantile}

\title{Weighted quantiles}

\description{
Computes the weighted quantiles of a distribution.
}
\usage{
weighted.quantile(x, weights, probs = seq(0, 1, 0.25),
                  na.rm = TRUE, names = FALSE)
}
\arguments{
  \item{x}{numeric vector whose sample quantiles are wanted}
  \item{weights}{Numeric vector of weights}
  \item{probs}{numeric vector of probabilities with values in [0,1] }
  \item{na.rm}{logical. If true, any NA are removed from x before the quantiles are computed.}
  \item{names}{logical. if true, the result has a names attribute.}
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
