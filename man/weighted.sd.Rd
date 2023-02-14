\name{weighted.sd}
\alias{weighted.sd}

\title{Weighted standard deviation}

\description{
Computes the weighted standard deviation of a distribution.
}
\usage{
weighted.sd(x, weights = NULL, na.rm = FALSE)
}
\arguments{
  \item{x}{numeric vector}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.cor}}
}

\examples{
data(Movies)
weighted.sd(Movies$Critics, weights = rep(c(.8,1.2), 500))
}

\keyword{ multivariate }
\keyword{ misc }
