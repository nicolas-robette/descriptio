\name{weighted.mad}
\alias{weighted.mad}

\title{Weighted median absolute deviation to median}

\description{
Computes the weighted median absolute deviation to median (aka MAD) of a distribution.
}
\usage{
weighted.mad(x, weights = NULL, na.rm = FALSE)
}
\arguments{
  \item{x}{numeric vector}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\value{
a length-one numeric vector
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.quantile}}
}

\examples{
data(Movies)
weighted.mad(Movies$Critics, weights = rep(c(.8,1.2), 500))
}

\keyword{ multivariate }
\keyword{ misc }
