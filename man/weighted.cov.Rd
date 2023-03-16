\name{weighted.cov}
\alias{weighted.cov}

\title{Weighted covariance}

\description{
Computes the weighted covariance between two distributions.
}

\usage{
weighted.cov(x, y, weights = NULL, na.rm = FALSE)
}

\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\value{
a length-one numeric vector
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.sd}}, \code{\link{weighted.cor}}, \code{\link{weighted.cov2}}
}

\examples{
data(Movies)
weighted.cov(Movies$Critics, Movies$BoxOffice, weights = rep(c(.8,1.2), 500))
}

\keyword{ multivariate }
\keyword{ misc }
