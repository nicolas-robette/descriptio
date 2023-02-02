\name{weighted.mad}
\alias{weighted.mad}

\title{Weighted median absolute deviation to median}

\description{
Computes the weighted median absolute deviation to median (aka MAD) of a distribution.
}
\usage{
weighted.mad(x, weights)
}
\arguments{
  \item{x}{numeric vector}
  \item{weights}{numeric vector of weights}
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
