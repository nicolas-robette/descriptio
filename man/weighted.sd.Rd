\name{weighted.sd}
\alias{weighted.sd}

\title{Weighted standard deviation}

\description{
Computes the weighted standard deviation of a distribution.
}
\usage{
weighted.sd(x, weights)
}
\arguments{
  \item{x}{numeric vector}
  \item{weights}{numeric vector of weights}
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
