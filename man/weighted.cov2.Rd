\name{weighted.cov2}
\alias{weighted.cov2}

\title{Weighted covariances}

\description{
Computes a matrix of weighted covariances between the columns of \code{x} and the columns of \code{y}.
}

\usage{
weighted.cov2(x, y = NULL, weights = NULL, na.rm = FALSE)
}

\arguments{
  \item{x}{a data frame of numeric vectors}
  \item{y}{an optional data frame of numeric vectors. Default is NULL, which means that covariances between the columns of \code{x} are computed.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\value{
a matrix of covariances
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.cov}}
}

\examples{
data(Movies)
weighted.cov2(Movies[,c("Budget", "Critics", "BoxOffice")], weights = rep(c(.8,1.2), 500))
}

\keyword{ multivariate }
\keyword{ misc }
