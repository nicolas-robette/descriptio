\name{weighted.cor}
\alias{weighted.cor}

\title{Weighted correlation}

\description{
Computes the weighted correlation of a distribution. This can be Pearson, Spearman or Kendall correlation.
}
\usage{
weighted.cor(x, y, weights, method = "pearson", na.rm = TRUE)
}
\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{weights}{numeric vector of weights}
  \item{method}{a character string indicating which correlation coefficient is to be computed. One of "pearson" (default), "kendall", or "spearman".}
  \item{na.rm}{logical. If true, any NA are removed from x before the quantiles are computed.}
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.sd}}
}

\examples{
data(Movies)
weighted.cor(Movies$Critics, Movies$BoxOffice, weights = rep(c(.8,1.2), 500))
weighted.cor(Movies$Critics, Movies$BoxOffice, weights = rep(c(.8,1.2), 500), method = "spearman")
}

\keyword{ multivariate }
\keyword{ misc }
