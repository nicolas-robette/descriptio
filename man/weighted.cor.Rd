\name{weighted.cor}
\alias{weighted.cor}

\title{Weighted correlation}

\description{
Computes the weighted correlation between two distributions. This can be Pearson, Spearman or Kendall correlation.
}
\usage{
weighted.cor(x, y, weights = NULL, method = "pearson", na.rm = FALSE)
}
\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{method}{a character string indicating which correlation coefficient is to be computed. One of "pearson" (default), "kendall", or "spearman".}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\value{
a length-one numeric vector
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.sd}}, \code{\link{weighted.cor2}}
}

\examples{
data(Movies)
weighted.cor(Movies$Critics, Movies$BoxOffice, weights = rep(c(.8,1.2), 500))
weighted.cor(Movies$Critics, Movies$BoxOffice, weights = rep(c(.8,1.2), 500), method = "spearman")
}

\keyword{ multivariate }
\keyword{ misc }
