\name{weighted.cor2}
\alias{weighted.cor2}

\title{Weighted correlations}

\description{
Computes a matrix of weighted correlations between the columns of \code{x} and the columns of \code{y}. This can be Pearson, Spearman or Kendall correlation.
}

\usage{
weighted.cor2(x, y = NULL, weights = NULL, method = "pearson", na.rm = FALSE)
}

\arguments{
  \item{x}{a data frame of numeric vectors}
  \item{y}{an optional data frame of numeric vectors. Default is NULL, which means that correlations between the columns of \code{x} are computed.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{method}{a character string indicating which correlation coefficient is to be computed. One of "pearson" (default), "kendall", or "spearman".}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
}

\value{
a matrix of correlations
}

\author{Nicolas Robette}

\seealso{
\code{\link{weighted.cor}}
}

\examples{
data(Movies)
weighted.cor2(Movies[,c("Budget", "Critics", "BoxOffice")], weights = rep(c(.8,1.2), 500))
}

\keyword{ multivariate }
\keyword{ misc }
