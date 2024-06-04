\name{weighted.table}
\alias{weighted.table}

\title{Computes a (possibly weighted) contingency table}
\description{Computes a contingency table from one or two vectors, with the possibility of specifying weights.}

\usage{weighted.table(x, y = NULL, weights = NULL, stat = "freq",
              mar = FALSE, na.rm = FALSE, na.value = "NAs", digits = 1)}
\arguments{
  \item{x}{an object which can be interpreted as factor}
  \item{y}{an optional object which can be interpreted as factor}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{stat}{character. Whether to compute a contingency table ("freq", default), percentages ("prop"), row percentages ("rprop") or column percentages ("cprop").}
  \item{mar}{logical, indicating whether to compute margins. Default is FALSE.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
  \item{digits}{integer indicating the number of decimal places (default is 1)}
}


\value{Returns a contingency table.}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{assoc.twocat}}}

\examples{
data(Movies)
weighted.table(Movies$Country, Movies$ArtHouse)
}

\keyword{ multivariate }
\keyword{ misc }
