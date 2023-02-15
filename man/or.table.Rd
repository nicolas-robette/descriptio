\name{or.table}
\alias{or.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the odds ratios for every cells of a contingency table}
\description{
Computes the odds ratio for every cells of the cross-tabulation between two categorical variables
}
\usage{
or.table(x, y, weights = NULL, na.rm = FALSE, na.value = "NA", digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NA". Only used if na.rm = FALSE.}
  \item{digits}{integer. The number of digits (default is 3). If NULL, the results are not rounded.}
}

\value{
A table with the odds ratios
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Movies)
or.table(Movies$Country, Movies$ArtHouse)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
