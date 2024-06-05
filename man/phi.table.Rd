\name{phi.table}
\alias{phi.table}

\title{Computes the phi coefficient for every cells of a contingency table}

\description{
Computes the phi coefficient for every cells of the cross-tabulation between two categorical variables
}

\usage{
phi.table(x, y, weights = NULL, na.rm = FALSE, na.value = "NAs", digits = 3)
}

\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
  \item{digits}{integer. The number of digits (default is 3). If NULL, the results are not rounded.}
}

\value{
A table with the phi coefficients
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
}

\examples{
data(Movies)
phi.table(Movies$Country, Movies$ArtHouse)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
