\name{stdres.table}
\alias{stdres.table}

\title{
Standardized residuals of a contingency table
}

\description{
Computes standardized or adjusted residuals of a (possibly) weighted contingency table
}

\usage{
stdres.table(x, y, weights = NULL, na.rm = FALSE,
  na.value = "NAs", digits = 3, residuals = "std")
}

\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
  \item{digits}{integer. The number of digits (default is 3). If NULL, the results are not rounded.}
  \item{residuals}{If "std" (default), standardized (i.e. Pearson) residuals are computed. If "adj", adjusted standardized residuals are computed.}
}

\value{
A table with the residuals
}

\note{
The adjusted standardized residuals are strictly equivalent to test-values for nominal variables as proposed by Lebart et al (1984).
}

\references{
Agresti, A. (2007). \emph{An Introduction to Categorical Data Analysis}, 2nd ed. New York: John Wiley & Sons.

Rakotomalala R., \emph{Comprendre la taille d'effet (effect size)}, http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf

Lebart L., Morineau A. and Warwick K., 1984, *Multivariate Descriptive Statistical Analysis*, John Wiley and sons, New-York.
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{phi.table}}, \code{\link{or.table}}, \code{\link{pem.table}}
}

\examples{
data(Movies)
stdres.table(Movies$Country, Movies$ArtHouse)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
