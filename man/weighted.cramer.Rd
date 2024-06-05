\name{weighted.cramer}
\alias{weighted.cramer}

\title{
Cramer's V
}

\description{
Computes Cramer's V measure of association between two (possibly weighted) categorical variables}

\usage{
weighted.cramer(x, y, weights = NULL, na.rm = FALSE)
}

\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds.}
}

\value{
Numerical value with Cramer's V.
}

\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}}
}

\examples{
data(Movies)
weighted.cramer(Movies$Country, Movies$ArtHouse)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
