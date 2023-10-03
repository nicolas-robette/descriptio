\name{assoc.catcont}
\alias{assoc.catcont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between a categorical variable and a continuous variable}
\description{
Measures the association between a categorical variable and a continuous variable
}
\usage{
assoc.catcont(x, y, weights = NULL,
              na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
              nperm = NULL, distrib = "asympt", digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the categorical variable (must be a factor)}
  \item{y}{the continuous variable (must be a numeric vector)}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variable (i.e. x) should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variable (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NA". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variable (i.e. y) should be silently removed before the computation proceeds. Default is FALSE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}.}
  \item{digits}{integer. The number of digits (default is 3).}
}

\value{
A list with the following elements :
\item{summary}{summary statistics (mean, median, etc.) of the continuous variable for each level of the categorical variable}
\item{eta.squared}{eta-squared between the two variables}
\item{permutation.pvalue}{p-value from a permutation (i.e. non-parametric) test of independence}
\item{cor}{point biserial correlation between the two variables, for each level of the categorical variable}
\item{cor.perm.pval}{permutation p-value of the correlation between the two variables, for each level of the categorical variable}
\item{test.values}{test-values as proposed by Lebart et al (1984)}
\item{test.values.pval}{p-values corresponding to the test-values}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]

Lebart L., Morineau A. and Warwick K., 1984, *Multivariate Descriptive Statistical Analysis*, John Wiley and sons, New-York.
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}}, \code{\link{assoc.twocont}}, \code{\link{assoc.yx}}, \code{\link{condesc}},
\code{\link{catdesc}}, \code{\link{darma}} %, \code{\link{ggassoc_boxplot}}
}

\examples{
data(Movies)
with(Movies, assoc.catcont(Country, Budget, nperm = 10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
