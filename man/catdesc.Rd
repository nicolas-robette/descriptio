\name{catdesc}
\alias{catdesc}

\title{Measures the association between a categorical variable and some continuous and/or categorical variables}
\description{
Measures the association between a categorical variable and some continuous and/or categorical variables}
\usage{
catdesc(y, x, weights = NULL, 
na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
measure = "phi", limit = NULL, correlation = "kendall", robust = TRUE, 
nperm = NULL, distrib = "asympt", digits = 2)
}

\arguments{
  \item{y}{the categorical variable to describe (must be a factor)}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variables should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variables should be silently removed before the computation proceeds. Default is FALSE.}
  \item{measure}{character. The measure of local association between categories of categorical variables. Can be "phi" for phi coefficient (default), "or" for odds ratios, "std.residuals" for standardized (i.e. Pearson) residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{limit}{for the relationship between y and a categorical variable, only associations higher or equal to \code{limit} will be displayed. If NULL (default), they are all displayed.}
  \item{correlation}{character. The type of measure of correlation measure to use between two continuous variables : "pearson", "spearman" or "kendall" (default).}
  \item{robust}{logical. If TRUE (default), median and mad are used instead of mean and standard deviation.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{digits}{numeric. Number of digits for mean, median, standard deviation and mad. Default is 2.}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x}
\item{bylevel}{a list with one element for each level of y}
Each element in bylevel has the following items :
\item{categories}{a data frame with categorical variables from x and local associations}
\item{continuous.var}{a data frame with continuous variables from x and associations measured by correlation coefficients}
}

\note{
If nperm is not NULL, permutation tests of independence are computed and the p-values from these tests are provided.
}

\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}

\author{Nicolas Robette}

\seealso{
\code{\link[FactoMineR]{catdes}}, \code{\link{condesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}
}
\examples{
data(Movies)
catdesc(Movies$ArtHouse, Movies[,c("Budget","Genre","Country")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
