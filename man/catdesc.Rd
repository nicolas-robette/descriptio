\name{catdesc}
\alias{catdesc}

\title{Measures the association between a categorical variable and some continuous and/or categorical variables}
\description{
Measures the association between a categorical variable and some continuous and/or categorical variables}
\usage{
catdesc(y, x, weights = NULL, 
na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
measure = "phi", limit = NULL, correlation = "kendall", robust = TRUE, 
nperm = NULL, distrib = "asympt", dec = c(3,3,3,3,1,3))
}

\arguments{
  \item{y}{the categorical variable to describe (must be a factor)}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variables should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NA". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variables should be silently removed before the computation proceeds. Default is FALSE.}
  \item{measure}{character. The measure of local association between categories of categorical variables. Can be "phi" for phi coefficient, "or" for odds ratios, "std.residuals" (default) for standardized (i.e. Pearson) residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{limit}{for the relationship between y and a categorical variable, only associations higher or equal to \code{limit} will be displayed. If NULL (default), they are all displayed.}
  \item{correlation}{character. The type of measure of correlation measure to use between two continuous variables : "pearson", "spearman" or "kendall" (default).}
  \item{robust}{logical. If FALSE, mean and standard deviation are used instead of median and mad. Default is TRUE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{dec}{vector of 6 integers for number of decimals. The first value if for association measures, the second for permutation p-values, the third for percents, the fourth for phi coefficients, the fifth for medians and mads, the sixth for point biserial correlations. Default is c(3,3,3,3,1,3).}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x}
\item{bylevel}{a list with one element for each level of y}
Each element in bylevel has the following items :
\item{categories}{a data frame with categorical variables from x and associations measured by phi}
\item{continuous.var}{a data frame with continuous variables from x and associations measured by correlation coefficients}
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
