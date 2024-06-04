\name{condesc}
\alias{condesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between a continuous variable and some continuous and/or categorical variables}
\description{
Measures the association between a continuous variable and some continuous and/or categorical variables}
\usage{
condesc(y, x, weights = NULL, 
na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
limit = NULL, correlation = "kendall", robust = TRUE, 
nperm = NULL, distrib = "asympt", digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the continuous variable to describe}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variables should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variables should be silently removed before the computation proceeds. Default is FALSE.}
  \item{limit}{for the relationship between y and a category of a categorical variable, only associations (point-biserial correlations) higher or equal to \code{limit} will be displayed. If NULL (default), they are all displayed.}
  \item{correlation}{character. The type of correlation measure to use between two continuous variables : "pearson", "spearman" or "kendall" (default).}
  \item{robust}{logical. If TRUE (default), meadian and mad are used instead of mean and standard deviation.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{digits}{numeric. Number of digits for mean, median, standard deviation and mad. Default is 2.}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x}
\item{categories}{a data frame with categorical variables from x and associations measured by point biserial correlation.}
}

\note{
If nperm is not NULL, permutation tests of independence are computed and the p-values from these tests are provided.
}

\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}

\author{Nicolas Robette}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[FactoMineR]{condes}}, \code{\link{catdesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}

}
\examples{
data(Movies)
condesc(Movies$BoxOffice, Movies[,c("Budget","Genre","Country")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
