\name{assoc.yx}
\alias{assoc.yx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Bivariate association measures between a response and predictor variables.
}
\description{
  Computes bivariate association measures between a response and predictor variables (and, optionnaly, between every pairs of predictor variables.)
}
\usage{
  assoc.yx(y, x, weights = NULL, xx = TRUE, correlation = "kendall",
  na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
  nperm = NULL, distrib = "asympt", dec = c(3,3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the response variable}
  \item{x}{the predictor variables}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{xx}{whether the association measures should be computed for couples of predictor variables (default) or not. With a lot of predictors, consider setting xx to FALSE (for reasons of computation time).}
  \item{correlation}{character. The type of measure of correlation measure to use between two continuous variables : "pearson", "spearman" or "kendall" (default).}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variables should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variables should be silently removed before the computation proceeds. Default is FALSE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{dec}{vector of 2 integers for number of decimals. The first value if for association measures, the second for permutation p-values. Default is c(3,3).}
}

\details{
The function computes an association measure : Pearson's, Spearman's or Kendall's correlation for pairs of numeric variables, Cramer's V for pairs of factors and eta-squared for pairs numeric-factor. It can also compute the p-value of a permutation test of association for each pair of variables.}

\value{
  A list of the following items :
  \item{YX}{: a table with the association measures between the response and predictor variables}
  \item{XX}{: a table with the association measures between every pairs of predictor variables}
  In each table :
  \item{measure}{: name of the association measure}
  \item{association}{: value of the association measure}
  \item{permutation.pvalue}{: p-value from the permutation test}
}

\author{
  Nicolas Robette
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    \code{\link{darma}}, \code{\link{assoc.twocat}}, \code{\link{assoc.twocont}}, \code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
  }

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  assoc.yx(iris2$Species,iris2[,1:4],nperm=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
