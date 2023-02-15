\name{assoc.twocat}
\alias{assoc.twocat}

\title{Cross-tabulation and measures of association between two categorical variables}
\description{
Cross-tabulation and measures of association between two categorical variables
}
\usage{
assoc.twocat(x, y, weights = NULL, na.rm = FALSE, na.value = "NA",
             nperm = NULL, distrib = "asympt")
}

\arguments{
  \item{x}{the first categorical variable (must be a factor)}
  \item{y}{the second categorical variable (must be a factor)}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NA". Only used if na.rm = FALSE.}  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{asympt}, default) or via Monte Carlo resampling (\code{approx}).}
 
}

\value{
A list of lists with the following elements :    

\code{tables} list :
\item{freq}{cross-tabulation frequencies}
\item{prop}{percentages}
\item{rprop}{row percentages}
\item{cprop}{column percentages}
\item{expected}{expected values}
\code{global} list :
\item{chi.squared}{chi-squared value}
\item{cramer.v}{Cramer's V between the two variables}
\item{permutation.pvalue}{p-value from a permutation (so non-parametric) test of independence}
\item{global.pem}{global PEM}
\item{GK.tau.xy}{Goodman and Kruskal tau (forward association)}
\item{GK.tau.yx}{Goodman and Kruskal tau (backward association)}
\code{local} list :
\item{std.residuals}{the table of standardized (i.e.Pearson) residuals.}
\item{adj.residuals}{the table of adjusted standardized residuals.}
\item{odds.ratios}{the table of odds ratios.}
\item{local.pem}{the table of local PEM}
\item{phi}{the table of the phi coefficients for each pair of levels}
\item{phi.perm.pval}{the table of permutation p-values for each pair of levels}
\code{gather} : a data frame gathering informations, with one row per cell of the cross-tabulation.
}
\references{
Agresti, A. (2007). \emph{An Introduction to Categorical Data Analysis}, 2nd ed. New York: John Wiley & Sons.

Rakotomalala R., \emph{Comprendre la taille d'effet (effect size)}, http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.catcont}}, \code{\link{assoc.twocont}}, \code{\link{assoc.yx}}, \code{\link{condesc}},
\code{\link{catdesc}}, \code{\link{darma}}
}
\examples{
data(Movies)
assoc.twocat(Movies$Country, Movies$ArtHouse, nperm=100)
}
\keyword{ multivariate }
\keyword{ misc }
