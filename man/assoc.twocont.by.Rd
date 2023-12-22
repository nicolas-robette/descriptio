\name{assoc.twocont.by}
\alias{assoc.twocont.by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the groupwise association between two continuous variables}
\description{
Measures the association between two continuous variables with Pearson, Spearman and Kendall correlations, for each category of a group variable.
}
\usage{
assoc.twocont.by(x, y, by, weights = NULL, na.rm = FALSE,
                 nperm = NULL, distrib = "asympt")
}

\arguments{
  \item{x}{numeric vector : a continuous variable}
  \item{y}{numeric vector : a continuous variable}
  \item{by}{factor : the group variable}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}.}
}

\value{
A list of items, one for each category of the groupe variable. Each item is a data frame with Pearson, Spearman and Kendall correlations. The correlation value is in the first row and a p-value from a permutation (so non parametric) test of independence is in the second row.
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocont}}, \code{\link{assoc.twocat}}, \code{\link{assoc.catcont}}, \code{\link{assoc.yx}}, \code{\link{condesc}},
\code{\link{catdesc}}, \code{\link{darma}} %, \code{\link{ggassoc_scatter}}
}
\examples{
## Hollander & Wolfe (1973), p. 187f.
## Assessment of tuna quality.  We compare the Hunter L measure of
##  lightness to the averages of consumer panel scores (recoded as
##  integer values from 1 to 6 and averaged over 80 such values) in
##  9 lots of canned tuna.
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
group <- factor(c("A","B","C","C","B","A","A","C","B"))
assoc.twocont.by(x,y,group,nperm=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
