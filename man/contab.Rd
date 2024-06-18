\name{contab}
\alias{contab}

\title{
Bivariate statistics between a continuous variable and a set of variables
}

\description{
Computes bivariate statistics between a continuous variable and a set of variables, possibly according to a strata variable.
}

\usage{
contab(x, y, strata = NULL, weights = NULL, robust = TRUE,
       digits = c(1,3), na.rm = TRUE, na.value = "NAs")
}

\arguments{
  \item{x}{data frame. The variables which are described in rows. They can be numerical or factors.}
  \item{y}{factor. The categorical variable which defines subgroups of observations described in columns.}
  \item{strata}{optional categorical variable to stratify the table by column. Default is NULL, which means no strata.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{robust}{logical. Whether to use medians (and mads) instead of means (and standard deviations). Default is TRUE.}
  \item{digits}{vector of 2 integers. The first value sets the number of digits for medians, mads, means and standard deviations (categorical variables). The second one sets the number of digits for slopes (continuous variables). Default is c(1,3). If NULL, the results are not rounded.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables with NA values (see \code{na.value} argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
}

\details{
For categorical variables in \code{x}, the function computes :

- column 1 : the median and the mad of \code{y} for each level of the variable

- column 2 : the global association between the variable and \code{y}, measured by the eta-squared

For continous variables in \code{x}, it computes :

- column 1 : the slope of the linear regression of \code{y} according to the variable

- column 2 : the global association between the variable and \code{y}, measured by Pearson and Spearman correlations
}

\value{
An object of class \code{gt_tbl}.
}

\author{Nicolas Robette}

\seealso{
\code{\link{regtab}}, \code{\link{condesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{assoc.twocont}},
\code{\link{assoc.twocont.by}}
}

\examples{
data(Movies)
contab(x = Movies[, c("Genre", "ArtHouse", "Budget")],
       y = Movies$BoxOffice)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
\keyword{ misc }
