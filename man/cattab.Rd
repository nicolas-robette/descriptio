\name{cattab}
\alias{cattab}

\title{
Bivariate statistics between a categorical variable and a set of variables
}

\description{
Computes bivariate statistics for a set of variables according to the subgroups of observations defined by a categorical variable.
}

\usage{
cattab(x, y, weights = NULL, percent = "column",
       robust = TRUE, show.n = TRUE, show.asso = TRUE,
       digits = c(1,1), na.rm = TRUE, na.value = "NAs")
}

\arguments{
  \item{x}{data frame. The variables which are described in rows. They can be numerical or factors.}
  \item{y}{factor. The categorical variable which defines subgroups of observations described in columns.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{percent}{character. Whether to compute row percentages ("row") or column percentages ("column", default).}
  \item{robust}{logical. Whether to use medians instead of means. Default is TRUE.}
  \item{show.n}{logical. Whether to display frequencies (between brackets) in addition to the percentages. Default is TRUE.}
  \item{show.asso}{logical. Whether to add a column with measures of global association (Cramer's V and eta-squared). Default is TRUE.}
  \item{digits}{vector of 2 integers. The first value sets the number of digits for percentages, the second one sets the number of digits for medians and means. Default is c(1,1). If NULL, the results are not rounded.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see \code{na.value} argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
}

\details{
The function uses \code{gtsummary} package to build the table of statistics, and then \code{gt} package to finalize the layout. Weights are handled silently with \code{survey} package.

Besides, the function is compatible with the attribute labels assigned with \code{labelled} package : these labels are displayed automatically.
}

\note{
This function is quite similar to \code{\link{profiles}}, but displays the results in a fancier way.
}

\value{
An object of class \code{gt_tbl}.
}

\author{Nicolas Robette}

\seealso{
\code{\link{catdesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{assoc.twocat}},
\code{\link{assoc.twocat.by}}, \code{\link{profiles}}
}

\examples{
data(Movies)
cattab(x = Movies[, c("Genre", "ArtHouse", "Critics", "BoxOffice")],
       y = Movies$Country)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
\keyword{ misc }
