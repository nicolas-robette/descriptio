\name{crosstab}
\alias{crosstab}

\title{
Pretty 2, 3 or 4-way cross-tabulations
}

\description{
Displays pretty 2, 3 or 4-way cross-tabulations, from possibly weighted data, and with the opportunity to color the cells of the table according to a local measure of association (phi coefficients, standardized residuals or PEM). 
}

\usage{
crosstab(x, 
         y,
         xstrata = NULL,
         ystrata = NULL,
         weights = NULL,
         stat = "rprop",
         show.n = FALSE,
         show.cramer = TRUE,
         na.rm = FALSE,
         na.value = "NAs",
         digits = 1,
         sort = "none",
         color.cells = FALSE,
         measure = "phi",
         limits = c(-1, 1),
         min.asso = 0.1, 
         palette = "PRGn",
         reverse = FALSE)
}

\arguments{
  \item{x}{the row categorical variable}
  \item{y}{the column categorical variable}
  \item{xstrata}{optional categorical variable to stratify the table by rows. Default is NULL, which means no row strata.}
  \item{ystrata}{optional categorical variable to stratify the table by columns. Default is NULL, which means no column strata.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{stat}{character. Whether to compute a contingency table ("freq", default), percentages ("prop"), row percentages ("rprop") or column percentages ("cprop").}
  \item{show.n}{logical. Whether to display frequencies (between brackets) in addition to the percentages. Ignored if stat = "freq". Default is FALSE.}
  \item{show.cramer}{logical. If TRUE (default), Cramer's V measure of association is displayed beside the table.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see \code{na.value} argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
  \item{digits}{integer. The number of digits (default is 1). If NULL, the results are not rounded.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
  \item{color.cells}{logical, indicating whether the cells of the table should be colored according to local measures of association. Default is FALSE.}
  \item{measure}{character. The measure of association used to color the cells. Can be "phi" for phi coefficient (default), "std.residuals" for standardized residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence. Only used if color.cells = TRUE.}
  \item{limits}{a numeric vector of length 2 providing limits of the scale. Default is c(-1,1). Only used if color.cells = TRUE.}
  \item{min.asso}{numerical value. The cells with a local association below min.asso (in absolute value) are kept blank. Only used if color.cells = TRUE.}
  \item{palette}{The colours or colour function that values will be mapped to (see details).}
  \item{reverse}{Whether the colors (or color function) in palette should be used in reverse order. For example, if the default order of a palette goes from blue to green, then reverse = TRUE will result in the colors going from green to blue. Default is FALSE. Only used if color.cells = TRUE.}
}

\details{
The function uses \code{gtsummary} package to build the cross-tabulation, and then \code{gt} package to finalize the layout and color the cells. Weights are handled silently with \code{survey} package.

Besides, the function is compatible with the attribute labels assigned with \code{labelled} package : these labels are displayed automatically.

The \code{palette} argument can be any of the following :
1. A character vector of RGB or named colours. Examples: palette(), c("#000000", "#0000FF", "#FFFFFF"), topo.colors(10)
2. The name of an \code{RColorBrewer} palette, e.g. "BuPu" or "Greens".
3. The full name of a \code{viridis} palette: "viridis", "magma", "inferno", or "plasma".
4. A function that receives a single value between 0 and 1 and returns a colour. Examples: colorRamp(c("#000000", "#FFFFFF"), interpolate="spline").

}

\value{
An object of class \code{gt_tbl}.
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{weighted.table}}, \code{\link{phi.table}}
}

\examples{
data(Movies)
crosstab(Movies$Genre, Movies$Country)
with(Movies, crosstab(Genre, Country, ystrata = ArtHouse, show.n = TRUE, color.cells = TRUE))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
\keyword{ misc }
