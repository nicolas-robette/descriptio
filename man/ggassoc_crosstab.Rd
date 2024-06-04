\name{ggassoc_crosstab}

\alias{ggassoc_crosstab}

\title{Proportional area plot}

\description{
For a cross-tabulation, plots the observed (or expected) frequencies by using rectangles with proportional areas, and the measures of local association between the categories with a diverging gradient of colour, using ggplot2.
}

\usage{
ggassoc_crosstab(data, mapping, size = "freq", max.size =  20,
                 measure = "phi", limits = NULL, sort = "none", 
                 na.rm = FALSE, na.value = "NAs",
                 colors = NULL, direction = 1, legend = "right")
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetics being used. x and y are required, weight can also be specified.}
  \item{size}{character. If "freq" (default), areas are proportional to observed frequencies. If "expected", they are proportional to expected frequencies.}
  \item{max.size}{numeric value, specifying the maximum size of the squares. Default is 20.}
  \item{measure}{character. The measure of association used for filling the rectangles. Can be "phi" for phi coefficient (default), "or" for odds ratios, "std.residuals" for standardized residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{limits}{a numeric vector of length two providing limits of the scale. If NULL (default), the limits are automatically adjusted to the data.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.} 
  \item{colors}{vector of colors that will be interpolated to produce a color gradient. If NULL (default), the "Temps" palette from \code{rcartocolors} package is used.}
  \item{direction}{Sets the order of colours in the scale. If 1, the default, colours are as output by RColorBrewer::brewer.pal(). If -1, the order of colours is reversed.}
  \item{legend}{the position of legend ("none", "left", "right", "bottom", "top"). If "none", no legend is displayed.}
}

\details{
The measure of local association measures how much each combination of categories of x and y is over/under-represented.

The areas of the rectangles are proportional to observed or expected frequencies. Their color shading varies according to the measure of association.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_phiplot}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Movies)
ggassoc_crosstab(data=Movies, mapping=ggplot2::aes(Genre, Country))
}

\keyword{ multivariate }
\keyword{ aplot }
