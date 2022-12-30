\name{ggassoc_marimekko}

\alias{ggassoc_marimekko}

\title{Marimekko plot}

\description{
For a cross-tabulation, plots a marimekko chart (also called mosaic plot), using ggplot2.
}

\usage{
ggassoc_marimekko(data, mapping, type = "classic", 
measure = "phi", limit = NULL, 
palette = NULL, direction = 1, 
linecolor = "gray60", linewidth = 0.1, 
sort = "none", legend = "right")
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{type}{character. If "classic" (default), a simple marimekko chart is plotted, with no use of local associations. If type is "shades", tiles are shaded according to the local associations between categories. If type is "patterns", tiles are filled with patterns, and the density of patterns is proportional to the absolute level of local association between categories.}
  \item{measure}{character. The measure of association used for filling (if type is "shades) or patterning (if type is "patterns") the tiles. Can be "phi" for phi coefficient, "or" for odds ratios, "std.residuals" (default) for standardized (i.e. Pearson) residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{limit}{numeric value, ony used if type is "shades". It specifies the upper limit of the scale for the color gradient filling the bars, i.e. for the measures of association (the lower limit is set to 0-limit). It corresponds to the maximum absolute value of association one wants to represent in the plot. If NULL (default), the limit is automatically adjusted to the data.}
  \item{palette}{character. If type is "shades", name of a ColorBrewer diverging palette (default is PRGn). If type is "classic" or "patterns", palette name in the form packagename::palettename (cf \code{paletteer} package), and default is "khroma::bright", a colorblind friendly palette.}
  \item{direction}{Sets the order of colours in the scale. If 1, the default, colours are as output by RColorBrewer::brewer.pal(). If -1, the order of colours is reversed.}
  \item{linecolor}{character. Color of the contour lines of the tiles. Default is gray60.}
  \item{linewidth}{numeric. Width of the contour lines of the tiles. Default is 0.1.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
  \item{legend}{the position of legend ("none", "left", "right", "bottom", "top"). If "none", no legend is displayed.}
}

\details{
The measure of local association measures how much each combination of categories of x and y is over/under-represented.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 

Note : It does not allow faceting.
}

\value{
a ggplot object
}

\references{
Hartigan, J.A., and Kleiner, B. (1984), "A mosaic of television ratings". \emph{The American Statistician}, 38, 32–35. 

Friendly, M. (1994), "Mosaic displays for multi-way contingency tables". \emph{Journal of the American Statistical Association}, 89, 190–200.
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_crosstab}}, \code{\link[GGally]{ggpairs}}
}

\examples{
\dontrun{
data(Taste)
ggassoc_marimekko(data=Movies, mapping=ggplot2::aes(Genre, Country))
ggassoc_marimekko(data=Movies, mapping=ggplot2::aes(Genre, Country), type = "patterns")
ggassoc_marimekko(data=Movies, mapping=ggplot2::aes(Genre, Country), type = "shades")
}
}

\keyword{ multivariate }
\keyword{ aplot }
