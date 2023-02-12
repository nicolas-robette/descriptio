\name{ggassoc_phiplot}

\alias{ggassoc_phiplot}

\title{Bar plot of measures of local association of a crosstabulation}

\description{
For a cross-tabulation, plots the measures of local association with bars of varying height, using ggplot2.
}

\usage{
ggassoc_phiplot(data, mapping, measure = "phi", limit = NULL,
sort="none")
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetics being used. x and y are required, weight can also be specified.}
  \item{measure}{character. The measure of association used for filling the rectangles. Can be "phi" for phi coefficient (default), "or" for odds ratios, "std.residuals" for standardized residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{limit}{numeric value, specifying the upper limit of the scale for the height of the bars, i.e. for the measures of association (the lower limit is set to 0-limit). It corresponds to the maximum absolute value of association one wants to represent in the plot. If NULL (default), the limit is automatically adjusted to the data.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
}

\details{
The measure of association measures how much each combination of categories of x and y is over/under-represented.
The bars vary in width according to the number of observations in the categories of the column variable. They vary in height according to the measure of association. Bars are black if the association is positive and white if it is negative.

The genuine version of this plot (see Cibois, 2004) uses the measure of association called "pem", i.e. the local percentages of maximum deviation from independence.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\references{
Cibois Philippe, 2004, \emph{Les écarts à l'indépendance. Techniques simples pour analyser des données d'enquêtes}, Collection "Méthodes quantitatives pour les sciences sociales"
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_crosstab}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Movies)
ggassoc_phiplot(data=Movies, mapping=ggplot2::aes(Country, Genre))
}

\keyword{ multivariate }
\keyword{ aplot }
