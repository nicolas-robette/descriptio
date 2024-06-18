\name{regtab}
\alias{regtab}

\title{
Univariate and Multivariate Regressions and Their Average Marginal Effects
}

\description{
Computes linear or binomial regressions in two steps : univariate regressions and a multivariate regressions. All the results are nicely displayed side by side with average marginal effects.
}

\usage{
regtab(x, y, weights = NULL, continuous = "slopes", 
 show.ci = TRUE, conf.level = 0.95)
}

\arguments{
  \item{x}{data frame. The explanatory (i.e. independent) variables used in regressions. They can be numerical or factors.}
  \item{y}{vector. The outcome (i.e. dependent) variable. It can be numerical (linear regression) or a factor with 2 levels (binomial regression).}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{continuous}{character. The kind of average marginal effects computed for continuous explanatory variables. If "slopes" (defaults), these are average marginal slopes. If "predictions", these are average marginal predictions for a set of values.}
  \item{show.ci}{logical. Whether to display the confidence intervals}
  \item{conf.level}{numerical value. Defaults to 0.95, which corresponds to a 95 percent confidence interval. Must be strictly greater than 0 and less than 1.}
}

\details{
This function is basically a wrapper for regression functions in the \code{gtsummary} function. It computes a series of univariate regressions (one for each explanatory variable), then a multivariate regression (with all explanatory variables) and displays the results side by side. These results are presented in the form of average marginal effects : average marginal predictions for categorical variables and average marginal slopes (or predictions) for continuous variables.

Besides, the function is compatible with the attribute labels assigned with \code{labelled} package : these labels are displayed automatically.
}

\value{
an object of class \code{tbl_merge} from \code{gtsummary} package
}

\author{Nicolas Robette}

\references{
Arel-Bundock V, Greifer N, Heiss A (Forthcoming). “How to Interpret Statistical Models Using marginaleffects in R and Python.” \emph{Journal of Statistical Software}.

Larmarange J., 2024, “Prédictions marginales, contrastes marginaux & effets marginaux”, in \emph{Guide-R, Guide pour l’analyse de données d’enquêtes avec R}, https://larmarange.github.io/guide-R/analyses/estimations-marginales.html
}

\seealso{
\code{\link{cattab}}, \code{\link{catdesc}}, \code{\link{condesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{assoc.twocat}}, \code{\link{assoc.twocat.by}}
}

\examples{
data(Movies)
regtab(x = Movies[, c("Genre", "Budget", "Festival", "Critics")],
       y = Movies$BoxOffice)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
\keyword{ misc }
