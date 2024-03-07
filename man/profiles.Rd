\name{profiles}
\alias{profiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profiles by level of a categorical variable
}

\description{
Computes profiles (frequencies or percentages) for subgroups of observations defined by the levels of a categorical variable.
}

\usage{
profiles(X, y, weights = NULL, stat = "cprop",
 mar = TRUE, digits = 1)
}

\arguments{
  \item{X}{data frame. The variables which are described in the profiles. There should be only factors.}
  \item{y}{factor. The categorical variable which defines subgroups of observations whose profiles will be computed.}
  \item{weights}{numeric vector of weights. If NULL (default), uniform weights (i.e. all equal to 1) are used.}
  \item{stat}{character. Whether to compute frequencies ("freq"), percentages ("prop"), row percentages ("rprop") or column percentages ("cprop", default).}
  \item{mar}{logical, indicating whether to compute margins. Default is TRUE.}
  \item{digits}{numeric. Number of digits. Default is 1.}
}

\value{
A data frame with profiles in columns
}

\author{Nicolas Robette}

\seealso{
\code{\link{catdesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{assoc.twocat}}, \code{\link{assoc.twocat.by}}
}

\examples{
data(Movies)
profiles(Movies[,c(2,4,5)], Movies$Country)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
\keyword{ misc }
