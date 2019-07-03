\name{ihm-package}
\alias{ihm-package}
\docType{package}
\title{
A package to draw a heatmap with a given data (statistic) matrix and an optional p-value matrix
}
\description{
This is an R package for drawing heatmaps. Two important features motivated the development of this package. (1) Given a data set (typically each cell representing the value of a statistic) and an associated matrix of p-values, display the dataset in heatmap and annotate the cells to show the statistical significance of each cell (p-values are optional though). (2) Automatically compute the dimensions of the figure area to make publication quality figures without manual setting of parameters. This package is based on heatmap.2 function of the R gplots package.
}
\details{
\tabular{ll}{
Package: \tab ihm\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2012-11-22\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
The most important function of this package is ihm. All other functions are used by it in turn.
}
\author{
Gopal Peddinti
<cugopal@gmail.com>
}
\references{
This package depends on gplots package. Also, the function heat.2 is almost entirely a copy of heamap.2 function from gplots (except for a very minor change as of now)
}
\keyword{package}
\examples{
\dontrun{data(dataset)}
\dontrun{data(pvals)}
\dontrun{ihm(dataset, pvals)}
}
