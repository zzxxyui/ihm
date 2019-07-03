\name{draw.heat.2}
\alias{draw.heat.2}
\title{
An internal function to draw heatmap and corresponding p-values
}
\description{
This function by itself can be used to draw the heatmap with p-values. BUT, it is intended as an internal function called by ihm(). The user only needs the function ihm.  
}
\usage{
draw.heat.2(dataset, pvals, ...)
}
\arguments{
  \item{dataset}{
dataset whose values will be displayed in the cells of the heatmap
}
  \item{pvals}{
(optional) p-values based on which the statistical significance of each cell in the heatmap is show.
}
  \item{\dots}{
Additional parameters to be passed on to heat.2. Check the documentation of gplots/heatmap.2 fuction for possible arguments.
}
}
\details{
When pvals matrix is provided, it is assumed that the nummbers are the probability values. They are assumed to be p-values. And they are represented in the heatmap as follows:
* represents p-value < 0.05
** represents p-value < 0.01
*** represents p-value < 0.001
Note, however, that the function does not check if the provided p-values are legitimate probability values.
}
\author{
Gopal Peddinti
}

\examples{
data(dataset)
data(pvals)
draw.heat.2(dataset, pvals)
}
\keyword{heatmap}
\keyword{p-values}
