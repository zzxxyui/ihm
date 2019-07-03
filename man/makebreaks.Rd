\name{makebreaks}
\alias{makebreaks}
\title{
An internal function to compute the breaks and colors for drawing heatmap
}
\description{
This function computes the breaks and colors that would be used in heamtap functions as arguments. If the numbers in the data set have both negative and positive values, the colors range from blue to red while zero would be white. If all numbers are non-negative the colors range from white (for zero) to red. If all the numbers are non-positive the colors range from blue to white (for zero).
}
\usage{
makebreaks(dataset, breaks)
}
\arguments{
  \item{dataset}{
The dataset to be displayed in the heatmap
}
 \item{breaks}{
The same as breaks in heatmap.2
}
}
\author{
Gopal Peddinti
}

\examples{
data(dataset)
makebreaks(dataset)
}
\keyword{colors}
