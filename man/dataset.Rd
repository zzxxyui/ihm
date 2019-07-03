\name{dataset}
\alias{dataset}
\docType{data}
\title{
An example dataset argument for ihm function
}
\description{
A matrix with random data: drawn from a normal distribution with zero mean and unit variance. In a typical case, the dataset would be the matrix of log ratios of case versus control (e.g. in gene expression data) or correspondingly the values of the t-statistic.
}
\usage{data(dataset)}
\format{
  The format is:
 num [1:50, 1:20] -0.0627 -0.1526 1.2595 -1.7508 -0.1818 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:50] "row 1" "row 2" "row 3" "row 4" ...
  ..$ : chr [1:20] "col 1" "col 2" "col 3" "col 4" ...
}
\examples{
data(dataset)
}
\keyword{datasets}
