\name{pvals}
\alias{pvals}
\docType{data}
\title{
An example pvals argument for ihm function
}
\description{
A matrix with random data: drawn from a uniform distribution between zero and one. This matrix is in general supposed to be a p-value matrix - exactly the same dimensions as the dataset.
}
\usage{data(pvals)}
\format{
  The format is:
 num [1:50, 1:20] 0.592 0.412 0.581 0.168 0.841 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:50] "row 1" "row 2" "row 3" "row 4" ...
  ..$ : chr [1:20] "col 1" "col 2" "col 3" "col 4" ...
}
\examples{
data(pvals)
}
\keyword{p-values}
