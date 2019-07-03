\name{heat.2}
\alias{heat.2}
\title{
A function that does all the work in drawing a heatmap
}
\description{
This is almost entirely the same as gplots/heatmap.2, with just one exception that this function takes xlas argument that determines the orientation of the x-axis labels. Check gplots/heatmap.2 function for more details.
}
\usage{
heat.2(x, Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE, distfun = dist, hclustfun = hclust, dendrogram = c("both", "row", "column", "none"), symm = FALSE, scale = c("none", "row", "column"), na.rm = TRUE, revC = identical(Colv, "Rowv"), add.expr, breaks, symbreaks = min(x < 0, na.rm = TRUE) || scale != "none", col = "heat.colors", colsep, rowsep, sepcolor = "white", sepwidth = c(0.05, 0.05), cellnote, notecex = 1, notecol = "cyan", na.color = par("bg"), trace = c("column", "row", "both", "none"), tracecol = "cyan", hline = median(breaks), vline = median(breaks), linecol = tracecol, margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, labCol = NULL, labRow.color = NULL, labCol.color = NULL, key = TRUE, keysize = 1.5, density.info = c("histogram", "density", "none"), denscol = tracecol, symkey = min(x < 0, na.rm = TRUE) || symbreaks, densadj = 0.25, main = NULL, cexKey=1, xlab = NULL, ylab = NULL, lmat = NULL, lhei = NULL, lwid = NULL, xlas = 2, ...)
}
\arguments{
  \item{x}{
Check gplots/heatmap.2
}
  \item{Rowv}{
Check gplots/heatmap.2
}
  \item{Colv}{
Check gplots/heatmap.2
}
  \item{distfun}{
Check gplots/heatmap.2
}
  \item{hclustfun}{
Check gplots/heatmap.2
}
  \item{dendrogram}{
Check gplots/heatmap.2
}
  \item{symm}{
Check gplots/heatmap.2
}
  \item{scale}{
Check gplots/heatmap.2
}
  \item{na.rm}{
Check gplots/heatmap.2
}
  \item{revC}{
Check gplots/heatmap.2
}
  \item{add.expr}{
Check gplots/heatmap.2
}
  \item{breaks}{
Check gplots/heatmap.2
}
  \item{symbreaks}{
Check gplots/heatmap.2
}
  \item{col}{
Check gplots/heatmap.2
}
  \item{colsep}{
Check gplots/heatmap.2
}
  \item{rowsep}{
Check gplots/heatmap.2
}
  \item{sepcolor}{
Check gplots/heatmap.2
}
  \item{sepwidth}{
Check gplots/heatmap.2
}
  \item{cellnote}{
Check gplots/heatmap.2
}
  \item{notecex}{
Check gplots/heatmap.2
}
  \item{notecol}{
Check gplots/heatmap.2
}
  \item{na.color}{
Check gplots/heatmap.2
}
  \item{trace}{
Check gplots/heatmap.2
}
  \item{tracecol}{
Check gplots/heatmap.2
}
  \item{hline}{
Check gplots/heatmap.2
}
  \item{vline}{
Check gplots/heatmap.2
}
  \item{linecol}{
Check gplots/heatmap.2
}
  \item{margins}{
Check gplots/heatmap.2
}
  \item{ColSideColors}{
Check gplots/heatmap.2
}
  \item{RowSideColors}{
Check gplots/heatmap.2
}
  \item{cexRow}{
Check gplots/heatmap.2
}
  \item{cexCol}{
Check gplots/heatmap.2
}
  \item{labRow}{
Check gplots/heatmap.2
}
  \item{labCol}{
Check gplots/heatmap.2
}
  \item{labRow.color}{
A vector of color names to specify the desired color of row labels. By default it is NULL in which case the row labels are displayed in default black color. The length of this vector, when passed, should be either one or equal to nrow(x). When the argument RowSideColors is passed, often it makes sense to pass the same vector here
}
  \item{labCol.color}{
A vector of color names to specify the desired color of column labels. By default it is NULL in which case all column labels are displayed in default black color. The length of this vector, when passed, should be either one or equal to ncol(x). When the argument ColSideColors is passed, often it makes sense to pass the same vector here
}
  \item{key}{
Check gplots/heatmap.2
}
  \item{keysize}{
Check gplots/heatmap.2
}
  \item{density.info}{
Check gplots/heatmap.2
}
  \item{denscol}{
Check gplots/heatmap.2
}
  \item{symkey}{
Check gplots/heatmap.2
}
  \item{densadj}{
Check gplots/heatmap.2
}
  \item{main}{
Check gplots/heatmap.2
}
  \item{cexKey}{
Point size of the color key labels.
}
  \item{xlab}{
Check gplots/heatmap.2
}
  \item{ylab}{
Check gplots/heatmap.2
}
  \item{lmat}{
Check gplots/heatmap.2
}
  \item{lhei}{
Check gplots/heatmap.2
}
  \item{lwid}{
Check gplots/heatmap.2
}
  \item{xlas}{
xlas can be 1 or 2. If xlas=1, the column labels are displayed horizontally, and if xlas=2, they are drawn vertically.
}
  \item{\dots}{
Check gplots/heatmap.2
}
}
\author{
Gopal Peddinti, gplots authors
}

\examples{
data(dataset)
heat.2(dataset)
}
