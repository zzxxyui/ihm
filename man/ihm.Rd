\name{ihm}
\alias{ihm}
\title{
The main user function of this package
}
\description{
This function automatically computes the dimensions and draws heatmap optionally with p-values. This is the main interface of this package. All the other functions are used appropriately by this.
}
\usage{
ihm(dataset, pvals = NULL, xlas = 2, clusterColumns = FALSE, clusterRows = FALSE, na.omit = TRUE, device = "X11", file = NULL, res=96, rowmarginCorrection = 0, colmarginCorrection = 0, keyheightCorrection = 0, keywidthCorrection = 0, rowclusterheightCorrection = 0, colclusterheightCorrection = 0, imagewidthCorrection = 0, imageheightCorrection = 0, rowlabelsizeCorrection = 0, collabelsizeCorrection = 0, cellnotesizeCorrection = 0, canvasWidthCorrection = 0, canvasHeightCorrection = 0, colsidecolorheightCorrection = 0, rowsidecolorwidthCorrection = 0, breaks, ...)
}
\arguments{
  \item{dataset}{
The dataset to be displayed in the heatmap
}
  \item{pvals}{
(optional) p-values to mark the cells of the heatmap
}
  \item{xlas}{
If xlas=1, the column labels are show horizontally, and if xlas=2, they are shown vertically. xlas=1 is recommended only when the column labels are short enough to be displayed properly.
}
  \item{clusterColumns}{
A logical value representing whether the columns of the heatmap are to be clustered, and the column dendrogram to be drawn. Default value of FALSE supresses the clustering
}
  \item{clusterRows}{
A logical value representing whether the rows of the heatmap are to be clustered, and the row dendrogram to be drawn. Default value of FALSE supresses the clustering
}
  \item{na.omit}{
A logical value indicating whether the rows of the dataset to be removed if they contain NA's.
}
  \item{device}{
Name of the device to which the output has to be written. The default value, X11 lets the figure to be displayed to the user. The possible values are ``pdf'', ``eps'', ``png'', ``bmp'', ``jpeg'', ``tiff'' or ``wmf''. ``wmf'' works only on windows. When you want to create an output file, use thish argument instead of calling the device manually.
}
  \item{file}{
Output file name. It must be provided along with the device argument.
}
  \item{res}{
Resolution in ppi used in a bitmap image. This is the same as the argument 'res' of 'bmp', 'png', 'jpeg' or 'tiff' devices.
}
  \item{rowmarginCorrection}{
Correction for row margin. A positive value would increase the margin size and a negative value would decrease the margin.
}
  \item{colmarginCorrection}{
Correction for column margin. A positive value would increase the margin size and a negative value would decrease the margin.
}
  \item{keyheightCorrection}{
Correction for the height of the color key. A positive value would increase the key height and a negative value would decrease it.
}
  \item{keywidthCorrection}{
Correction for the width of the color key. A positive value would increase the key width and a negative value would decrease it.
}
  \item{rowclusterheightCorrection}{
Correction for the height of the row dendrogram. A positive value would increase the height and a negative value would decrease it.
}
  \item{colclusterheightCorrection}{
Correction for the height of the column dendrogram. A positive value would increase the height and a negative value would decrease it.
}
  \item{imagewidthCorrection}{
Correction for the width of the heatmap image part. A positive value would increase the width and a negative value would decrease it.
}
  \item{imageheightCorrection}{
Correction for the heigth of the heatmap image part. A positive value would increase the height and a negative value would decrease it.
}
  \item{rowlabelsizeCorrection}{
Correction for the font size of the row labels. A positive value would increase the font size and a negative value would decrease it.
}
  \item{collabelsizeCorrection}{
Correction for the font size of the column labels. A positive value would increase the font size and a negative value would decrease it.
}
  \item{cellnotesizeCorrection}{
Correction for the font size of the cell annotation (i.e. *'s). A positive value would increase the font size and a negative value would decrease it.
}
  \item{canvasWidthCorrection}{
Correction for the width of the figure canvas. A positive value would increase the width and a negative value would decrease it.
}
  \item{canvasHeightCorrection}{
Correction for the height of the figure canvas. A positive value would increase the height and a negative value would decrease it.
}
  \item{colsidecolorheightCorrection}{
Correction for the height of the column annotation (ColSideColors).
}
  \item{rowsidecolorwidthCorrection}{
Correction for the width of the row annotation (ColSideColors).
}
	\item{breaks}{
	Same as breaks in heatmap.2.
	}
  \item{\dots}{
Additional arguments to be passed on to heat.2 function. Check the help of gplots/heatmap.2. Nevertheless, take care that the arguments sent here don't contradict with what ihm sends. To be more precise, dendrogram, Colv, Rowv, cexRow, cexCol, notecex, lmat, lwid, lhei, margins, trace, density.info, xlas, col, notecol, breaks are automatically calculated by this function and can't be passed here
}
}
\author{
Gopal Peddinti
}
\note{
In previous versions, it was not possible to send RowSideColors and ColSideColors. It is now possible to use those arguments; please consult the help document of heatmap.2 function.
}

\examples{
\dontrun{data(dataset)}
\dontrun{data(pvals)}
\dontrun{ihm(dataset, pvals)}
\dontrun{ihm(dataset, pvals, device="pdf", file="Heatmap.pdf")}
}
\keyword{heatmap}
