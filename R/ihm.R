ihm <- function(dataset, pvals=NULL, xlas=2,
  clusterColumns = FALSE, clusterRows = FALSE,
  na.omit = TRUE, device = "X11", file = NULL,
  res = 96,
  rowmarginCorrection = 0,
  colmarginCorrection = 0,
  keyheightCorrection = 0,
  keywidthCorrection = 0,
  rowclusterheightCorrection = 0,
  colclusterheightCorrection = 0,
  imagewidthCorrection = 0,
  imageheightCorrection = 0,
  rowlabelsizeCorrection = 0,
  collabelsizeCorrection = 0,
  cellnotesizeCorrection = 0,
  keylabelsizeCorrection = 0,
  canvasWidthCorrection = 0,
  canvasHeightCorrection = 0,
  colsidecolorheightCorrection = 0,
  rowsidecolorwidthCorrection = 0,
  breaks,
  ...)
{
  if(clusterColumns && clusterRows) {
    dendrogram = "both"
    Rowv = TRUE
    Colv = TRUE
    colclustheight <- 0.5
    rowclustwidth <- 0.5
  } else if(clusterColumns) {
    dendrogram = "column"
    Rowv = FALSE
    Colv = TRUE
    colclustheight <- 0.5
    rowclustwidth <- 0.01
  } else if(clusterRows) {
    dendrogram = "row"
    Rowv = TRUE
    Colv = FALSE
    colclustheight <- 0.01
    rowclustwidth <- 0.5
  } else {
    dendrogram = "none"
    Rowv = FALSE
    Colv = FALSE
    colclustheight <- 0.01
    rowclustwidth <- 0.01
  }  
  
  if(na.omit) {
    removeRows <- na.action(na.omit(dataset))
    if(length(removeRows)>0) {
      dataset <- dataset[-removeRows,]
      rnames <- rownames(dataset)
      dataset <- apply(dataset, 2, as.numeric)
      rownames(dataset) <- rnames
      if(!is.null(pvals)) {
        pvals <- pvals[-removeRows,]
      }
    }
  }
  
  rnames<-rownames(dataset)
  dataset<-apply(dataset, 2, as.numeric)
  rownames(dataset) <- rnames
  
  imageheight = max(1, 20*plogis(nrow(dataset), location=20, scale=10) + imageheightCorrection)
  imagewidth= max(1, 20*plogis(ncol(dataset), location=20, scale=10) + imagewidthCorrection)
  keyheight = max(0.5, min(0.7, (2 - plogis(nrow(dataset), location=20, scale=100))) + keyheightCorrection)
  keywidth = max(1, 0.35*imagewidth) + keywidthCorrection
  cexR = max(0.5, ifelse(nrow(dataset) < 300, 2, 1) * keyheight * (log(imageheight) / (1+log(nrow(dataset)))) + rowlabelsizeCorrection)
  cexC = max(0.5, ifelse(nrow(dataset) < 300, 2, 1) * keyheight * (log(imagewidth) / (1+log(ncol(dataset)))) + collabelsizeCorrection)
  cexKey = max(cexR, cexC) + keylabelsizeCorrection
  noteCex = max(0.5, max(cexR, cexC) + cellnotesizeCorrection)
  rowmargin = max(1, 1 + max(nchar(rownames(dataset))) + rowmarginCorrection)
  colmargin = max(1, 1 + keyheight + max(nchar(colnames(dataset))) + colmarginCorrection)
  
  titleheight <- 1
  colclustheight <- max(1.5, titleheight + colclustheight + colclusterheightCorrection)
  rowclustwidth <- max(0.5, rowclustwidth + rowclusterheightCorrection)
  
  canvasWidth <- max(1, rowclustwidth+imagewidth+0.2*rowmargin + canvasWidthCorrection)
  canvasHeight <- max(1, colclustheight+imageheight+keyheight+0.2*colmargin + canvasHeightCorrection)
  
  if(hasArg(ColSideColors) && hasArg(RowSideColors)) {
    lmat <- rbind(c(0, 0, 5), c(0, 0, 2), c(4, 1, 3), c(0, 0, 6))
    colsidecolorheight <- max(0.2, 0.2+colsidecolorheightCorrection)
    rowsidecolorwidth <- max(0.2, 0.2+rowsidecolorwidthCorrection)
    lhei <- c(colclustheight, colsidecolorheight, imageheight, keyheight)
    lwid <- c(rowclustwidth, rowsidecolorwidth, imagewidth)
  } else if(hasArg(ColSideColors)) {
    lmat <- rbind(c(0, 4), c(0, 1), c(3, 2), c(0,5))
    colsidecolorheight <- max(0.2, 0.2+colsidecolorheightCorrection)
    lhei <- c(colclustheight, colsidecolorheight, imageheight, keyheight)
    lwid <- c(rowclustwidth, imagewidth)
  } else if(hasArg(RowSideColors)) {
    lmat <- rbind(c(0, 0, 4), c(3, 1, 2), c(0, 0, 5))
    lhei <- c(colclustheight, imageheight, keyheight)
    rowsidecolorwidth <- max(0.2, 0.2+rowsidecolorwidthCorrection)
    lwid <- c(rowclustwidth, rowsidecolorwidth, imagewidth)
  } else {
    lmat <- rbind(c(0, 3), c(2,1), c(0,4))
    lhei <- c(colclustheight, imageheight, keyheight)
    lwid <- c(rowclustwidth, imagewidth)    
  }  
  
  switch(device,
    "pdf"=pdf(file, width=canvasWidth, height=canvasHeight),
    "eps"=postscript(file, width=canvasWidth, height=canvasHeight),
    "png"=png(file, width=res*canvasWidth, height=res*canvasHeight, res=res),
    "bmp"=bmp(file, width=res*canvasWidth, height=res*canvasHeight, res=res),
    "jpeg"=jpeg(file, width=res*canvasWidth, height=res*canvasHeight, res=res),
    "tiff"=tiff(file, width=res*canvasWidth, height=res*canvasHeight, res=res),
    "wmf"=win.metafile(file, width=canvasWidth, height=canvasHeight),
    X11(width=canvasWidth, height=canvasHeight)
  )
  
  if(!missing(breaks)) {
    br <- makebreaks(dataset, breaks)
  } else {
    br <- makebreaks(dataset)
  }

  if(is.null(pvals)){    
    heat.2(x=dataset,
      trace="none",
      col=br[["colors"]],
      breaks=br[["breaks"]],
      density.info="none",	
      notecol="black",
      dendrogram=dendrogram,
      Rowv = Rowv,
      Colv = Colv,
      lmat=lmat, lhei=lhei, lwid=lwid,
      margins=c(colmargin, rowmargin),
      cexRow=cexR, cexCol=cexC, notecex=noteCex, xlas=xlas,
      symkey=FALSE, keywidth=keywidth, keyheight=keyheight,
      cexKey=cexKey,
      ...
    )
  } else{
    draw.heat.2(dataset=dataset, pvals=pvals,
      dendrogram=dendrogram,
      col=br[["colors"]],
      breaks=br[["breaks"]],
      Rowv = Rowv,
      Colv = Colv,
      lmat=lmat, lhei=lhei, lwid=lwid,
      margins=c(colmargin, rowmargin),
      xlas=xlas, cexRow=cexR, cexCol= cexC, notecex=noteCex,
      keywidth=keywidth, keyheight=keyheight,
      cexKey=cexKey, ...
    )
  }
  switch(device,
    "pdf"=dev.off(),
    "eps"=dev.off(),
    "png"=dev.off(),
    "bmp"=dev.off(),
    "jpeg"=dev.off(),
    "tiff"=dev.off(),
    "wmf"=dev.off()
  )
}

