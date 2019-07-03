makebreaks <- function(dataset, breaks) {
  minbr <- min(dataset, na.rm=TRUE)
  maxbr <- max(dataset, na.rm=TRUE)
  if(minbr == maxbr) {
    stop("min and max are identical in the dataset. Quitting")
  }
  
  breaklength <- 256
  computebreaks <- TRUE
  if(!missing(breaks)) {
    if(length(breaks)==1) {
      breaklength <- breaks
      computebreaks <- TRUE
    } else {
      mybreaks <- breaks
      computebreaks <- FALSE
    }
  }
  
  if(minbr >= 0) { ## i.e. if there are no negative numbers
    if(computebreaks) {
      mybreaks <- seq(from=0, to=maxbr, length.out=breaklength)
    }
    mycolors <- colorpanel(length(mybreaks)-1, "white", "red")
  }
  
  if(maxbr <= 0) { ## i.e. if there are no positive numbers
    if(computebreaks) {
      mybreaks <- seq(from=minbr, to=0, length.out=breaklength)
    }
    mycolors <- colorpanel(length(mybreaks)-1, "blue", "white")
  }
  
  if((minbr < 0) && (maxbr > 0)) {
    if(computebreaks) {
      mybreaks <- unique(c(seq(from=minbr, to=0, length.out=breaklength),
        seq(from=0, to=maxbr, length.out=breaklength)))
    }
    mycolors <- colorpanel(length(mybreaks)-1, "blue", "white", "red")
  }
  return(list("breaks"=mybreaks,
    "colors"=mycolors))
}