draw.heat.2 <-
function(dataset, pvals,...) {
  stars <- matrix("",nrow=nrow(pvals),ncol=ncol(pvals))
	for(j in seq(ncol(pvals)))
	{
		stars[which(pvals[,j]<0.05),j]="*"
		stars[which(pvals[,j]<0.01),j]="**"
		stars[which(pvals[,j]<0.001),j]="***"
	}
  
	heat.2(x=dataset,
		trace="none",
		density.info="none",
		cellnote=stars,
    symkey=FALSE,
		notecol="black",...)
}

