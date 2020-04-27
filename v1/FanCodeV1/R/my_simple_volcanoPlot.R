
#' Get simple volcano plot using base plot
#'
#' @param logFC vector, logFC and pvalue should be tha same order
#' @param pvalue vector
#' @param logFC.cutoff
#' @param pvalue.cutoff
#' @param xlab
#' @param ylab
#' @param main
#' @param xlim
#' @param ylim
#' @param linetype
#'
#' @return plot
#' @export
#'
#' @examples
my_simple_volcanoPlot<-function(logFC, pvalue, logFC.cutoff=1, pvalue.cutoff=0.05,
                                xlab="Log2(Fold change)", ylab="-Log10(Pvalue)",
                                main="", xlim=NULL, ylim=NULL, linetype="solid"){

  if (is.null(xlim)) xlim<-max(abs(logFC))
  if (is.null(ylim)) ylim<-max(-log10(pvalue))
  if (linetype=="solid") lty<-1 else if (linetype=="dashed") lty<-2
  data<-tibble(logFC=logFC, pvalue=pvalue,
               DEG=rep('notDEG',length(logFC)),
               colors=rep('grey',length(logFC)))
  # set colors
  data$DEG[data$logFC > logFC.cutoff & data$pvalue < pvalue.cutoff] = 'Up'
  data$DEG[data$logFC < -logFC.cutoff & data$pvalue < pvalue.cutoff] = 'Down'
  data$color = factor(data$DEG,levels=c("Down","notDEG","Up"),
                      labels=c("darkgreen","grey","red"))

  # draw scatter plot
  plot(data$logFC, -log10(data$pvalue), pch=16,
       col=as.character(data$color),
       xlab=xlab, ylab=ylab, cex=0.5, main=main, xlim=c(-xlim,xlim),ylim=c(0,ylim))
  abline(h=-log(pvalue.cutoff,10),col="black",lwd=1,lty=lty)
  abline(v=c(logFC.cutoff,-logFC.cutoff),col="black",lwd=1,lty=lty)
}
