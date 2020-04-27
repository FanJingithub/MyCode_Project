
#' Get the volcano plot using ggplot2
#'
#' @param logFC vector, logFC, pvalue and genes should be tha same order
#' @param pvalue vector
#' @param logFC.cutoff
#' @param pvalue.cutoff
#' @param xlab
#' @param ylab
#' @param main
#' @param xlim
#' @param ylim
#' @param linetype
#' @param genes vector
#' @param highlight_genes the genes to be highlight with label
#'
#' @return ggplot object.
#' @export
#'
#' @examples
my_volcanoPlot<-function(logFC, pvalue, logFC.cutoff=1, pvalue.cutoff=0.05,
                         xlab="Log2(Fold change)", ylab="-Log10(Pvalue)",
                         main="", xlim=NULL, ylim=NULL, linetype="solid",
                         genes=NULL, highlight_genes=NULL){

  require(ggplot2)
  require(ggrepel)

  if (is.null(xlim)) xlim<-max(abs(logFC))
  if (is.null(ylim)) ylim<-max(-log10(pvalue))
  if (is.null(highlight_genes)) highlight<-FALSE else highlight<-TRUE

  if (highlight){
    if (is.null(genes)){ cat("Error: gene list is required!\n"); renturn(1)}
    data<-tibble(gene=genes,
                 logFC=logFC, pvalue=pvalue,
                 DEG=rep('notDEG',length(logFC)),
                 color=rep('grey',length(logFC)),
                 marker=rep('',length(logFC)))
    levels<-c("Down","notDEG","Up","Highlight")
    colors<-c("blue","grey","orange","red")
    alphas<-c(0.6,0.5,0.6,1)
  } else {
    data<-tibble(logFC=logFC, pvalue=pvalue,
                 DEG=rep('notDEG',length(logFC)),
                 colors=rep('grey',length(logFC)),
                 marker=rep('',length(logFC)))
    levels<-c("Down","notDEG","Up")
    colors<-c("darkgreen","grey","red")
    alphas<-c(1,0.5,1)
  }

  data$DEG[data$logFC > logFC.cutoff & data$pvalue < pvalue.cutoff] = 'Up'
  data$DEG[data$logFC < -logFC.cutoff & data$pvalue < pvalue.cutoff] = 'Down'
  if (highlight){
    data$DEG[data$gene %in% highlight_genes] = 'Highlight'
    data$marker[data$gene %in% highlight_genes] = data$gene[data$gene %in% highlight_genes]
  }
  data$color = factor(data$DEG,levels=levels,
                      labels=colors)

  base_plot<- ggplot(data, aes(x=logFC,y=-log10(pvalue), label=data$marker))+
    geom_point(aes(color=as.character(data$color), alpha=as.character(data$color)), shape=19)+
    scale_color_manual(values=colors) + scale_alpha_manual(values=alphas)+
    geom_vline(xintercept=c(-logFC.cutoff,logFC.cutoff),linetype=linetype)+
    geom_hline(yintercept=-log10(pvalue.cutoff),linetype=linetype)+
    scale_x_continuous(limits = c(-xlim,xlim)) + scale_y_continuous(limits = c(0,ylim))+
    labs(title=main, x=xlab, y=ylab)+
    theme(panel.grid.major =element_blank(),panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),
          legend.position="none")
  if (highlight==FALSE) {
    result_plot<-base_plot
  } else {
    result_plot<-base_plot +
      geom_text_repel(
        size = 2,
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.3, "lines")
      )
  }
  return(result_plot)
}
