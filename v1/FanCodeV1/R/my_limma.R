
#' limma method to get DEG
#'
#' @param data tibble, gene by row and case by col
#' @param group_info tibble, with two col, first is case_id and second is group_name
#' @param adjust.method
#' @param data_type string, "counts" or "score"
#' @param p.value.cutoff
#' @param fold.change.cutoff
#' @param filterLow
#'
#' @return tibble.
#' @export
#'
#' @examples
my_limma<-function(data,group_info,adjust.method="fdr",data_type="counts",
                   p.value.cutoff=1,fold.change.cutoff=1,
                   filterLow=-100){
  require(dplyr)
  require(limma)
  require(edgeR)

  case_info_id<-tibble(case_id=colnames(data)[-1])
  case_info<-inner_join(case_info_id,group_info,by=c("case_id"=colnames(group_info)[1]))
  if (all(group_info[[1]]!=colnames(data)[-1])){
    message("Re-arranged!")
  }

  group_origin<-case_info[[colnames(group_info)[2]]] %>% as.factor()
  group<-as.factor(paste("G",as.character(as.numeric(group_origin)),sep=""))
  design <- model.matrix(~ 0+group)
  colnames(design) <- levels(group)

  expr<-data[,-1]
  gene_names<-data[[1]]
  if (length(gene_names)!=length(unique(gene_names))) { message("Duplicate genes!") }
  expr_mat<-as.matrix(expr)
  row.names(expr_mat)<-gene_names

  # filter low expression
  data_med<-apply(expr_mat, 1, median)
  low_index<-which(data_med<filterLow)
  if (length(low_index)>0){
    message("fitering low expression!")
    expr_mat<-expr_mat[-low_index,]
  }

  # log2 transform
  ex <- expr_mat
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC) {
    ex[which(ex <= 0)] <- NaN
    expr_mat <- log2(ex)
    message("log transforming...")
  }

  if (data_type=="counts"){
    message("using method for counts...")
    # fit the model
    x <- DGEList(counts=expr_mat, genes=row.names(expr_mat))
    # perform voom normalization
    y <- voom(x,design,plot=TRUE)
    # cluster libraries
    # ? plotMDS(y,xlim=c(-2.5,2.5))
    # fit linear model and assess differential expression
    fit <- lmFit(y, design)

  }
  if (data_type=="score"){
    message("using method for score...")
    # fit the model
    fit <- lmFit(expr_mat, design)
  }
  if ((data_type %in% c("counts","score"))==FALSE){
    message("Error: data type not supported!")
    return(1)
  }
  cont.matrix <- makeContrasts(G2-G1, levels=design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)
  limma_results <- topTable(fit2,adjust.method=adjust.method,coef=1,p.value=p.value.cutoff,
                            lfc=log(fold.change.cutoff,2),number=50000,sort.by = 'logFC')
  return(as_tibble(limma_results,rownames="gene"))
}
