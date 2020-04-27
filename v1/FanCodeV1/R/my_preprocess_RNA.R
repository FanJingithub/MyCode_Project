
#' Preprocess RNA data
#'
#' @param data tibble
#' @param correctNames
#' @param keep_duplicate
#' @param combine_method
#' @param filterLow
#'
#' @return tibble
#' @export
#'
#' @examples
my_preprocess_RNA<-function(data, correctNames=TRUE, keep_duplicate=FALSE, combine_method=median,
                            filterLow=0.001){

  # colnames correct
  if (correctNames==TRUE){
    if (all(colnames(data)!=make.names(colnames(data)))){
      message("correcting colnames...")
      colnames(data)<-make.names(colnames(data))
    }
  }

  # filter low expression
  data_med<-my_summary_tibble_row(data[,-1],median)
  low_index<-which(data_med<filterLow)
  if (length(low_index)>0){
    cat("fitering low expression...\n")
    data<-data[-low_index,]
  }

  # deal with duplicate gene
  if (keep_duplicate==FALSE){
    gene_names<-data[[1]]
    if (length(gene_names)!=length(unique(gene_names))) {
      cat("combining duplicate gene...\n")
      temp<-as.data.frame(aggregate(data[,-1],by=list(gene_names),FUN=combine_method))
      data<-bind_cols(tibble(gene=temp[,1]),as_tibble(temp[,-1]))
    } else {
      data<-bind_cols(tibble(gene=data[[1]]),as_tibble(data[,-1]))
    }
  }

  return(data)
}
