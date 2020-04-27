
#' Wilcoxon ranksum method for diff analysis
#'
#' @param data tibble, feature by row and case by col
#' @param group_info tibble, with two col, first is case_id and second is group_name
#' @param adjust.method
#' @param data_type string, "gene" or other
#' @param p.value.cutoff
#' @param fold.change.cutoff
#' @param FC_method
#'
#' @return tibble
#' @export
#'
#' @examples
my_diff_wilcox<-function(data, group_info, adjust.method="fdr", data_type="gene",
                         p.value.cutoff=1, fold.change.cutoff=1, FC_method=mean){

  require(dplyr)
  require(purrr)

  name<-colnames(data)[1]
  case_info_id<-tibble(case_id=colnames(data)[-1])
  case_info<-inner_join(case_info_id,group_info,by=c("case_id"=colnames(group_info)[1]))
  if (all(group_info[[1]]!=colnames(data)[-1])){
    message("Re-arranged!")
  }
  group_origin<-case_info[[colnames(group_info)[2]]] %>% as.factor()
  group<-as.factor(paste("G",as.character(as.numeric(group_origin)),sep=""))

  index_1<-which(group=="G1")
  index_2<-which(group=="G2")

  if (exists("my_t_tibble")){
    data_t<-my_t_tibble(data, new_name_col_1 = "case")
    calculate_results<-data_t %>% dplyr::select(-1) %>%
      map_dfr(~data.frame(logFC=log2((FC_method(.x[index_1])+0.001)/(FC_method(.x[index_2])+0.001)),
                          pvalue=wilcox.test(.x[index_1],.x[index_2])$p.value))
  } else {
    data_mat<-as.matrix(data[,-1])
    calculate_results<-apply(data_mat, 1, function(x){
      data.frame(logFC=log2((FC_method(x[index_1])+0.001)/(FC_method(x[index_2])+0.001)),
                 pvalue=wilcox.test(x[index_1],x[index_2])$p.value)
    }) %>% map_dfr(~.)
  }

  p_adjust<-p.adjust(calculate_results$pvalue, method=adjust.method)

  if (data_type=="gene"){
    wilcox_results<-bind_cols(tibble(gene=data[[1]]),
                              as_tibble(calculate_results),
                              tibble(p.adjust=p_adjust)) %>%
      filter(pvalue<p.value.cutoff) %>%
      filter(abs(logFC)>log2(fold.change.cutoff)) %>%
      arrange(desc(logFC))
  } else {
    wilcox_results<-bind_cols(tibble(feature=data[[1]]),
                              as_tibble(calculate_results),
                              tibble(p.adjust=p_adjust)) %>%
      filter(pvalue<p.value.cutoff) %>%
      filter(abs(logFC)>log2(fold.change.cutoff)) %>%
      arrange(desc(logFC))
  }

  return(wilcox_results)
}
