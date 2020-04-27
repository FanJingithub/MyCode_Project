
#' Generate the data for GSEA
#'
#' @param data tibble, feature by row and case by col
#' @param group_info tibble, with two col, first is case_id and second is group_name
#' @param name string, which is used to name the file
#'
#' @return NULL
#' @export
#'
#' @examples
my_prepare_GSEA<-function(data, group_info, name){

  require(dplyr)

  if (!file.exists("GSEA")){
    message("creating GSEA folder...")
    dir.create("GSEA")
  }

  case_info_id<-tibble(case_id=colnames(data)[-1])
  case_info<-inner_join(case_info_id,group_info,by=c("case_id"=colnames(group_info)[1]))
  if (all(group_info[[1]]!=colnames(data)[-1])){
    message("Re-arranged!")
  }
  group<-case_info[[colnames(group_info)[2]]]
  G1_name<-unique(group)[1]
  G2_name<-unique(group)[2]

  gene_names<-data[[1]]
  temp<-bind_cols(tibble(gene_name=gene_names,
                         description=rep("NA",length(gene_names))),
                  data[,-1])
  n_row<-dim(temp)[1]
  n_col<-dim(temp)[2]
  gct_data<-rbind(c("#2.1",rep("",n_col-1)),
                  c(n_row,n_col-2,rep("",n_col-2)),
                  c(colnames(temp)),
                  as.matrix(temp))

  write.table(gct_data, file=paste("GSEA/",name,".gct",sep=""), sep="\t", quote = F,
              row.names = F, col.names = F)

  cls_data<-rbind(c(n_col-2,2,1,rep("",length(group)-3)),
                  c("#",G1_name,G2_name,rep("",length(group)-3)),
                  group)
  write.table(cls_data, file=paste("GSEA/",name,".cls",sep=""), sep="\t", quote = F,
              row.names = F, col.names = F)
}
