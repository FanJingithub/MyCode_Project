
#' Get the genes of the genesets
#'
#' @param geneset_file string, the file of geneset
#'
#' @return list
#' @export
#'
#' @examples
my_get_geneset_genes<-function(geneset_file){
  Gene_Set_Genes<-list()
  temp<-readLines(geneset_file)
  for (i in 1:length(temp)){
    newLine<-strsplit(temp[i],split ="\t")[[1]]
    new_name<-newLine[1]
    Gene_Set_Genes[[new_name]]<-newLine[-1]
  }
  return(Gene_Set_Genes)
}
