
#' Calculate the tumor purity using package estimate.
#'
#' @param data tibble, feature by row and case by col
#'
#' @return matrix
#' @export
#'
#' @examples
my_calc_purity<-function(data){

  require(estimate)

  if (!file.exists("Purity")){
    message("creating Purity folder...")
    dir.create("Purity")
  }
  message("data type should be array, not RNA-Seq!")
  data_expr<-as.matrix(data)
  colnames(data_expr)<-c(colnames(data)[-1],"")
  write.table(data_expr, file=paste("Purity/input.txt",sep=""), sep="\t", quote = F,
              row.names = F, col.names = T)
  inputfile<-"Purity/input.txt"
  file_2<-"Purity/10412genes.gct"
  # OvarianCancerExpr <- system.file("extdata", "sample_input.txt",
  #                                  package="estimate")
  # read.table(OvarianCancerExpr)[1:4,1:4]
  filterCommonGenes(input.f=inputfile, output.f=outputfile,id="GeneSymbol")

  file_3<-"Purity/estimate_score.gct"
  estimateScore(input.ds = file_2, output.ds=file_3,platform="affymetrix")

  # plotPurity(scores="OV_estimate_score.gct", samples="s516", platform="affymetrix")

  scores=read.table(file_3,skip = 2,header = T)
  rownames(scores)=scores[,1]
  scores=t(scores[,3:ncol(scores)])
  return(scores)
}
