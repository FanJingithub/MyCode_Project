
#' Load the GSEA plot
#'
#' @param data list, which generated from my_load_GSEA
#' @param geneset string, spcify the geneset to load
#'
#' @return image object, which should be displayed using package EBImage
#' @export
#'
#' @examples
my_GSEA_plot_load<-function(data,geneset){

  require(stringr)
  require(png)
  require(grDevices)
  require(EBImage)

  dir_<-dir(data$dir)
  target_name<-paste("enplot_",geneset,sep="")
  targe_index<-which(str_detect(dir_, target_name))
  target_pic<-dir_[targe_index]
  pic = readImage(paste(data$dir,"\\",target_pic,sep=""))
  return(pic)
}
