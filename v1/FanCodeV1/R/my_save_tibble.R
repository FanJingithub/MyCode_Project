
#' Save tibble to file
#'
#' @param data tibble
#' @param file string,filename
#' @param type string, "csv", "tsv", "txt", "xlsx"
#' @param sheetName string
#'
#' @return NULL
#' @export
#'
#' @examples
my_save_tibble<-function(data, file, type=NULL, sheetName="Sheet1"){

  if (is.null(type)){
    temp<-strsplit(file,split = "[.]")[[1]]
    type<-temp[length(temp)]
  }

  if (!(type %in% c("csv","tsv","txt","xlsx"))){
    message("Error: file type is not supported!")
    return(1)
  }

  if (type=="xlsx"){
    require(xlsx)
    data<-as.data.frame(data)
    write.xlsx(data, file, row.names=FALSE, sheetName = sheetName, append = TRUE)
  } else {
    sep<-switch(type, csv=",", tsv="\t", txt="\t")
    write.table(data, file = file, sep = sep, col.names = T, row.names = F, quote = F)
  }
}
