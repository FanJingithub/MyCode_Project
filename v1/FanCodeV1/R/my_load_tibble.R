
#' Load tibble from file
#'
#' @param file string, filename
#' @param type string, "csv", "tsv", "txt", "xlsx"
#' @param sheetName string
#'
#' @return tibble
#' @export
#'
#' @examples
my_load_tibble<-function(file, type=NULL, sheetName=NULL){

  if (is.null(type)){
    temp<-strsplit(file,split = "[.]")[[1]]
    type<-temp[length(temp)]
  }

  if (!(type %in% c("csv","tsv","txt","xlsx"))){
    message("Error: file type is not supported!")
    return(1)
  }

  if (type=="xlsx"){
    require(readxl)
    if (is.null(sheetName)){
      data<-read_excel(file)
    } else {
      data<-read_excel(file, sheet = sheetName)
    }
  } else {
    require(readr)
    sep<-switch(type, csv=",", tsv="\t", txt="\t")
    data<-read.table(file = file, sep = sep, header = T)
  }
  return(data)
}
