
#' Get the row data of tibble
#'
#' @param data tibble
#' @param row integer/string
#' @param by string, "index" - by index, "name" - by rowname
#' @param keep_first boolean, wheather to keep first col
#'
#' @return vector
#' @export
#'
#' @examples
my_get_tibble_row<-function(data, row, by="index", keep_first=FALSE){

  data_new<-data
  if (keep_first==FALSE) data_new<-data[,-1]

  if (by=="name"){
    row_<-which(data[[1]]==row)
    if (length(row_)==0){
      message(paste("Error: can't find row ",row,"!",sep=""))
      return(1)
    } else {
      row<-row_
    }
  }

  if (length(data_new[[1]])>=row){
    result<-data_new[row,] %>% unlist() %>% as.vector()
  } else {
    message(paste("Error: can't find row ",row,"!",sep=""))
    return(1)
  }

  return(result)
}
