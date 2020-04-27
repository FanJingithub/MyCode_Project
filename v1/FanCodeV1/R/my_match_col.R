
#' Re-arrange the col to make the order of the two data is same
#'
#' @param data_1 tibble
#' @param data_2 tibble
#' @param method string, "inner" - like inner join, "left" - only keep the col of data_1, "right" - only keep the col of data_2
#'
#' @return list, including two tibble
#' @export
#'
#' @examples
my_match_col<-function(data_1, data_2, method="inner"){

  if (method=="right"){
    temp<-data_1
    data_1<-data_2
    data_2<-temp
    method<-"left"
  }
  colname_1<-colnames(data_1)
  colname_2<-colnames(data_2)
  new_index_1<-vector()
  new_index_2<-vector()
  if (method=="inner"){
    match_col<-intersect(colname_1,colname_2)
  } else if (method=="left"){
    match_col<-pmatch(colname_1,colname_2)
    if (length(na.omit(match_col))!=length(match_col)){
      message("Error: some col of data_1 can't be found in data_2!")
      return(1)
    }
  } else {
    message("Error: match method is not supported!")
    return(1)
  }

  new_index_1<-pmatch(match_col,colname_1)
  new_index_2<-pmatch(match_col,colname_2)
  match_res<-list(data_1[,new_index_1], data_2[,new_index_2])
  return(match_res)
}
