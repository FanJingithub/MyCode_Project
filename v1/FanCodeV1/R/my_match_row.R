
#' Re-arrange the row to make the order of the two data is same
#'
#' @param data_1 tibble
#' @param data_2 tibble
#' @param by_1
#' @param by_2
#' @param method string, "inner" - like inner join, "left" - only keep the row of data_1, "right" - only keep the row of data_2
#'
#' @return list, including two tibble
#' @export
#'
#' @examples
my_match_row<-function(data_1, data_2, by_1=1, by_2=1, method="inner"){

  if (method=="right"){
    temp<-data_1
    data_1<-data_2
    data_2<-temp
    method<-"left"
  }
  rowname_1<-data_1[[by_1]]
  rowname_2<-data_2[[by_2]]
  new_index_1<-vector()
  new_index_2<-vector()
  if (method=="inner"){
    match_row<-intersect(rowname_1,rowname_2)
  } else if (method=="left"){
    match_row<-pmatch(rowname_1,rowname_2)
    if (length(na.omit(match_row))!=length(match_row)){
      message("Error: some row of data_1 can't be found in data_2!")
      return(1)
    }
  } else {
    message("Error: match method is not supported!")
    return(1)
  }

  new_index_1<-pmatch(match_row,rowname_1)
  new_index_2<-pmatch(match_row,rowname_2)
  match_res<-list(data_1[new_index_1,], data_2[new_index_2,])
  return(match_res)
}
