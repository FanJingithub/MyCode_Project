
#' Transpose for tibble
#'
#' @param data tibble
#' @param new_name_col_1 string, the name for col_1 of result
#'
#' @return tibble
#' @export
#'
#' @examples
my_t_tibble<-function(data,new_name_col_1){
  temp<-t(data[,-1])
  colnames(temp)<-data[[1]]
  return(as_tibble(temp,rownames=new_name_col_1))
}
