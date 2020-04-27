
#' Extended summary for tibble row
#'
#' @param data tibble
#' @param fun function
#'
#' @return vector
#' @export
#'
#' @examples
my_summary_tibble_row<-function(data,fun){
  data_mat<-as.matrix(data)
  summry_row<-apply(data_mat, 1, fun)
  return(summry_row)
}
