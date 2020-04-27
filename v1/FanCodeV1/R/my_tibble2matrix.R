
#' transform tibble to matrix with rownames
#'
#' @param data tibble
#'
#' @return matrix
#' @export
#'
#' @examples
my_tibble2matrix<-function(data){
  data_mat<-as.matrix(data[,-1])
  rownames(data_mat)<-data[[1]]
  return(data_mat)
}
