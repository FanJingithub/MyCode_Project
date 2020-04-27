
#' Scale a tibble
#'
#' @param data tibble
#' @param by string, "row" or "col"
#'
#' @return tibble
#' @export
#'
#' @examples
my_scale_tibble<-function(data,by){
  data_mat<-my_tibble2matrix(data)
  scale_mat<-my_scale_matrix(data_mat,by)
  rownames(scale_mat)<-rownames(data_mat)
  colnames(scale_mat)<-colnames(data_mat)
  data_scale<-as_tibble(scale_mat,rownames=colnames(data)[1])
  return(data_scale)
}
