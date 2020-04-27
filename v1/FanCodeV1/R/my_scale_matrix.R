
#' Scale a matrix
#'
#' @param data matrix
#' @param by string, "row" or "col"
#'
#' @return matrix
#' @export
#'
#' @examples
my_scale_matrix<-function(data,by){
  scale_vector<-function(x){ return((x-mean(x))/sd(x)) }
  if (by=="row"){
    data_scale<-t(apply(data, 1, scale_vector))
  } else if (by=="col"){
    data_scale<-apply(data, 2, scale_vector)
  }
  return(data_scale)
}
