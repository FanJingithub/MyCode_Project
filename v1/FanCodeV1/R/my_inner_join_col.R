
#' The col version of inner_join
#'
#' @param data_1 tibble
#' @param data_2 tibble
#'
#' @return tibble
#' @export
#'
#' @examples
my_inner_join_col<-function(data_1, data_2){
  match_data<-my_match_col(data_1,data_2)
  data_1<-match_data[[1]]
  data_2<-match_data[[2]]
  return(bind_rows(data_1,data_2))
}
