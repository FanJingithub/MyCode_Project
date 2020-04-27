
#' Re-arrange the row and col to make the order of the two data is same
#' The dismatch row or col would be drop.
#'
#' @param data_1 tibble
#' @param data_2 tibble
#'
#' @return list, including two tibble
#' @export
#'
#' @examples
my_match_both_rowcol_inner<-function(data_1, data_2){
  match_col<-my_match_col(data_1,data_2)
  data_1<-match_col[[1]]
  data_2<-match_col[[2]]
  match_row<-my_match_row(data_1,data_2)
  return(match_row)
}
