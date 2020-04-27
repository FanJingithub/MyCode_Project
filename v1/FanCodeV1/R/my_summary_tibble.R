
#' Extended summary for tibble
#'
#' @param data tibble
#' @param fun function
#'
#' @return vector
#' @export
#'
#' @examples
my_summary_tibble<-function(data,fun){ return( map(data,fun) %>% unlist() %>% as.vector() ) }
