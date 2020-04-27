
#' Calculate the correlation for two kinds of score
#'
#' @param data_1 tibble/matrix
#' @param data_2 tibble/matrix
#' @param score string, "each_row" - score by row, "each_col" - score by col
#'
#' @return matrix
#' @export
#'
#' @examples
my_calc_cor<-function(data_1, data_2, score="each_row"){

  if (length(intersect(class(data_1),c("tble_df","tbl","data.frame")))>0 &&
      length(intersect(class(data_2),c("tble_df","tbl","data.frame")))>0){
    message("The first col would not calculate, and used as colname and rowname...\n")
  } else if (class(data_1)=="matrix" && class(data_2)=="matrix"){
    if (is.null(rownames(data_1))){ rownames(data_1)<-c(1:dim(data_1)[1]) }
    if (is.null(rownames(data_2))){ rownames(data_2)<-c(1:dim(data_2)[1]) }

    data_1<-as_tibble(data_1,rownames="name")
    data_2<-as_tibble(data_2,rownames="name")
  } else {
    message("The data type is not supported!")
    return(1)
  }
  if (score=="each_col"){
    data_1<-my_t_tibble(data=data_1, new_name_col_1="name")
    data_2<-my_t_tibble(data=data_2, new_name_col_1="name")
  }
  if (!all(colnames(data_1)[-1]==colnames(data_2)[-1])){
    message("Error: two data not match!")
    return(0)
  }
  col_name_1<-data_1[[1]]
  col_name_2<-data_2[[1]]

  cor_result<-matrix(rep(0,length(col_name_1)*length(col_name_2)),
                     nrow = length(col_name_1))
  rownames(cor_result)<-col_name_1
  colnames(cor_result)<-col_name_2

  for (j in 1:length(col_name_1)){
    for (k in 1:length(col_name_2)){
      x1<-my_get_tibble_row(data_1,j)
      x2<-my_get_tibble_row(data_2,k)
      cor_result[j,k]<-cor(x1,x2)
    }
  }

  return(cor_result)
}
