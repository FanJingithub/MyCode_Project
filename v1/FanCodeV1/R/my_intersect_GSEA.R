
#' Get the intersect GSEA results of two datasets.
#'
#' @param data_1 list, which generated from my_load_GSEA
#' @param data_2 list, which generated from my_load_GSEA
#' @param FDR.cutoff
#' @param p.cutoff
#'
#' @return list, with two tibble, for pos and neg
#' @export
#'
#' @examples
my_intersect_GSEA<-function(data_1, data_2, FDR.cutoff=1, p.cutoff=0.5){

  require(dplyr)

  # POS
  list_1<-data_1$pos %>%
    filter(`FDR q-val`<FDR.cutoff) %>%
    filter(`NOM p-val`<p.cutoff) %>%
    dplyr::select(NAME,`NOM p-val`)

  list_2<-data_2$pos %>%
    filter(`FDR q-val`<FDR.cutoff) %>%
    filter(`NOM p-val`<p.cutoff) %>%
    dplyr::select(NAME,`NOM p-val`)

  pos<-inner_join(list_1,list_2,by="NAME")

  # NEG
  list_1<-data_1$neg %>%
    filter(`FDR q-val`<FDR.cutoff) %>%
    filter(`NOM p-val`<p.cutoff) %>%
    dplyr::select(NAME,`NOM p-val`)

  list_2<-data_2$neg %>%
    filter(`FDR q-val`<FDR.cutoff) %>%
    filter(`NOM p-val`<p.cutoff) %>%
    dplyr::select(NAME,`NOM p-val`)

  neg<-inner_join(list_1,list_2,by="NAME")

  return(list(pos=pos,neg=neg))
}
