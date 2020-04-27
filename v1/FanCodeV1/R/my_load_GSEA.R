
#' Load GSEA results from given dir
#'
#' @param dir string, the directory of the results
#' @param pos_name string, the name of POS group, such as "sensitive"
#' @param neg_name string, the name of NEG group, such as "resistant"
#'
#' @return list, with dir, id, pos_results, neg_results
#' @export
#'
#' @examples
my_load_GSEA<-function(dir, pos_name, neg_name){

  require(rvest)
  require(dplyr)

  id<-strsplit(dir, split ="[.]")[[1]][3]

  table_colnames<-c("index","NAME",	"DETAILS","SIZE",	"ES",	"NES", "NOM p-val",	"FDR q-val",
                    "FWER p-val",	"RANK AT MAX",	"LEADING EDGE")

  temp<-read_html(paste(dir,"\\gsea_report_for_",pos_name,"_",id,".html",sep="")) %>%
    html_nodes("div.richTable table") %>%
    html_table()
  temp<-temp[[1]]
  colnames(temp)<-table_colnames
  pos<-as_tibble(temp)

  temp<-read_html(paste(dir,"\\gsea_report_for_",neg_name,"_",id,".html",sep="")) %>%
    html_nodes("div.richTable table") %>%
    html_table()
  temp<-temp[[1]]
  colnames(temp)<-table_colnames
  neg<-as_tibble(temp)

  load_results<-list(dir=dir, id=id, pos=pos, neg=neg)
  return(load_results)
}
