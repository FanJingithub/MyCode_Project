
#' Enrichment analysis using clusterProfiler.
#'
#' @param DEG_list vector, the Diff Expr Genes list
#' @param DB string, "GO", "KEGG", "All"
#' @param sub_DB string, "BP", "MF", "CC", "All"
#' @param pvalueCutoff
#' @param pAdjustMethod
#' @param qvalueCutoff
#'
#' @return list
#' @export
#'
#' @examples
my_enrichment_DEG<-function(DEG_list, DB="GO", sub_DB="BP",
                            pvalueCutoff = 0.05, pAdjustMethod = "BH", qvalueCutoff = 0.2){

  require(clusterProfiler)
  require(org.Hs.eg.db)
  org.db<-"org.Hs.eg.db"

  enrichment_results<-list()

  if (DB=="GO"){
    if (sub_DB %in% c("MF","BP","CC")){
      enrichment_results[[sub_DB]]<- enrichGO(gene = DEG_list, OrgDb = org.db, keyType = "SYMBOL",
                                              ont = sub_DB, pvalueCutoff=pvalueCutoff, readable=T)
    } else if (sub_DB=="All"){
      enrichment_results<-list(
        MF=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="MF", pvalueCutoff=pvalueCutoff, readable=T),
        BP=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="BP", pvalueCutoff=pvalueCutoff, readable=T),
        CC=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="CC", pvalueCutoff=pvalueCutoff, readable=T)
      )
    }
  } else if (DB=="KEGG"){
    transform_id<-bitr(DEG_list, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
    DGE_ENTREZID<-transform_id$ENTREZID
    enrichment_results[["KEGG"]]<-enrichKEGG(gene = DGE_ENTREZID, organism = "hsa", keyType = "kegg",
                                             pvalueCutoff=pvalueCutoff, readable=T)
  } else if (DB=="All"){
    transform_id<-bitr(DEG_list, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
    DGE_ENTREZID<-transform_id$ENTREZID
    enrichment_results<-list(
      MF=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="MF", pvalueCutoff=pvalueCutoff, readable=T),
      BP=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="BP", pvalueCutoff=pvalueCutoff, readable=T),
      CC=enrichGO(gene=DEG_list, OrgDb=org.db, keyType="SYMBOL", ont ="CC", pvalueCutoff=pvalueCutoff, readable=T),
      KEGG=enrichKEGG(gene=DGE_ENTREZID, organism="hsa", keyType="kegg", pvalueCutoff=pvalueCutoff, readable=T)
    )
  } else {
    message(paste("sub_DB type of ",sub_DB," is not supported!"))
    return(1)
  }

  return(enrichment_results)
}
