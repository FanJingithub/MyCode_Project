
#' GSEA analysis using clusterProfiler or fgsea
#'
#' @param genes vector,logFC and genes should be tha same order
#' @param logFC vector
#' @param method string, "gesGO" or "fgsea"
#' @param sub_DB string, "MF","BP","CC", "GO_All", "KEGG", and "GO"("GO" is only for fgsea)
#' @param pvalueCutoff
#' @param gmt_dir string, the gmt dir to perform special geneset analysis
#'
#' @return list
#' @export
#'
#' @examples
my_GSEA<-function(genes, logFC, method="gesGO", sub_DB="MF", pvalueCutoff = 0.05,
                  gmt_dir=NULL){

  require(org.Hs.eg.db)
  org.db<-"org.Hs.eg.db"

  data<-tibble(gene=genes,logFC=logFC)

  transform_id<-bitr(data$gene, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
  temp<-inner_join(data,as_tibble(transform_id),by=c("gene"="SYMBOL")) %>%
    arrange(logFC)
  # arrange(desc(logFC))

  ranks<-temp$logFC
  names(ranks)<-temp$ENTREZID

  GSEA_results<-list()

  if (method=="gesGO"){
    require(clusterProfiler)

    if (sub_DB %in% c("MF","BP","CC")){
      GSEA_results[[sub_DB]] <- gseGO(geneList=ranks, ont=sub_DB,
                                      OrgDb=org.db, pvalueCutoff = 0.05)
    } else if (sub_DB=="GO_All"){
      GSEA_results<-list(MF=gseGO(geneList=ranks,ont="MF",OrgDb=org.db,pvalueCutoff=pvalueCutoff),
                         BP=gseGO(geneList=ranks,ont="BP",OrgDb=org.db,pvalueCutoff=pvalueCutoff),
                         CC=gseGO(geneList=ranks,ont="CC",OrgDb=org.db,pvalueCutoff=pvalueCutoff))
    } else if (sub_DB=="KEGG"){
      GSEA_results[["KEGG"]]<-gseKEGG(ranks, organism = "hsa", pvalueCutoff=pvalueCutoff)
    } else if (sub_DB=="All"){
      GSEA_results<-list(MF=gseGO(geneList=ranks,ont="MF",OrgDb=org.db,pvalueCutoff=pvalueCutoff),
                         BP=gseGO(geneList=ranks,ont="BP",OrgDb=org.db,pvalueCutoff=pvalueCutoff),
                         CC=gseGO(geneList=ranks,ont="CC",OrgDb=org.db,pvalueCutoff=pvalueCutoff),
                         KEGG=gseKEGG(ranks, organism = "hsa", pvalueCutoff=pvalueCutoff))
    } else {
      message(paste("sub_DB type of ",sub_DB," is not supported!"))
      return(1)
    }
  } else if (method=="fgsea"){
    require(fgsea)

    # gmt.file <- system.file("extdata", "mouse.reactome.gmt", package="fgsea")
    # str(head(pathways))
    # rnk.file <- system.file("extdata", "naive.vs.th1.rnk", package="fgsea")
    # ranks <- read.table(rnk.file,
    #                     header=TRUE, colClasses = c("character", "numeric"))
    # ranks <- setNames(ranks$t, ranks$ID)
    # str(ranks)

    data<-tibble(gene=genes,logFC=logFC) %>% arrange(logFC)
    ranks<-data$logFC
    names(ranks)<-data$gene

    if (sub_DB %in% c("KEGG","GO")){

      gmt.file <- paste("D:/data_work/R_sources/GSEA/",sub_DB,".gmt",sep="")
      pathways <- gmtPathways(gmt.file)
      GSEA_results<-list(data=fgsea(pathways, ranks, minSize=15, maxSize=500, nperm=1000),
                         ranks=ranks, pathways=pathways)
    } else {
      if (is.null(gmt_dir)){
        message("gmt file is required!")
        return(1)
      }
    }
  }

  return(GSEA_results)
}
