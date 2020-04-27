
#' Prepare the TCGA RNA-Seq data
#'
#' @param data_dir string, the dir which including all the TCGA RNA data files
#' @param sample_sheet_dir string, the dir which including the sample sheet
#' @param primary_only boolean, wheather to keep only the primary sample
#'
#' @return tibble, the Expression data
#' @export
#'
#' @examples
my_prepare_TCGA_RNA<-function(data_dir, sample_sheet_dir, primary_only=TRUE){

  require(dplyr)

  temp<-read.table(sample_sheet_dir, header = T, sep = "\t")
  sample_sheet<-as_tibble(temp) %>%
    transmute(file_ID=as.character(File.ID),
              file_name=as.character(File.Name),
              case_ID=as.character(Case.ID),
              sample_ID=as.character(Sample.ID),
              sample_type=as.character(Sample.Type))

  if (primary_only){
    sample_sheet<-sample_sheet %>%
      filter(sample_type=="Primary Tumor") %>%
      filter(substr(sample_ID,14,16)=="01A")
  }

  sample_sheet<-sample_sheet %>%
    mutate(filePath=paste(work_dir,file_ID,file_name,sep = "/")) %>%
    distinct(case_ID,.keep_all = T)

  filePath<-sample_sheet$filePath
  tot_case<-length(filePath)
  name_sample<-sample_sheet$case_ID

  get_data<-function(i){
    temp<-read.table(filePath[i], header=F, sep="\t") %>%
      as_tibble() %>% mutate(Gene=as.character(V1))
    temp[[name_sample[i]]]<-temp$V2
    temp<-temp %>% dplyr::select(c(3,4))
    return(temp)
  }

  message("Reading the data...")
  data_list<-purrr::map(c(1:tot_case), ~get_data(.))

  message("Combining...")
  data_combine<-data_list %>% purrr::map(~(.x[,2])) %>% purrr::reduce(bind_cols)
  data_combine<-bind_cols(data_list[[1]][,1],data_combine)

  message("Checking the rows...")
  name_combine<-data_list %>% purrr::map(~(.x[,1])) %>% purrr::reduce(bind_cols)
  check_same<-function(x){ if (length(unique(x))==1){ return(0) } else { return(1) } }
  name_combine<-as_tibble(t(name_combine))
  check_result<-purrr::map(name_combine,~check_same(.)) %>% purrr::reduce(sum)

  if (check_result>0){
    warning("The rows are not matched, now rematch them...")
    data_combine<-data_list[[1]]
    for (i in 2:length(data_list)){
      data_combine<-inner_join(data_combine,data_list[[i]],by="Gene")
    }
  }

  data_2<-data_combine %>% mutate(ENSEMBL=substr(Gene,1,15))

  require(clusterProfiler)
  require(org.Hs.eg.db)
  gene_ensg<-as.character(data_2$ENSEMBL)
  gene_symbol<-bitr(gene_ensg, fromType="ENSEMBL", toType="SYMBOL", OrgDb="org.Hs.eg.db")

  data_3<-inner_join(data_2,gene_symbol,by="ENSEMBL") %>% as_tibble %>%
    mutate(ID=SYMBOL) %>% dplyr::select(ID,c(2:(length(name_sample)+1)))

  return(data_3)
}
