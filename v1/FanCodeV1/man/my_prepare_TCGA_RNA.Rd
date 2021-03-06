% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_prepare_TCGA_RNA.R
\name{my_prepare_TCGA_RNA}
\alias{my_prepare_TCGA_RNA}
\title{Prepare the TCGA RNA-Seq data}
\usage{
my_prepare_TCGA_RNA(data_dir, sample_sheet_dir, primary_only = TRUE)
}
\arguments{
\item{data_dir}{string, the dir which including all the TCGA RNA data files}

\item{sample_sheet_dir}{string, the dir which including the sample sheet}

\item{primary_only}{boolean, wheather to keep only the primary sample}
}
\value{
tibble, the Expression data
}
\description{
Prepare the TCGA RNA-Seq data
}
