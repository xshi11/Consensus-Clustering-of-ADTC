library(openxlsx)

genomic_train_raw <- read.xlsx(
  xlsxFile <- "genomic_train.xlsx",  
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

genomic_gene_set <- read.xlsx(
  xlsxFile <- "gene_set.xlsx",
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

select_factor <- c(genomic_gene_set[,symbol], "Consensus_Cluster")

for (i in select_factor) {
  genomic_train_raw[,i] <- as.factor(genomic_train_raw[,i])
}

genomic_valid_raw <- read.xlsx(
  xlsxFile <- "genomic_valid.xlsx",
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

for (i in select_factor) {
  genomic_valid_raw[,i] <- as.factor(genomic_valid_raw[,i])
}

save(genomic_train_raw, genomic_valid_raw, file <- "genomic_disco_data_raw.Rdata")