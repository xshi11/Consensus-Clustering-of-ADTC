rm(list <- ls())

library(openxlsx)
library(dplyr)
library(stringr)

integrated_train_raw <- read.xlsx(
  xlsxFile <- "integrated_train.xlsx",  
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

integrated_train <- integrated_train_raw[1:7]

dir<-'cnn_result_2'
all_files_path <- list.files(path <- dir, pattern <- '*.csv$',recursive <- TRUE)
all_files <- all_files_path[c(1:20)]
all_files_dir <- paste0('result_2\\',all_files)
all_files_result <- str_extract(all_files, "\\d+")
all_files_result <- as.numeric(all_files_result)

for (i in 1 : length(all_files_dir)) {
  current_data <- read.csv(all_files_dir[i], stringsAsFactors <- FALSE)
  merged_data <- left_join(
    integrated_train,
    current_data[, c("wsi_name", "X0...","X1...","X2...")],
    by <- "wsi_name"
  )

  new_col_name_CC1 <- paste0("result_", all_files_result[i],"_CC1")
  new_col_name_CC2 <- paste0("result_", all_files_result[i],"_CC2")
  new_col_name_CC3 <- paste0("result_", all_files_result[i],"_CC3")

  integrated_train[[new_col_name_CC1]] <- merged_data$"X0..."
  integrated_train[[new_col_name_CC2]] <- merged_data$"X1..."
  integrated_train[[new_col_name_CC3]] <- merged_data$"X2..."
}

integrated_train[["Consensus_Cluster"]] <- integrated_train_tep$Consensus_Cluster
percent_to_decimal <- function(x) {
  as.numeric(gsub("%", "", x)) / 100
}
integrated_train <- integrated_train %>%
  mutate(across(8:67, percent_to_decimal))

integrated_valid_raw <- read.xlsx(
  xlsxFile <- "integrated_valid.xlsx",  
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

integrated_valid <- integrated_valid_raw[1:7]

dir<-'cnn_result_2'
all_files_path <- list.files(path <- dir, pattern <- '*.csv$',recursive <- TRUE)
all_files <- all_files_path[c(1:20)]
all_files_dir <- paste0('result_2\\',all_files)
all_files_result <- str_extract(all_files, "\\d+")
all_files_result <- as.numeric(all_files_result)

for (i in 1 : length(all_files_dir)) {
  current_data <- read.csv(all_files_dir[i], stringsAsFactors <- FALSE)
  merged_data <- left_join(
    integrated_valid,
    current_data[, c("wsi_name", "X0...","X1...","X2...")],
    by <- "wsi_name"
  )
  
  new_col_name_CC1 <- paste0("result_", all_files_result[i],"_CC1")
  new_col_name_CC2 <- paste0("result_", all_files_result[i],"_CC2")
  new_col_name_CC3 <- paste0("result_", all_files_result[i],"_CC3")
  
  integrated_valid[[new_col_name_CC1]] <- merged_data$"X0..."
  integrated_valid[[new_col_name_CC2]] <- merged_data$"X1..."
  integrated_valid[[new_col_name_CC3]] <- merged_data$"X2..."
}

integrated_valid[["Consensus_Cluster"]] <- integrated_valid_tep$Consensus_Cluster
percent_to_decimal <- function(x) {
  as.numeric(gsub("%", "", x)) / 100
}
integrated_valid <- integrated_valid %>%
  mutate(across(8:67, percent_to_decimal))

save(integrated_train, integrated_valid, file <- "integrated_disco_data_raw.Rdata")