rm(list <- ls())

library(openxlsx)
library(dplyr)
library(stringr)

excoh3_sample_raw <- read.xlsx(
  xlsxFile <- "excoh3_sample.xlsx",
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)
excoh3_sample <- excoh3_sample_raw
colnames(excoh3_sample)[4] <- "wsi_name"

dir<-'result_excoh3'
all_files <- list.files(path <- dir, pattern <- '*.csv$',recursive <- TRUE)
all_files_dir <- paste0('result_excoh3\\',all_files)
all_files_result <- str_extract(all_files, "\\d+")
all_files_result <- as.numeric(all_files_result)

for (i in 1 : length(all_files_dir)) {
 current_data <- read.csv(all_files_dir[i], stringsAsFactors <- FALSE)
  merged_data <- left_join(
    excoh3_sample,
    current_data[, c("wsi_name", "X0...","X1...","X2...")],
    by <- "wsi_name"
  )
  
  new_col_name_CC1 <- paste0("result_", all_files_result[i],"_CC1")
  new_col_name_CC2 <- paste0("result_", all_files_result[i],"_CC2")
  new_col_name_CC3 <- paste0("result_", all_files_result[i],"_CC3")
  
  excoh3_sample[[new_col_name_CC1]] <- merged_data$"X0..."
  excoh3_sample[[new_col_name_CC2]] <- merged_data$"X1..."
  excoh3_sample[[new_col_name_CC3]] <- merged_data$"X2..."
}

percent_to_decimal <- function(x) {
  as.numeric(gsub("%", "", x)) / 100
}

excoh3_sample <- excoh3_sample %>%
  mutate(across(10:18, percent_to_decimal))

colnames(excoh3_sample)[4] <- "slide_wsi_name.svs"

select_factor <- c("BRAF_V600E", "RAS", "TERT_promoter")

for (i in select_factor) {
  excoh3_sample[,i] <- as.factor(excoh3_sample[,i])
}

save(excoh3_sample, file <- "excoh3_sample_df.Rdata")