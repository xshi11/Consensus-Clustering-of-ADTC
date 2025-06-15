rm(list <- ls())

suppressPackageStartupMessages({
  library(randomForest)
  library(openxlsx)
  library(ggplot2)
  library(reshape2)
  library(caret)
  library(yardstick)
  library(tidyr)
  library(rlang)
  library(dplyr)
})

load("tcga_sample_df.Rdata")
load("integrated_rf_model.Rdata")

tcga_sample_predict <- tcga_sample
tcga_sample_predict$CC_predict <- predict(rf_model, tcga_sample)
table(tcga_sample_predict$CC_predict)
tcga_sample_predict_prob <- predict(rf_model, tcga_sample, type <- "prob")

write.xlsx(tcga_sample_predict, file <- "tcga_sample_predict.xlsx")
