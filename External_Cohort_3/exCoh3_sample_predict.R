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

load("excoh3_sample_df.Rdata")
load("integrated_rf_model.Rdata")

excoh3_sample_predict <- excoh3_sample
excoh3_sample_predict$CC_predict <- predict(rf_model, excoh3_sample)
table(excoh3_sample_predict$CC_predict)
excoh3_sample_predict_prob <- predict(rf_model, excoh3_sample, type <- "prob")

write.xlsx(excoh3_sample_predict, file <- "excoh3_sample_predict.xlsx")
