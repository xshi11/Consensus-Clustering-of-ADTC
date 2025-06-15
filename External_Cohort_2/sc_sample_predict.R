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

load("sc_sample_df.Rdata")
load("integrated_rf_model.Rdata")

sc_sample_predict <- sc_sample
sc_sample_predict$CC_predict <- predict(rf_model, sc_sample)
table(sc_sample_predict$CC_predict)
sc_sample_predict_prob <- predict(rf_model, sc_sample, type <- "prob")

write.xlsx(sc_sample_predict, file <- "sc_sample_predict.xlsx")
