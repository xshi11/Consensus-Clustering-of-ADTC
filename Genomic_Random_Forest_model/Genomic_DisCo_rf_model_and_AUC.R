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
load("genomic_disco_data_raw.Rdata")

factor_select_raw <- read.xlsx(
  xlsxFile <- "genomic_rf_factor_select.xlsx",
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

factor_source <- "model"

factor_select <- factor_select_raw[, factor_source][!is.na(factor_select_raw[, factor_source])]

genomic_train <- genomic_train_raw[, c(factor_select, "Consensus_Cluster")]
genomic_valid <- genomic_valid_raw[, c(factor_select, "Consensus_Cluster")]

set.seed(123)
rf_model <- randomForest(
  Consensus_Cluster ~ .,
  data <- genomic_train,
  ntree <- 500,
  importance <- TRUE
)

genomic_valid_predict <- predict(rf_model, genomic_valid)
accuracy <- sum(diag(table(genomic_valid_predict, genomic_valid$Consensus_Cluster))) / nrow(genomic_valid)
table(genomic_valid_predict)

genomic_valid_predict_prob <- predict(rf_model, genomic_valid, type <- "prob")
genomic_valid_predict_roc <- genomic_valid %>%
  select(Consensus_Cluster) %>%
  bind_cols(
    as.data.frame(genomic_valid_predict_prob) %>%
      set_names(levels(genomic_valid$Consensus_Cluster))
  )
handtill_auc <- roc_auc(
  genomic_valid_predict_roc,
  truth <- Consensus_Cluster,
  !!!syms(levels(genomic_valid$Consensus_Cluster)),  # 动态传递所有类别概率列
  estimator <- "hand_till"  # 关键！指定多分类方法
)
handtill_auc <- as.matrix(t(handtill_auc))

