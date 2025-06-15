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
load("integrated_disco_data_raw.Rdata")

factor_select_raw <- read.xlsx(
  xlsxFile <- "integrated_rf_factor_select.xlsx",
  sheet <- 1,
  colNames <- TRUE,
  rowNames <- FALSE
)

factor_source <- "model"

factor_select <- factor_select_raw[, factor_source][!is.na(factor_select_raw[, factor_source])]
genomic_features <- c("BRAF_V600E", "RAS", "TERT_promoter")

integrated_train <- integrated_train[, c(genomic_features, factor_select, "Consensus_Cluster")]
integrated_valid <- integrated_valid[, c(genomic_features, factor_select, "Consensus_Cluster")]

set.seed(123)
rf_model <- randomForest(
  Consensus_Cluster ~ .,
  data <- integrated_train,
  ntree <- 500,
  importance <- TRUE
)

integrated_valid_predict <- predict(rf_model, integrated_valid)
accuracy <- sum(diag(table(integrated_valid_predict, integrated_valid$Consensus_Cluster))) / nrow(integrated_valid)
table(integrated_valid_predict)

integrated_valid_predict_prob <- predict(rf_model, integrated_valid, type <- "prob")
integrated_valid_predict_roc <- integrated_valid %>%
  select(Consensus_Cluster) %>%
  bind_cols(
    as.data.frame(integrated_valid_predict_prob) %>%
      set_names(levels(integrated_valid$Consensus_Cluster))
  )
handtill_auc <- roc_auc(
  integrated_valid_predict_roc,
  truth <- Consensus_Cluster,
  !!!syms(levels(integrated_valid$Consensus_Cluster)),
  estimator <- "hand_till"
)
handtill_auc <- as.matrix(t(handtill_auc))

save(rf_model, file <- "integrated_rf_model.Rdata")