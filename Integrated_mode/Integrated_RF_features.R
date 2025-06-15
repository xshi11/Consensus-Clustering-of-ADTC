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
  library(splines)
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

integrated_train_predict <- predict(rf_model, integrated_train)
compare_integrated_train <- table(integrated_train_predict, integrated_train$Consensus_Cluster, dnn <- c('Predicted', 'Actual'))
compare_integrated_train
sum(diag(compare_integrated_train)/sum(compare_integrated_train))

integrated_valid_predict <- predict(rf_model, integrated_valid)
accuracy <- sum(diag(table(integrated_valid_predict, integrated_valid$Consensus_Cluster))) / nrow(integrated_valid)
table(integrated_valid_predict)

confusionMatrix(integrated_valid_predict, integrated_valid$Consensus_Cluster)

integrated_train.cv <- replicate(10, rfcv(integrated_train[-ncol(integrated_train)], integrated_train$Consensus_Cluster, cv.fold <- 10,step <- 0.8), simplify <- FALSE)
integrated_train.cv <- data.frame(sapply(integrated_train.cv, '[[', 'error.cv'))
integrated_train.cv$factors <- rownames(integrated_train.cv)
integrated_train.cv <- reshape2::melt(integrated_train.cv, id <- 'factors')
integrated_train.cv$factors <- as.numeric(as.character(integrated_train.cv$factors))

p <- ggplot(integrated_train.cv, aes(factors, value)) +
  geom_smooth(se <- FALSE,	method <- 'glm', formula <- y~bs(x,3)) +
  theme(panel.grid <- element_blank(), panel.background <- element_rect(color <- 'black', fill <- 'transparent')) +  
  labs(title <- '',x <- 'Number of factors', y <- 'Cross-validation error')

p

p + geom_vline(xintercept <- 3)

varImpPlot(rf_model, main <- "gene_mutation_importance")
importance_rf_model <- data.frame(importance(rf_model))
importance_rf_model_MDA <- row.names(importance_rf_model[order(importance_rf_model$MeanDecreaseAccuracy, decreasing <- TRUE), ])
importance_rf_model_MDG <- row.names(importance_rf_model[order(importance_rf_model$MeanDecreaseGini, decreasing <- TRUE), ])

write.xlsx(importance_rf_model, file <- "integrated_DisCo_rf_importance.xlsx")