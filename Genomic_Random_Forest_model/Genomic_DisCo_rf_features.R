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

genomic_train_predict <- predict(rf_model, genomic_train)
compare_genomic_train <- table(genomic_train_predict, genomic_train$Consensus_Cluster, dnn <- c('Predicted', 'Actual'))
compare_genomic_train
sum(diag(compare_genomic_train)/sum(compare_genomic_train))

genomic_valid_predict <- predict(rf_model, genomic_valid)
accuracy <- sum(diag(table(genomic_valid_predict, genomic_valid$Consensus_Cluster))) / nrow(genomic_valid)
table(genomic_valid_predict)

confusionMatrix(genomic_valid_predict, genomic_valid$Consensus_Cluster)

genomic_train.cv <- replicate(10, rfcv(genomic_train[-ncol(genomic_train)], genomic_train$Consensus_Cluster, cv.fold <- 10,step <- 0.8), simplify <- FALSE)
genomic_train.cv <- data.frame(sapply(genomic_train.cv, '[[', 'error.cv'))
genomic_train.cv$factors <- rownames(genomic_train.cv)
genomic_train.cv <- reshape2::melt(genomic_train.cv, id <- 'factors')
genomic_train.cv$factors <- as.numeric(as.character(genomic_train.cv$factors))

p <- ggplot(genomic_train.cv, aes(factors, value)) +
  geom_smooth(se <- FALSE,	method <- 'glm', formula <- y~bs(x,3)) +
  theme(panel.grid <- element_blank(), panel.background <- element_rect(color <- 'black', fill <- 'transparent')) +  
  labs(title <- '',x <- 'Number of factors', y <- 'Cross-validation error')

p

p + geom_vline(xintercept <- 3)

varImpPlot(rf_model, main <- "gene_mutation_importance")
importance_rf_model <- data.frame(importance(rf_model))
importance_rf_model_MDA <- row.names(importance_rf_model[order(importance_rf_model$MeanDecreaseAccuracy, decreasing <- TRUE), ])
importance_rf_model_MDG <- row.names(importance_rf_model[order(importance_rf_model$MeanDecreaseGini, decreasing <- TRUE), ])

write.xlsx(importance_rf_model, file <- "Genomic_DisCo_rf_importance.xlsx")