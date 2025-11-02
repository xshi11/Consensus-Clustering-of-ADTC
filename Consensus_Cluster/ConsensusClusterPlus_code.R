rm(list = ls())
library("openxlsx")

protein_matrix <- read.xlsx("Protein_matrix.xlsx",sheet = 1, rowNames = T, colNames = T)

protein_matrix2 <- protein_matrix
protein_matrix2$cv <- apply(protein_matrix2,1,function(x) {sd(x,na.rm = T)/mean(x,na.rm =T)})
protein_matrix2 <- protein_matrix2[order(protein_matrix2$cv,decreasing = T),]

protein_matrix3 <- 2^protein_matrix
protein_matrix3$cv <- apply(protein_matrix3,1,function(x) {sd(x,na.rm = T)/mean(x,na.rm =T)})


###### ConsensusClusterPlus ########
library(ConsensusClusterPlus)

Consensus_data <- 2^as.matrix(protein_matrix)[rownames(protein_matrix3[protein_matrix3$cv>0.5,]),]

maxK <- 5
reps <- 500
pItem <- 0.8
pFeature <- 1
clusterAlg <- "kmdist" # 'hc', 'pam', 'km', 'kmdist'
distance <- "pearson" #  'pearson', 'spearman', 'euclidean', 'binary', 'maximum', 'canberra', 'minkowski" 
innerLinkage <- "average"
finalLinkage <- "average"
corUse <- "everything" # 'everything','pairwise.complete.obs', 'complete.obs' 


clustermethod <- 'kmdist'
distancemethod <- 'pearson'

for (tmp.cluster.method in clustermethod) {
  
  for (tmp.distan.method in distancemethod) {
    if(tmp.cluster.method=="kmdist"&&tmp.distan.method=="binary"){
      next
    }else{
      print(paste0("cluster:",tmp.cluster.method))
      print(paste0("distance:",tmp.distan.method))
      temp_result1 <- ConsensusClusterPlus(d = Consensus_data, maxK = maxK,
                                           reps = reps, pItem = pItem,
                                           pFeature = pFeature,
                                           clusterAlg = tmp.cluster.method,
                                           distance = tmp.distan.method,
                                           title = c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),
                                           plot = "pdf",
                                           ml=NULL,
                                           innerLinkage=innerLinkage,
                                           finalLinkage=finalLinkage,
                                           tmyPal=NULL,seed=50000,
                                           writeTable=TRUE,weightsItem=NULL,
                                           weightsFeature=NULL,verbose=F,corUse=corUse)
      
      
      test <- calcICL(temp_result1,plot="pdf",writeTable=TRUE,title =c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)))
      
      
      subclass2 <- as.data.frame(temp_result1[[2]]$consensusClass)
      colnames(subclass2)[1] <- "cluster"
      print("2 cluster")
      
      write.csv(table(subclass2$cluster),paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","2cluster_result_table_data.csv"))
      
      write.csv(subclass2,paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","2cluster_result_data.csv"))
      
      subclass3 <- as.data.frame(temp_result1[[3]]$consensusClass)
      colnames(subclass3)[1] <- "cluster"
      print("3 cluster")
      
      write.csv(table(subclass3$cluster),paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","3cluster_result_table_data.csv"))
      
      write.csv(subclass3,paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","3cluster_result_data.csv"))
      
    }
    
  }
  
}


save(temp_result1,file = "maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/ConsensusClusterPlus_Result.Rdata")

consensus_k2 <- temp_result1[[2]] 
consensus_k3 <- temp_result1[[3]]  # list index corresponds to k
consensus_k4 <- temp_result1[[4]]
consensus_k5 <- temp_result1[[5]]


BiocManager::install('M3C')
library(M3C)
library(cluster)
library(dplyr)
library(tidyr)
library(factoextra)



calc_pac <- function(consensus_matrix, lower = 0.1, upper = 0.9) {
  cm_vals <- consensus_matrix[upper.tri(consensus_matrix)]
  mean(cm_vals > lower & cm_vals < upper)
}

pac_k2 <- calc_pac(consensus_k2$consensusMatrix)
pac_k3 <- calc_pac(consensus_k3$consensusMatrix)
pac_k4 <- calc_pac(consensus_k4$consensusMatrix)
pac_k5 <- calc_pac(consensus_k5$consensusMatrix)

pac_df <- data.frame(
  K = c(2,3, 4,5),
  PAC = c(pac_k2,pac_k3,pac_k4, pac_k5)
)
pac_df
openxlsx::write.xlsx(pac_df,"maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/PAC_Value.xlsx")


library(M3C)
m3c_res <- M3C::M3C(Consensus_data,
                    iters = 200,
                    maxK = 5,
                    clusteralg = "km",
                    distance = "euclidean",
                    seed = 123)

save(m3c_res,file = "maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/M3C_Result.Rdata")


library(dplyr)
m3c_summary <- M3C::M3CResults(m3c_res) %>%
  filter(K %in% c(3, 4))
m3c_summary


openxlsx::write.xlsx(m3c_res$scores,"maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/M3C_Value.xlsx")



# 3. GAP Statistic

library(cluster)
library(factoextra) 

gap_res <- cluster::clusGap(t(Consensus_data),  
                            FUN = stats::kmeans,
                            K.max = 5,
                            B = 500,
                            nstart = 25,
                            iter.max = 100)

gap_table <- as.data.frame(gap_res$Tab)
gap_table$k <- 1:5
subset(gap_table, k %in% c(3, 4))

openxlsx::write.xlsx(gap_table,"maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/GAP_Value.xlsx")

p1 <- factoextra::fviz_gap_stat(gap_res)
ggsave(filename = "maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/GAP_Value_plot.pdf" ,plot = p1,width = 4,height = 3)

save(gap_res,file = "maxK-5_reps-500_pItem-0.8_clusterAlg-kmdist_distance-pearson_corUse-everything/GAP_Result.Rdata")


