
sample_info <- read.xlsx("RAIT_label20231018.xlsx")
del_protein_list <- read.xlsx("建议删除的血液蛋白.xlsx",startRow = 1,colNames = F)

protein_matrix <- read.csv("RAIT_Protein_matrix_with_missing_ratio_0.8_normalized_20231018.csv",check.names = F)
# 8327  115
protein_matrix <- protein_matrix[!as.character(protein_matrix$Protein)%in%del_protein_list$X1,]
# 8250  115
rownames(protein_matrix) <- paste0(protein_matrix$Accession,"_",protein_matrix$Protein)
protein_matrix <- protein_matrix[,-c(grep("Accession",colnames(protein_matrix)),grep("Protein",colnames(protein_matrix)))]



protein_matrix2 <- protein_matrix
protein_matrix2$cv <- apply(protein_matrix2,1,function(x) {sd(x,na.rm = T)/mean(x,na.rm =T)})
protein_matrix2 <- protein_matrix2[order(protein_matrix2$cv,decreasing = T),]

protein_matrix3 <- 2^protein_matrix
protein_matrix3$cv <- apply(protein_matrix3,1,function(x) {sd(x,na.rm = T)/mean(x,na.rm =T)})


###### ConsensusClusterPlus ########
library(ConsensusClusterPlus)

# Consensus_data <- as.matrix(protein_matrix)[rownames(protein_matrix3[protein_matrix3$cv>0.5,]),]

Consensus_data <- 2^as.matrix(protein_matrix)[rownames(protein_matrix3[protein_matrix3$cv>0.5,]),]

maxK <- 5
reps <- 10
pItem <- 0.8
pFeature <- 1
clusterAlg <- "kmdist" # 'hc', 'pam', 'km', 'kmdist'
distance <- "pearson" #  'pearson', 'spearman', 'euclidean', 'binary', 'maximum', 'canberra', 'minkowski" 
innerLinkage <- "average"
finalLinkage <- "average"
corUse <- "everything" # 'everything','pairwise.complete.obs', 'complete.obs' 


# clustermethod <- c('hc', 'pam', 'km', 'kmdist')
# distancemethod <- c('pearson', 'spearman', 'euclidean', 'binary', 'maximum', 'canberra', 'minkowski')

clustermethod <- 'kmdist'
distancemethod <- 'pearson'

####### 原始值的结果 CV >0.5
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
      subclass2$是否摄碘 <- sample_info$是否摄碘1摄碘2碘抵抗[match(rownames(subclass2),sample_info$FUSCC)]
      colnames(subclass2)[1] <- "cluster"
      print("2 cluster")
      print(table(subclass2$cluster,subclass2$是否摄碘))
      
      write.csv(table(subclass2$cluster,subclass2$是否摄碘),paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","2cluster_result_table_data.csv"))
      
      write.csv(subclass2,paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","2cluster_result_data.csv"))
      
      subclass3 <- as.data.frame(temp_result1[[3]]$consensusClass)
      subclass3$是否摄碘 <- sample_info$是否摄碘1摄碘2碘抵抗[match(rownames(subclass3),sample_info$FUSCC)]
      colnames(subclass3)[1] <- "cluster"
      print("3 cluster")
      print(table(subclass3$cluster,subclass3$是否摄碘))
      
      write.csv(table(subclass3$cluster,subclass3$是否摄碘),paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","3cluster_result_table_data.csv"))
      
      write.csv(subclass3,paste0(c(paste0("maxK-",maxK,"_reps-",reps,"_pItem-",pItem,"_clusterAlg-",tmp.cluster.method,"_distance-",tmp.distan.method,"_corUse-",corUse)),"/","3cluster_result_data.csv"))
      
    }
    
  }
  
}
