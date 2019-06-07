setwd("~/R Language Default Dir/Github-projects/multiview-oneclass/")
source("R/load-packages.R")
use_condaenv("r-reticulate")
reticulate::source_python("Python/sklearn-outlier-algos.py")




OCSVM_repeated_ODsampling <- function(datasetname, input_normal_sample_size, percentage_OD) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  DToutliers1 <- fread(paste0("data/derived-data/", datasetname, ".results.csv"))
  
  normal_sample_size <- input_normal_sample_size
  
  # Original DT - View 1
  auc_original <- list()
  for(j in 1:50){
    train_id_original <- DToriginal[Label == "no", sample(x = id, size = normal_sample_size * dim(DToriginal[Label == "no"])[1])]
    test_id_original <- setdiff(DToriginal$id, train_id_original)
    
    trainDToriginal <- DToriginal[id %in% train_id_original]
    specificsDTtrainorig <- copy(trainDToriginal[, .(id, Label)])
    trainDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    testDToriginal <- DToriginal[id %in% test_id_original]
    specificsDTtestorig <- copy(testDToriginal[, .(id, Label)])
    testDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    
    OCSVM_scoresDT_original <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToriginal, DTtest = testDToriginal),
                                          Label = specificsDTtestorig$Label,
                                          id = specificsDTtestorig$id)
    
    
    auc_original[[j]] <- OCSVM_scoresDT_original[, auc(Label, Scores)]
  }
  auc_original <- as.data.table(unlist(auc_original))
  auc_original[, Representation:= rep("Original-View", dim(auc_original)[1])]
  # Outliers DT - View 2
  
  
  KNNs1 <- paste0("KNN-00", 1:9)
  KNNs2 <- paste0("KNN-0", 10:99)
  KNNs <- c(KNNs1, KNNs2, "KNN-100")
  sampleKNNs <- sample(x = KNNs, size = percentage_OD * length(KNNs), replace = F)
  DToutliersKNNs <- DToutliers1[, .SD, .SDcols = sampleKNNs]
  
  KNNWs1 <- paste0("KNNW-00", 1:9)
  KNNWs2 <- paste0("KNNW-0", 10:99)
  KNNWs <- c(KNNWs1, KNNWs2, "KNNW-100")
  sampleKNNWs <- sample(x = KNNWs, size = percentage_OD * length(KNNWs), replace = F)
  DToutliersKNNWs <- DToutliers1[, .SD, .SDcols = sampleKNNWs]
  
  LOFs1 <- paste0("LOF-00", 1:9)
  LOFs2 <- paste0("LOF-0", 10:99)
  LOFs <- c(LOFs1, LOFs2, "LOF-100")
  sampleLOFs <- sample(x = LOFs, size = percentage_OD * length(LOFs), replace = F)
  DToutliersLOFs <- DToutliers1[, .SD, .SDcols = sampleLOFs]
  
  SimplifiedLOFs1 <- paste0("SimplifiedLOF-00", 1:9)
  SimplifiedLOFs2 <- paste0("SimplifiedLOF-0", 10:99)
  SimplifiedLOFs <- c(SimplifiedLOFs1, SimplifiedLOFs2, "SimplifiedLOF-100")
  sampleSimplifiedLOFs <- sample(x = SimplifiedLOFs, size = percentage_OD * length(SimplifiedLOFs), replace = F)
  DToutlierssampleSimplifiedLOFs <- DToutliers1[, .SD, .SDcols = sampleSimplifiedLOFs]
  
  LoOPs1 <- paste0("LoOP-00", 1:9)
  LoOPs2 <- paste0("LoOP-0", 10:99)
  LoOPs <- c(LoOPs1, LoOPs2, "LoOP-100")
  sampleLoOPs <- sample(x = LoOPs, size = percentage_OD * length(LoOPs), replace = F)
  DToutliersLoOPs <- DToutliers1[, .SD, .SDcols = sampleLoOPs]
  
  ODINs1 <- paste0("ODIN-00", 1:9)
  ODINs2 <- paste0("ODIN-0", 10:99)
  ODINs <- c(ODINs1, ODINs2, "ODIN-100")
  sampleODINs <- sample(x = ODINs, size = percentage_OD * length(ODINs), replace = F)
  DToutliersODINs <- DToutliers1[, .SD, .SDcols = sampleODINs]
  
  FastABODs1 <- paste0("FastABOD-00", 3:9)
  FastABODs2 <- paste0("FastABOD-0", 10:99)
  FastABODs <- c(FastABODs1, FastABODs2, "FastABOD-100")
  sampleFastABODs <- sample(x = FastABODs, size = percentage_OD * length(FastABODs), replace = F)
  DToutliersFastABODs <- DToutliers1[, .SD, .SDcols = sampleFastABODs]
  
  KDEOSs1 <- paste0("KDEOS-00", 2:9)
  KDEOSs2 <- paste0("KDEOS-0", 10:99)
  KDEOSs <- c(KDEOSs1, KDEOSs2, "KDEOS-100")
  sampleKDEOSs <- sample(x = KDEOSs, size = percentage_OD * length(KDEOSs), replace = F)
  DToutliersKDEOSs <- DToutliers1[, .SD, .SDcols = sampleKDEOSs]
  
  LDFs1 <- paste0("LDF-00", 1:9)
  LDFs2 <- paste0("LDF-0", 10:99)
  LDFs <- c(LDFs1, LDFs2, "LDF-100")
  sampleLDFs <- sample(x = LDFs, size = percentage_OD * length(LDFs), replace = F)
  DToutliersLDFs <- DToutliers1[, .SD, .SDcols = sampleLDFs]
  
  INFLOs1 <- paste0("INFLO-00", 1:9)
  INFLOs2 <- paste0("INFLO-0", 10:99)
  INFLOs <- c(INFLOs1, INFLOs2, "INFLO-100")
  sampleINFLOs <- sample(x = INFLOs, size = percentage_OD * length(INFLOs), replace = F)
  DToutliersINFLOs <- DToutliers1[, .SD, .SDcols = sampleINFLOs]
  
  COFs1 <- paste0("COF-00", 1:9)
  COFs2 <- paste0("COF-0", 10:99)
  COFs <- c(COFs1, COFs2, "COF-100")
  sampleCOFs <- sample(x = COFs, size = percentage_OD * length(COFs), replace = F)
  DToutliersCOFs <- DToutliers1[, .SD, .SDcols = sampleCOFs]
  
  #how many feature to select from each DTview
  select_features <- round((percentage_OD * length(COFs)) / 11)
  
  DToutliers_all <- DToutliers1[, .SD, .SDcols = c(sample(x = KNNs, size = select_features, replace = F),
                                                   sample(x = KNNWs, size = select_features, replace = F),
                                                   sample(x = LOFs, size = select_features, replace = F),
                                                   sample(x = SimplifiedLOFs, size = select_features, replace = F),
                                                   sample(x = LoOPs, size = select_features, replace = F),
                                                   sample(x = ODINs, size = select_features, replace = F),
                                                   sample(x = FastABODs, size = select_features, replace = F),
                                                   sample(x = KDEOSs, size = select_features, replace = F),
                                                   sample(x = LDFs, size = select_features, replace = F),
                                                   sample(x = INFLOs, size = select_features, replace = F),
                                                   sample(x = COFs, size = select_features, replace = F))]
  
  
  list_DTview2 <- list(DToutliersKNNs, DToutliersKNNWs, DToutliersLOFs, 
                       DToutlierssampleSimplifiedLOFs, DToutliersLoOPs,
                       DToutliersODINs, DToutliersFastABODs, DToutliersKDEOSs,
                       DToutliersLDFs, DToutliersINFLOs, DToutliersCOFs,
                       DToutliers_all)
  
  qq <- 0
  for(w in list_DTview2){
    qq <- qq + 1
    if(length(which(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
      print(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])
      print(qq)
      hh <- data.table::transpose(as.data.table(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]]))
      hh[, cols1:=names(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])]
      cols_to_delete <- hh[V1!=0, cols1]
      DT <- list_DTview2[[qq]]
      cols_to_keep <- setdiff(names(DT), cols_to_delete) 
      list_DTview2[[qq]] <- DT[, .SD, .SDcols = cols_to_keep]
      
    }
  }
  
  for(w in list_DTview2){
    if(length(which(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
      print(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]])
    }
  }
  
  auc_DT_list <- list()
  jj <- 1
  for(dataset_view in list_DTview2){
    
    dataset_view[, Label:= DToriginal$Label]
    dataset_view[, id:= DToriginal$id]
    
    auc_dataset_view <- list()
    for(ii in 1:50){
      print(jj)
      train_id_outliers <- dataset_view[Label == "no", sample(x = id, size = normal_sample_size * dim(dataset_view[Label == "no"])[1])]
      test_id_outliers <- setdiff(dataset_view$id, train_id_outliers)
      
      traindataset_view <- dataset_view[id %in% train_id_outliers]
      specificsDTtrain <- copy(traindataset_view[, .(id, Label)])
      traindataset_view[, `:=`(id = NULL, Label = NULL)]
      
      testdataset_view <- dataset_view[id %in% test_id_outliers]
      specificsDTtest <- copy(testdataset_view[, .(id, Label)])
      testdataset_view[, `:=`(id = NULL, Label = NULL)]
      
      
      OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = traindataset_view, DTtest = testdataset_view),
                                       Label = specificsDTtest$Label)
      
      auc_dataset_view[[ii]] <- OCSVM_scoresDT_out[, auc(Label, Scores)]
    }
    temp <- as.data.table(unlist(auc_dataset_view))
    temp[, algo:= paste0("VV", jj)]
    auc_DT_list[[jj]] <- temp
    jj <- jj + 1
  } 
  auc_DT_view2 <- rbindlist(auc_DT_list)
  auc_DT_view2[, Representation:=   c(rep("KNN", 50),
                                      rep("KNNW", 50),
                                      rep("LOF", 50),
                                      rep("SimplifiedLOF", 50),
                                      rep("LoOP", 50),
                                      rep("ODIN", 50),
                                      rep("FastABOD", 50),
                                      rep("KDEOS", 50),
                                      rep("LDF", 50),
                                      rep("INFLO", 50),
                                      rep("COF", 50),
                                      rep("All-Algorithms", 50))]
  
  auc_DT_view2[, algo:=NULL]
  final_DT <- rbindlist(list(auc_DT_view2, auc_original))
  # final_DT[, as.factor(Representation)]
  final_DT[Representation == "Original-View", group:= "1"]
  final_DT[Representation == "All-Algorithms", group:= "2"]
  final_DT[!(group %in% c("1", "2")) , group:= "3"]
  final_DT[, group:= as.factor(group)]
  final_DT[, V1:= as.numeric(V1)]
  
  p <- ggplot(data = final_DT) +
    aes(x = Representation, y = as.numeric(V1), fill = group) +
    geom_boxplot() +
    theme_minimal() + 
    labs(title = paste0(percentage_OD * 100, 
                        "% randomly (just once) selected features for each Representation Algo & ",
                        normal_sample_size* 100, "% repeatedly (50 times) and randomly selected Normal data."),
         subtitle = paste0("Dataset: ", datasetname), y = "AUC") +
    scale_y_continuous(breaks = seq(0.3, 1.0,0.05))   
  
  
  ggsave(plot = p, filename = paste0("figures/", datasetname, 
                                     "_normalsize_", input_normal_sample_size, 
                                     "_percentageOD_", percentage_OD, ".pdf"), 
         width = 12, height = 6, units = "in", dpi = 300)
  # scores_DT <- dplyr::bind_cols(OCSVM_scoresDT_original, OCSVM_scoresDT_out)
  # scores_DT[, 2:=NULL]
  # setnames(scores_DT, old = c(1, 3:4), new = c("scores-view1", "scores-view2", "Label"))
  # scores_DT[, lapply(.SD, function(x) auc(Label, x)), .SDcols = c(1, 3)]
  # 
  # id_view1 <- scores_DT[`scores-view1`> quantile(x = `scores-view1`, probs = given_probs)][, id]
  # id_view2 <- scores_DT[`scores-view2`> quantile(x = `scores-view2`, probs = given_probs)][, id]
  
}