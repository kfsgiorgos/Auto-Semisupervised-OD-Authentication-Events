setwd("~/R Language Default Dir/Github-projects/multiview-oneclass/")
source("R/load-packages.R")
use_condaenv("r-reticulate")
reticulate::source_python("Python/sklearn-outlier-algos.py")

# run OCSVM  on original data --------------------------------------------------------------

# test how accuaret each OCSVM is. Repeat random samples of the normal class  -----------------------------------------

DToriginal <- fread("data/derived-data/Shuttle_withoutdupl_norm_v05.csv")
DToutliers1 <- fread("data/derived-data/Shuttle_withoutdupl_norm_v05.results.csv")

normal_sample_size <- 0.2
list1 <- list()
for (i in 1:100 ) {
  train_id_original <- DToriginal[Label == "no", sample(x = id, size = normal_sample_size * dim(DToriginal[Label == "no"])[1])]
  test_id_original <- setdiff(DToriginal$id, train_id_original)
  
  trainDToriginal <- DToriginal[id %in% train_id_original]
  specificsDTtrainorig <- copy(trainDToriginal[, .(id, Label)])
  trainDToriginal[, `:=`(id = NULL, Label = NULL)]
  
  testDToriginal <- DToriginal[id %in% test_id_original]
  specificsDTtestorig <- copy(testDToriginal[, .(id, Label)])
  testDToriginal[, `:=`(id = NULL, Label = NULL)]
  
  
  OCSVM_scoresDT_original <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToriginal, DTtest = testDToriginal),
                               Label = specificsDTtestorig$Label)
  list1[[i]] <- OCSVM_scoresDT_original[Label == "yes" & Scores < 0, .N]
  
}


DToutliers <- DToutliers1[, .(`LOF-004`, `LOF-099`, `KNNW-081`, `KNN-001`, `FastABOD-007`, 
               `FastABOD-076`, `LDOF-006`, `LDOF-019`, `KDEOS-018`, `KDEOS-048`,
               `COF-005`, `COF-055`, `INFLO-044`, `INFLO-075`, `SimplifiedLOF-023`, `SimplifiedLOF-043`, `SimplifiedLOF-001`)]
DToutliers[, `:=` (Label = DToriginal$Label, id = DToriginal$id)]


list2 <- list()
for (i in 1:100) {
  train_id_outliers <- DToutliers[Label == "no", sample(x = id, size = normal_sample_size * dim(DToutliers[Label == "no"])[1])]
  test_id_outliers <- setdiff(DToutliers$id, train_id_outliers)
  
  trainDToutliers <- DToutliers[id %in% train_id_outliers]
  specificsDTtrain <- copy(trainDToutliers[, .(id, Label)])
  trainDToutliers[, `:=`(id = NULL, Label = NULL)]
  
  testDToutliers <- DToutliers[id %in% test_id_outliers]
  specificsDTtest <- copy(testDToutliers[, .(id, Label)])
  testDToutliers[, `:=`(id = NULL, Label = NULL)]
  
  
  OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToutliers, DTtest = testDToutliers),
                               Label = specificsDTtest$Label)
  list2[[i]] <- OCSVM_scoresDT_out[Label == "yes" & Scores < 0, .N]
  
}

DToriginal[,.N , by = Label]
normal_sample_size * dim(DToriginal[Label=="no"])[1]
normal_sample_size
print(paste0("Original performance: ", median(unlist(list1))))
print(paste0("Outlier performance: ", median(unlist(list2))))


# Get OCSVM scores for each view ------------------------------------------------


run_multiview_no_distance <- function(datsetname, input_normal_sample_size, given_probs) {
  DToriginal <- fread(paste0("data/derived-data/", datsetname,".csv"))
  DToutliers1 <- fread(paste0("data/derived-data/", datsetname, ".results.csv"))
  
  normal_sample_size <- input_normal_sample_size
  
  # Original DT - View 1
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
  # Outliers DT - View 2
  DToutliers <- DToutliers1[, .(`LOF-004`, `LOF-099`, `LOF-045`, `LOF-049`, `KNNW-001`, `KNNW-081`, `KNNW-031`, `KNN-001`, `FastABOD-007`, 
                                `FastABOD-076`, `LDOF-006`, `LDOF-019`, `KDEOS-058`, `KDEOS-018`, `KDEOS-048`,
                                `COF-005`, `COF-055`, `INFLO-044`, `INFLO-075`, `SimplifiedLOF-023`, `SimplifiedLOF-043`, `SimplifiedLOF-009`)]
  DToutliers[, `:=` (Label = DToriginal$Label, id = DToriginal$id)]
  
  # if(length(which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)) != 0){
  #   print(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17])
  #   stop(paste0("OD is infinite: ", names(DToutliers)[which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)]))
  #   }
  
  # DToutliers[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:17]
  
  
  train_id_outliers <- DToutliers[Label == "no", sample(x = id, size = normal_sample_size * dim(DToutliers[Label == "no"])[1])]
  test_id_outliers <- setdiff(DToutliers$id, train_id_outliers)
  
  trainDToutliers <- DToutliers[id %in% train_id_outliers]
  specificsDTtrain <- copy(trainDToutliers[, .(id, Label)])
  trainDToutliers[, `:=`(id = NULL, Label = NULL)]
  
  testDToutliers <- DToutliers[id %in% test_id_outliers]
  specificsDTtest <- copy(testDToutliers[, .(id, Label)])
  testDToutliers[, `:=`(id = NULL, Label = NULL)]
  
  
  OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToutliers, DTtest = testDToutliers),
                                   Label = specificsDTtest$Label)
  
  
  scores_DT <- dplyr::bind_cols(OCSVM_scoresDT_original, OCSVM_scoresDT_out)
  scores_DT[, 2:=NULL]
  setnames(scores_DT, old = c(1, 3:4), new = c("scores-view1", "scores-view2", "Label"))
  scores_DT[, lapply(.SD, function(x) auc(Label, x)), .SDcols = c(1, 3)]
  
  id_view1 <- scores_DT[`scores-view1`> quantile(x = `scores-view1`, probs = given_probs)][, id]
  id_view2 <- scores_DT[`scores-view2`> quantile(x = `scores-view2`, probs = given_probs)][, id]
  

  return(list(id_view1, id_view2, specificsDTtrainorig$id))
  
  }

predicted_normal <- run_multiview_no_distance(datsetname = "Ionosphere_withoutdupl_norm", 
                                              input_normal_sample_size = 0.2, given_probs = 0.99)


get_multiview_forloop <- function(datasetname1, input_normal_sample_size1, given_probs1, Iter) {
  DToriginal <- fread(paste0("data/derived-data/", datasetname1,".csv"))
  DToutliers1 <- fread(paste0("data/derived-data/", datasetname1, ".results.csv"))
  
  predicted_normal <- run_multiview_no_distance(datsetname = datasetname1, 
                                                input_normal_sample_size = input_normal_sample_size1, given_probs = given_probs1)

  # Original DT - View 1
  list_id_view1 <- list()
  list_id_view2 <- list()
  for( i in 1:Iter){
    if(i ==1){
      train_id_original <- unique(c(predicted_normal[[2]], predicted_normal[[3]]))
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
      OCSVM_scoresDT_original[, `scores-view1`:= Scores]
      list_id_view1[[i]] <- OCSVM_scoresDT_original[`scores-view1`> quantile(x = `scores-view1`, probs = given_probs1)][, id]
      # Outliers DT - View 2
      DToutliers <- DToutliers1[, .(`LOF-004`, `LOF-099`, `LOF-045`, `LOF-049`, `KNNW-001`, `KNNW-081`, `KNNW-031`, `KNN-001`, `FastABOD-007`, 
                                    `FastABOD-076`, `LDOF-006`, `LDOF-019`, `KDEOS-058`, `KDEOS-018`, `KDEOS-048`,
                                    `COF-005`, `COF-055`, `INFLO-044`, `INFLO-075`, `SimplifiedLOF-023`, `SimplifiedLOF-043`, `SimplifiedLOF-009`)]
      DToutliers[, `:=` (Label = DToriginal$Label, id = DToriginal$id)]
      
      # if(length(which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)) != 0){
      #   print(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17])
      #   stop(paste0("OD is infinite: ", names(DToutliers)[which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)]))
      #   }
      
      # DToutliers[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:17]
      
      
      train_id_outliers <- unique(c(predicted_normal[[1]], predicted_normal[[3]]))
      test_id_outliers <- setdiff(DToutliers$id, train_id_outliers)
      
      trainDToutliers <- copy(DToutliers[id %in% train_id_outliers])
      specificsDTtrain <- copy(trainDToutliers[, .(id, Label)])
      trainDToutliers[, `:=`(id = NULL, Label = NULL)]
      
      testDToutliers <- copy(DToutliers[id %in% test_id_outliers])
      specificsDTtest <- copy(testDToutliers[, .(id, Label)])
      testDToutliers[, `:=`(id = NULL, Label = NULL)]
      
      
      OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToutliers, DTtest = testDToutliers),
                                       Label = specificsDTtest$Label)
      
      
      OCSVM_scoresDT_out[, id:=specificsDTtest$id]
      OCSVM_scoresDT_out[, `scores-view2`:= Scores]
      list_id_view2[[i]] <- OCSVM_scoresDT_out[`scores-view2`> quantile(x = `scores-view2`, probs = given_probs1)][, id]
      
      } else{
        
        train_id_original <- sort(unique(c(predicted_normal[[1]], predicted_normal[[3]], list_id_view2[[i-1]])))
        
        test_id_original1 <- setdiff(DToriginal$id, train_id_original)
        test_id_original <- test_id_original1[!test_id_original1 %in% unlist(list_id_view1[1:i-1])]
        
        
        trainDToriginal <- copy(DToriginal[id %in% train_id_original])
        specificsDTtrainorig <- copy(trainDToriginal[, .(id, Label)])
        trainDToriginal[, `:=`(id = NULL, Label = NULL)]
        
        testDToriginal <- copy(DToriginal[id %in% test_id_original])
        specificsDTtestorig <- copy(testDToriginal[, .(id, Label)])
        testDToriginal[, `:=`(id = NULL, Label = NULL)]
        
        
        OCSVM_scoresDT_original <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToriginal, DTtest = testDToriginal),
                                              Label = specificsDTtestorig$Label,
                                              id = specificsDTtestorig$id)
        OCSVM_scoresDT_original[, `scores-view1`:= Scores]
        list_id_view1[[i]] <- OCSVM_scoresDT_original[`scores-view1`> quantile(x = `scores-view1`, probs = given_probs1)][, id]
        
        # Outliers DT - View 2
        DToutliers <- DToutliers1[, .(`LOF-004`, `LOF-099`, `LOF-045`, `LOF-049`, `KNNW-001`, `KNNW-081`, `KNNW-031`, `KNN-001`, `FastABOD-007`, 
                                      `FastABOD-076`, `LDOF-006`, `LDOF-019`, `KDEOS-058`, `KDEOS-018`, `KDEOS-048`,
                                      `COF-005`, `COF-055`, `INFLO-044`, `INFLO-075`, `SimplifiedLOF-023`, `SimplifiedLOF-043`, `SimplifiedLOF-009`)]
        DToutliers[, `:=` (Label = DToriginal$Label, id = DToriginal$id)]
        
        # if(length(which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)) != 0){
        #   print(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17])
        #   stop(paste0("OD is infinite: ", names(DToutliers)[which(DToutliers[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:17] != 0)]))
        #   }
        
        # DToutliers[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:17]
        
        
        train_id_outliers <- sort(unique(c(predicted_normal[[2]], predicted_normal[[3]], list_id_view1[[i-1]])))
        test_id_outliers1 <- setdiff(DToutliers$id, train_id_outliers)
        test_id_outliers <- test_id_outliers1[!test_id_outliers1 %in% unlist(list_id_view2[1:i-1])]
        
        trainDToutliers <- copy(DToutliers[id %in% train_id_outliers])
        specificsDTtrain <- copy(trainDToutliers[, .(id, Label)])
        trainDToutliers[, `:=`(id = NULL, Label = NULL)]
        
        testDToutliers <- copy(DToutliers[id %in% test_id_outliers])
        specificsDTtest <- copy(testDToutliers[, .(id, Label)])
        testDToutliers[, `:=`(id = NULL, Label = NULL)]
        
        
        OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToutliers, DTtest = testDToutliers),
                                         Label = specificsDTtest$Label)
                                         
        OCSVM_scoresDT_out[, id:=specificsDTtest$id]
        OCSVM_scoresDT_out[, `scores-view2`:= Scores]
        list_id_view2[[i]] <- OCSVM_scoresDT_out[`scores-view2`> quantile(x = `scores-view2`, probs = given_probs1)][, id]
        
        
    }
  }
  
  print(paste0("Dimension of Test dataset ", dim(testDToutliers)[1]))
  original_list <- list()
  original_list[[1]] <- predicted_normal[[1]]
  original_list[2:(Iter+1)] <- list_id_view1
  
  outlier_list <- list()
  outlier_list[[1]] <- predicted_normal[[2]]
  outlier_list[2:(Iter+1)] <- list_id_view2
  
  
  return(list(original_list, outlier_list))
}

dataset <- "Waveform_withoutdupl_norm_v02"
DT <- fread(paste0("data/derived-data/", datasetname1 = dataset, ".csv"))
DT[, .N, by = Label]
0.3 * DT[, .N, by = Label][Label == "no", N]
res <- get_multiview_forloop(datasetname1 = dataset, input_normal_sample_size1 = 0.3 , given_probs1 = 0.99, Iter = 290)
view1 <- list()
for (i in 1:291) {
  view1[[i]] <- DT[id %in% res[[1]][[i]]][, Label]
}
table(unlist(view1))


view2 <- list()
for (i in 1:291) {
  view2[[i]] <- DT[id %in% res[[2]][[i]]][, Label]
}
table(unlist(view2))

#datasetname <- "Ionosphere_withoutdupl_norm"


# Unsupervised Representation Learning vs Original ------------------------

test_function <- function(datasetname, input_normal_sample_size, percentage_OD) {
  
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

# DONE
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   print(i)
#   print(Sys.time())
#   test_function(datasetname = "Waveform_withoutdupl_norm_v02", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }
# DONE in local job, delete.R

# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Waveform_withoutdupl_norm_v02", 
#                 input_normal_sample_size = i, percentage_OD = 0.6)
# }

# DONE
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Pima_withoutdupl_norm_02_v01", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }

# DONE in local job, delete.R
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Pima_withoutdupl_norm_02_v01", 
#                 input_normal_sample_size = i, percentage_OD = 0.6)
# }

# DONE in local job, delete.R
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Pima_withoutdupl_norm_05_v07", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }

# DONE in local job, delete.R
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Pima_withoutdupl_norm_05_v07", 
#                 input_normal_sample_size = i, percentage_OD = 0.6)
# }


# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Stamps_withoutdupl_norm_02_v06", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }


# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Stamps_withoutdupl_norm_02_v06", 
#                 input_normal_sample_size = i, percentage_OD = 0.6)
# }
# 
# 
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "Waveform_withoutdupl_norm_v01", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }
# 
# 
# for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
#   test_function(datasetname = "WDBC_withoutdupl_norm_v07", 
#                 input_normal_sample_size = i, percentage_OD = 0.4)
# }



# test_function(datasetname = "Pima_withoutdupl_norm_02_v09", 
#               input_normal_sample_size = 0.3, percentage_OD = 0.45)
# 


