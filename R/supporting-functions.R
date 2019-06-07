# Normalization functions -------------------------------------------------

# min-max
get_min_max_scaling <- function(column) {
  # min-max normalization - 0 -1
  res <- (column - min(column))/(max(column) - min(column))
  return(res)
}
# mean-sd
get_mean_sd_normalization <- function(column) {
  # Mean and SD normalization
  z <- column
  mean.z <- mean(z)
  sd.z <- sd(z)
  if (sd.z==0)
    return(z)
  (z - mean.z )/sd.z
}
# median-IQR
get_median_IQR_normalization <- function(column) {
  # Median and IQR normalization
  z <- column
  median.z <- median(z)
  iqr.z <- IQR(z)
  if (iqr.z==0)
    return(z)
  (z - median.z )/iqr.z
}

# median-MAD
get_median_MAD_normalization <- function(column) {
  # Median and MAD normalization
  z <- column
  median.z <- median(z)
  mad.z <- 1.4826*median(abs(z-median.z))
  if (mad.z==0)
    return(z)
  (z - median.z )/mad.z
}


# function for OCSVM_train_result script  -----------------------------------------------------------------------


calculate_AUC_LOF_iForest_Combined <- function(number_PCs) {
  iForest_DT <- fread(paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs", number_PCs, "_Allcombinations.csv"))
  LOF_DT <- fread(paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_", number_PCs,".csv"))
  LOF_DT[, Label:=NULL]
  LOF_DT[, Mean:=rowMeans(LOF_DT)]
  LOF_DT[, Label:=iForest_DT$Label]
  
  LOFiForest_numeric <- cbind(iForest_DT[, .SD, .SDcols = seq(from  = 1, to = 41, by = 2)], 
                              LOF_DT[, 1:12])
  ## We change signs because iForest produce Scores that are "the lower the more the outlierness" "
  LOFiForest_numeric <- LOFiForest_numeric[, lapply(.SD, function(x) -1 * (x)), 
                                           .SDcols = 1:dim(LOFiForest_numeric)[2]]
  LOFiForest_numeric_normalized <- LOFiForest_numeric[, lapply(.SD, function(x) get_mean_sd_normalization(x)), 
                                                      .SDcols = 1:dim(LOFiForest_numeric)[2]]
  LOFiForest_numeric_normalized[, Means:= rowMeans(LOFiForest_numeric_normalized)]
  LOFiForest_numeric_normalized[, Label:= iForest_DT$Label]
  
  
  AUCs_LOF_iForest_Comb <- data.table(Subspace = number_PCs,
                                      AUC_LOF = pROC::auc(LOF_DT$Label, LOF_DT$Mean),
                                      AUC_iForest = pROC::auc(iForest_DT$Label, iForest_DT$Mean),
                                      AUC_Combined = pROC::auc(LOFiForest_numeric_normalized$Label, LOFiForest_numeric_normalized$Mean))
  return(list(AUCs_LOF_iForest_Comb, 
              LOFiForest_numeric_normalized[, .(Means, Label)]))
}


# function for OCSVM by all subspaces ------------------------------------

get_DTs_byThreshold_by_Association <- function(threshold_input, argument_input) {
  
  start1 <- Sys.time()
  args1 <- argument_input
  associations_argument_list <- list("ORIGINAL", "SU_DC", "SU_DU", "SC_DC", "ALL_EDGES")
  associations_argument <- associations_argument_list[[args1]]
  NUs <- c(0.0001, 0.0005, 0.001, 0.005)
  GAMMAs <- c(0.01, 0.05, 0.09, 0.001)
  KERNELs <- c("rbf", "sigmoid")
  
  
  i <- 0
  NumberReportedOutliers_argument_numeric <- threshold_input
  system.time({Results_by_threshold_AEs <- create_labels_AEncodings_given_threshold(Number_of_reported_outliers = NumberReportedOutliers_argument_numeric)})
  NumberReportedOutliers1 <- as.character(NumberReportedOutliers_argument_numeric)
  DT <- as.data.table(Results_by_threshold_AEs[[args1]])
  Labels_DT1 <- as.data.table(Results_by_threshold_AEs[[6]])
  rm(Results_by_threshold_AEs)
  gc()
  list_AEs <- list()
  i <- i + 1 
  iter <- 0
  list_scores <- list()
  for (nu1 in NUs){
    for (gamma1 in GAMMAs){
      for(kernel1 in KERNELs){
        
        print(paste0("We started this experiment at: " ,start1))
        
        print(paste0("This iteration started at: ", Sys.time()))
        iter <- iter + 1
        print(paste0("Kernel: " , kernel1))
        print(paste0("Association Encoding: ", associations_argument))
        print(paste0("Iteration: ", iter))
        print(paste0("Reported Outliers : ", NumberReportedOutliers_argument_numeric))
        
        correct_data <- data.table::copy(DT)
        correct_data[, Predicted_Label_900PCs:=NULL]
        correct_data[, index:= 1:.N]
        Malicious_Data <- correct_data[Predicted_Label_allSubspaces == "Malicious"]
        malicious_index <- as.data.table(Malicious_Data[, index])
        Malicious_Data[, Predicted_Label_allSubspaces:=NULL]
        Malicious_Data[, index:= NULL]
        
        Normal_Data <- correct_data[Predicted_Label_allSubspaces == "Normal"]
        Normal_Data[, Predicted_Label_allSubspaces:=NULL]
        normal_index <- as.data.table(Normal_Data[, index])
        Normal_Data[, index:= NULL]
        print("Runnning OCSVM")
        print(Sys.time())
        outlier_scores <- as.data.table(OCSVM_train_test_900PCs(My_Nu = nu1, My_Gamma = gamma1, My_Kernel = kernel1, 
                                                                Association_string = associations_argument,
                                                                NumberReportedOutliers_string = NumberReportedOutliers1,
                                                                Normaldata_input = Normal_Data,
                                                                Maliciousdata_input = Malicious_Data))
        
        malicious_index[, OCSVM_sores:=outlier_scores$`0`]
        malicious_index[, AE:= associations_argument]
        malicious_index[, Procedure:= "Produced by All Subspaces"]
        malicious_index[, Reported_Outliers:= NumberReportedOutliers1]
        true_label <- Labels_DT1[index_1 %in% malicious_index$V1, V1]
        list_scores[[iter]] <- malicious_index[, Label:= true_label]
        gc()
        print("iteration OK! ")
        
      }
    }
    All_OCSVM_scores <- as.data.table(do.call(cbind, list_scores))
    gc()
  }
  final_OCSVMs_produced_by_AllSubspaces <- All_OCSVM_scores
  fwrite(final_OCSVMs_produced_by_AllSubspaces, 
         paste0("Latest_directory/OCSVM_results_scripts/Results_by_AllSubspaces/OCSVMs_produced_by_AllSubspaces_reportedOutliers_", 
                threshold_input, "_Association_", associations_argument, ".csv"))
}




# function for OCSVM produced by 900PCs -----------------------------------

get_DTs_byThreshold_by_Association_900PCs <- function(threshold_input, argument_input) {
  
  start1 <- Sys.time()
  args1 <- argument_input
  associations_argument_list <- list("ORIGINAL", "SU_DC", "SU_DU", "SC_DC", "ALL_EDGES")
  associations_argument <- associations_argument_list[[args1]]
  NUs <- c(0.0001, 0.0005, 0.001, 0.005)
  GAMMAs <- c(0.01, 0.05, 0.09, 0.001)
  KERNELs <- c("rbf", "sigmoid")
  
  
  i <- 0
  NumberReportedOutliers_argument_numeric <- threshold_input
  system.time({Results_by_threshold_AEs <- create_labels_AEncodings_given_threshold(Number_of_reported_outliers = NumberReportedOutliers_argument_numeric)})
  NumberReportedOutliers1 <- as.character(NumberReportedOutliers_argument_numeric)
  DT <- as.data.table(Results_by_threshold_AEs[[args1]])
  Labels_DT1 <- as.data.table(Results_by_threshold_AEs[[6]])
  rm(Results_by_threshold_AEs)
  gc()
  list_AEs <- list()
  i <- i + 1 
  iter <- 0
  list_scores <- list()
  for (nu1 in NUs){
    for (gamma1 in GAMMAs){
      for(kernel1 in KERNELs){
        
        print(paste0("We started this experiment at: " ,start1))
        
        print(paste0("This iteration started at: ", Sys.time()))
        iter <- iter + 1
        print(paste0("Kernel: " , kernel1))
        print(paste0("Association Encoding: ", associations_argument))
        print(paste0("Iteration: ", iter))
        print(paste0("Reported Outliers : ", NumberReportedOutliers_argument_numeric))
        
        correct_data <- data.table::copy(DT)
        correct_data[, Predicted_Label_allSubspaces:=NULL]
        correct_data[, index:= 1:.N]
        
        Malicious_Data <- correct_data[Predicted_Label_900PCs == "Malicious"]
        Malicious_Data[, Predicted_Label_900PCs:=NULL]
        malicious_index <- as.data.table(Malicious_Data[, index])
        Malicious_Data[, index:= NULL]
        
        Normal_Data <- correct_data[Predicted_Label_900PCs == "Normal"]
        Normal_Data[, Predicted_Label_900PCs:=NULL]
        normal_index <- as.data.table(Normal_Data[, index])
        Normal_Data[, index:= NULL]
        
        print("Runnning OCSVM")
        print(Sys.time())
        outlier_scores <- as.data.table(OCSVM_train_test_900PCs(My_Nu = nu1, My_Gamma = gamma1, My_Kernel = kernel1, 
                                                                Association_string = associations_argument,
                                                                NumberReportedOutliers_string = NumberReportedOutliers1,
                                                                Normaldata_input = Normal_Data,
                                                                Maliciousdata_input = Malicious_Data))
        
        malicious_index[, OCSVM_sores:=outlier_scores$`0`]
        malicious_index[, AE:= associations_argument]
        malicious_index[, Procedure:= "Produced by 900 PCs"]
        malicious_index[, Reported_Outliers:= NumberReportedOutliers1]
        true_label <- Labels_DT1[index_1 %in% malicious_index$V1, V1]
        list_scores[[iter]] <- malicious_index[, Label:= true_label]
        gc()
        print("iteration OK! ")
        
      }
    }
    All_OCSVM_scores <- as.data.table(do.call(cbind, list_scores))
    gc()
  }
  final_OCSVMs_produced_by_AllSubspaces <- All_OCSVM_scores
  fwrite(final_OCSVMs_produced_by_AllSubspaces, 
         paste0("Latest_directory/OCSVM_results_scripts/Results_by_900PCs/OCSVMs_produced_by_900PCs_reportedOutliers_", 
                threshold_input, "_Association_", associations_argument, ".csv"))
}





# test function -----------------------------------------------------------

get_DTs_byThreshold_by_Association_Averaging <- function(threshold_input, argument_input) {
  source("~/Videos/R-Classification-LosAlamos/Latest_directory/OCSVM_results_scripts/yest_OCSVM.R")
  start1 <- Sys.time()
  args1 <- argument_input
  associations_argument_list <- list("ORIGINAL", "SU_DC", "SU_DU", "SC_DC", "ALL_EDGES")
  associations_argument <- associations_argument_list[[args1]]
  NUs <- c(0.0001, 0.0005, 0.001, 0.005)
  GAMMAs <- c(0.01, 0.05, 0.09, 0.001)
  KERNELs <- c("rbf", "sigmoid")
  
  
  i <- 0
  NumberReportedOutliers_argument_numeric <- threshold_input
  system.time({Results_by_threshold_AEs <- create_labels_AEncodings_given_threshold_averaging(Number_of_reported_outliers = NumberReportedOutliers_argument_numeric)})
  NumberReportedOutliers1 <- as.character(NumberReportedOutliers_argument_numeric)
  DT <- as.data.table(Results_by_threshold_AEs[[args1]])
  Labels_DT1 <- as.data.table(Results_by_threshold_AEs[[6]])
  rm(Results_by_threshold_AEs)
  gc()
  list_AEs <- list()
  i <- i + 1 
  iter <- 0
  list_scores <- list()
  for (nu1 in NUs){
    for (gamma1 in GAMMAs){
      for(kernel1 in KERNELs){
        
        print(paste0("We started this experiment at: " ,start1))
        
        print(paste0("This iteration started at: ", Sys.time()))
        iter <- iter + 1
        print(paste0("Kernel: " , kernel1))
        print(paste0("Association Encoding: ", associations_argument))
        print(paste0("Iteration: ", iter))
        print(paste0("Reported Outliers : ", NumberReportedOutliers_argument_numeric))
        
        correct_data <- data.table::copy(DT)
        # correct_data[, Predicted_Label_900PCs:=NULL]
        correct_data[, index:= 1:.N]
        Malicious_Data <- correct_data[Predicted_Label_allSubspaces == "Malicious"]
        malicious_index <- as.data.table(Malicious_Data[, index])
        Malicious_Data[, Predicted_Label_allSubspaces:=NULL]
        Malicious_Data[, index:= NULL]
        dim(Malicious_Data)
        
        Normal_Data <- correct_data[Predicted_Label_allSubspaces == "Normal"]
        Normal_Data[, Predicted_Label_allSubspaces:=NULL]
        normal_index <- as.data.table(Normal_Data[, index])
        Normal_Data[, index:= NULL]
        print("Runnning OCSVM")
        print(Sys.time())
        outlier_scores <- as.data.table(OCSVM_train_test_900PCs(My_Nu = nu1, My_Gamma = gamma1, My_Kernel = kernel1, 
                                                                Association_string = associations_argument,
                                                                NumberReportedOutliers_string = NumberReportedOutliers1,
                                                                Normaldata_input = Normal_Data,
                                                                Maliciousdata_input = Malicious_Data))
        
        malicious_index[, OCSVM_sores:=outlier_scores$`0`]
        malicious_index[, AE:= associations_argument]
        malicious_index[, Procedure:= "Produced by All Subspaces"]
        malicious_index[, Reported_Outliers:= NumberReportedOutliers1]
        true_label <- Labels_DT1[index_1 %in% malicious_index$V1, V1]
        list_scores[[iter]] <- malicious_index[, Label:= true_label]
        gc()
        print("iteration OK! ")
        
      }
    }
    All_OCSVM_scores <- as.data.table(do.call(cbind, list_scores))
    gc()
  }
  final_OCSVMs_produced_by_AllSubspaces <- All_OCSVM_scores
  
  fwrite(final_OCSVMs_produced_by_AllSubspaces, 
         paste0("Latest_directory/OCSVM_results_scripts/Results_by_AllSubspaces/OCSVMs_produced_by_AllSubspaces_AVERAGING_reportedOutliers_", 
                threshold_input, "_Association_", associations_argument, ".csv"))
}


