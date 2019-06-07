.libPaths(new = "/home/giorgosk/Videos/R/x86_64-pc-linux-gnu-library/3.4/")
Sys.setenv(R_LIBS_USER = "/home/giorgosk/Videos/R/x86_64-pc-linux-gnu-library/3.4/")
library(doParallel)
library(data.table)
library(foreach)
library(ggplot2)
library(reticulate)
options(scipen = 9999)
setwd("/home/giorgosk/Videos/R-Classification-LosAlamos")
reticulate::source_python("~/Videos/R-Classification-LosAlamos/Latest_directory/sklearn_detectors.py")
source("Latest_directory/supporting-functions.R")



# function for OCSVM_train_result script
create_labels_AEncodings_given_threshold  <- function(Number_of_reported_outliers) {
  
  combo_LOF_iForest20PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 20)[[2]]
  combo_LOF_iForest20PCs[, index:= 1:.N]
  mal_index20 <-  combo_LOF_iForest20PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest20PCs[, Predicted_label_20:= 0L]
  combo_LOF_iForest20PCs[index %in% mal_index20, Predicted_label_20:= 1L]
  combo_LOF_iForest20PCs[Predicted_label_20 == 1L, .N, by = Label]
  
  # PCs 50
  combo_LOF_iForest50PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 50)[[2]]
  combo_LOF_iForest50PCs[, index:= 1:.N]
  mal_index50 <-  combo_LOF_iForest50PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest50PCs[, Predicted_label_50:= 0L]
  combo_LOF_iForest50PCs[index %in% mal_index50, Predicted_label_50:= 1L]
  # PCs 100
  combo_LOF_iForest100PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 100)[[2]]
  combo_LOF_iForest100PCs[, index:= 1:.N]
  mal_index100 <-  combo_LOF_iForest100PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest100PCs[, Predicted_label_100:= 0L]
  combo_LOF_iForest100PCs[index %in% mal_index100, Predicted_label_100:= 1L]
  # PCs 150
  combo_LOF_iForest150PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 150)[[2]]
  combo_LOF_iForest150PCs[, index:= 1:.N]
  mal_index150 <-  combo_LOF_iForest150PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest150PCs[, Predicted_label_150:= 0L]
  combo_LOF_iForest150PCs[index %in% mal_index150, Predicted_label_150:= 1L]
  # PCs 200
  combo_LOF_iForest200PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 200)[[2]]
  combo_LOF_iForest200PCs[, index:= 1:.N]
  mal_index200 <-  combo_LOF_iForest200PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest200PCs[, Predicted_label_200:= 0L]
  combo_LOF_iForest200PCs[index %in% mal_index200, Predicted_label_200:= 1L]
  # PCs 500
  combo_LOF_iForest500PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 500)[[2]]
  combo_LOF_iForest500PCs[, index:= 1:.N]
  mal_index500 <-  combo_LOF_iForest500PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest500PCs[, Predicted_label_500:= 0L]
  combo_LOF_iForest500PCs[index %in% mal_index500, Predicted_label_500:= 1L]
  # PCs 900
  combo_LOF_iForest900PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 900)[[2]]
  combo_LOF_iForest900PCs[, index:= 1:.N]
  mal_index900 <-  combo_LOF_iForest900PCs[order(Means, decreasing = T)][1:Number_of_reported_outliers, index]
  combo_LOF_iForest900PCs[, Predicted_label_900:= 0L]
  combo_LOF_iForest900PCs[index %in% mal_index900, Predicted_label_900:= 1L]
  
  predicted_labels_allSubspaces <- as.data.table(do.call(cbind, list(combo_LOF_iForest20PCs[, Predicted_label_20], 
                                                                     combo_LOF_iForest50PCs[, Predicted_label_50],
                                                                     combo_LOF_iForest100PCs[, Predicted_label_100], 
                                                                     combo_LOF_iForest150PCs[, Predicted_label_150],
                                                                     combo_LOF_iForest200PCs[, Predicted_label_200],
                                                                     combo_LOF_iForest500PCs[, Predicted_label_500])))
  # Create predicted labels based on the collective ensembles --------------
  predicted_labels_allSubspaces[, SUMS:= rowSums(predicted_labels_allSubspaces)]
  predicted_labels_allSubspaces[, Final_predicted_label:= "Normal"]
  predicted_labels_allSubspaces[SUMS>=4, Final_predicted_label:= "Malicious"]
  predicted_labels_allSubspaces[, Label:= combo_LOF_iForest900PCs$Label]
  predicted_labels_allSubspaces[Final_predicted_label == "Malicious", .N, by = Label]
  predicted_labels_allSubspaces[, index:= 1:.N]
  
  predicted_Malicious_all_subspaces_DT <- copy(predicted_labels_allSubspaces)
  fwrite(predicted_Malicious_all_subspaces_DT, paste0("Latest_directory/predicted_Malicious_all_subspaces_DT", Number_of_reported_outliers, ".csv"))
  
  # Write predicted labels based on the 900 PCs ensembles --------------
  fwrite(combo_LOF_iForest900PCs, paste0("Latest_directory/predicted_Malicious_900PCs_subspace_DT", Number_of_reported_outliers, ".csv"))
  
  Malicious_index_all_subspaces <- predicted_labels_allSubspaces[Final_predicted_label == "Malicious", index]
  Normal_index_all_subspaces <- predicted_labels_allSubspaces[Final_predicted_label == "Normal", index]
  
  Malicious_index_900PCs <- combo_LOF_iForest900PCs[Predicted_label_900 == 1, index]
  Normal_index_900PCs <- combo_LOF_iForest900PCs[Predicted_label_900 == 0, index]
  
  print("Start of original")
  ################### One hot encoded Original Features ###################
  Encoded_original <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/OnehotEncoded_150Krows.csv", nThread = 5)
  Encoded_original[, index:= 1:.N]
  ## One hot enocoded - Labels by all Subspaces
  Encoded_original[, Predicted_Label_allSubspaces:= "Normal"]
  Encoded_original[index %in% Malicious_index_all_subspaces, Predicted_Label_allSubspaces:= "Malicious"]
  ## One hot enocoded - Labels by 900 PCs
  Encoded_original[, Predicted_Label_900PCs:= "Normal"]
  Encoded_original[index %in% Malicious_index_900PCs, Predicted_Label_900PCs:= "Malicious"]
  # fwrite(Encoded_original, 
  #        paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/OCSVM_results_scripts/OnehotEncoded_Original_FinalVersion_Labels", 
  #               Number_of_reported_outliers,"_Outliers.csv"), nThread = 5)
  
  ################### One hot encoded Source Computer --- > Destination Computer ###################
  print("Start of SC DC")
  Encoded_SC_DC <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/OnehotEncoded_150Krows_ComputerComputer.csv", nThread = 5)
  Encoded_SC_DC[, index:= 1:.N]
  ## One hot enocoded - Labels by all Subspaces
  Encoded_SC_DC[, Predicted_Label_allSubspaces:= "Normal"]
  Encoded_SC_DC[index %in% Malicious_index_all_subspaces, Predicted_Label_allSubspaces:= "Malicious"]
  ## One hot enocoded - Labels by 900 PCs
  Encoded_SC_DC[, Predicted_Label_900PCs:= "Normal"]
  Encoded_SC_DC[index %in% Malicious_index_900PCs, Predicted_Label_900PCs:= "Malicious"]
  # fwrite(Encoded_SC_DC, 
  #        paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/OCSVM_results_scripts/OnehotEncoded_ComputerComputer_FinalVersion_Labels", 
  #               Number_of_reported_outliers,"_Outliers.csv"), nThread = 5)
  
  ################### One hot encoded Source User --- > Destination User ###################
  print("Start of SU DU")
  Encoded_SU_DU <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/OnehotEncoded_150Krows_UserUser.csv", nThread = 5)
  Encoded_SU_DU[, index:= 1:.N]
  ## One hot enocoded - Labels by all Subspaces
  Encoded_SU_DU[ , Predicted_Label_allSubspaces:= "Normal"]
  Encoded_SU_DU[index %in% Malicious_index_all_subspaces, Predicted_Label_allSubspaces:= "Malicious"]
  ## One hot enocoded - Labels by 900 PCs
  Encoded_SU_DU[, Predicted_Label_900PCs:= "Normal"]
  Encoded_SU_DU[index %in% Malicious_index_900PCs, Predicted_Label_900PCs:= "Malicious"]
  # fwrite(Encoded_SU_DU, 
  #        paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/OCSVM_results_scripts/OnehotEncoded_UserUser_FinalVersion_Labels", 
  #               Number_of_reported_outliers,"_Outliers.csv"), nThread = 5)
  
  ###################  One hot encoded Source_User ---> Destination Computer   ################### 
  print("Start of SU DC")
  Encoded_SU_DC <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/OnehotEncoded_150Krows_SourceComputer.csv")
  Encoded_SU_DC[, index:= 1:.N]
  ## One hot enocoded - Labels by all Subspaces
  Encoded_SU_DC[, Predicted_Label_allSubspaces:= "Normal"]
  Encoded_SU_DC[index %in% Malicious_index_all_subspaces, Predicted_Label_allSubspaces:= "Malicious"]
  ## One hot enocoded - Labels by 900 PCs
  Encoded_SU_DC[, Predicted_Label_900PCs:= "Normal"]
  Encoded_SU_DC[index %in% Malicious_index_900PCs, Predicted_Label_900PCs:= "Malicious"]
  # fwrite(Encoded_SU_DC, 
  #        paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/OCSVM_results_scripts/OnehotEncoded_SourceComputer_FinalVersion_Labels", 
  #               Number_of_reported_outliers,"_Outliers.csv"), nThread = 5)
  
  ###################  One hot encoded ALL EDGES   ###################  
  print("Start of All edges")
  Encoded_all_edges <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/OnehotEncoded_150Krows_ALL_EDGES.csv")
  Encoded_all_edges[, index:= 1:.N]
  ## One hot enocoded - Labels by all Subspaces
  Encoded_all_edges[, Predicted_Label_allSubspaces:= "Normal"]
  Encoded_all_edges[index %in% Malicious_index_all_subspaces, Predicted_Label_allSubspaces:= "Malicious"]
  ## One hot enocoded - Labels by 900 PCs
  Encoded_all_edges[, Predicted_Label_900PCs:= "Normal"]
  Encoded_all_edges[index %in% Malicious_index_900PCs, Predicted_Label_900PCs:= "Malicious"]
  
  Labels_DT <- as.data.table(combo_LOF_iForest20PCs$Label)
  Labels_DT[, index_1:= 1:.N]
  
  return(list(Encoded_original, Encoded_SU_DC, Encoded_SU_DU, Encoded_SC_DC, Encoded_all_edges, Labels_DT))
}




# the same function but we use the Averaging function instead of S --------





args <- commandArgs(TRUE)
args_1 <- as.numeric(args[1])
args_2 <- as.numeric(args[2])

#cl <- parallel::makeCluster(5)
#doParallel::registerDoParallel(cl)
#print(Sys.time())
#foreach(Iter = 1:5, .packages = c('data.table', 'reticulate', 'foreach'))  %dopar% {
get_DTs_byThreshold_by_Association(args_1, args_2)
# get_DTs_byThreshold_by_Association(500, 1)
print(Sys.time())
#    }
#parallel::stopCluster(cl)
# args1 <- 1

# associations_argument_list <- list("ORIGINAL", "SU_DC", "SU_DU", "SC_DC", "ALL_EDGES")
# associations_argument <- associations_argument_list[[args1]]
# NUs <- c(0.0001)#, 0.0005, 0.001, 0.005)
# GAMMAs <- c(0.01)#, 0.05, 0.09, 0.001)
# KERNELs <- c("rbf")#, "sigmoid")
# i <- 0
# for(IJ in c(500, 1500, 2000, 3000, 5000, 7000, 10000)){
#   Sys.time()
#   NumberReportedOutliers_argument_numeric <- IJ
#   system.time({Results_by_threshold_AEs <- create_labels_AEncodings_given_threshold(Number_of_reported_outliers = NumberReportedOutliers_argument_numeric)})
#   NumberReportedOutliers1 <- as.character(NumberReportedOutliers_argument_numeric)
#   list_AEs <- list()
#   i <- i + 1 
#   iter <- 0
#   list_scores <- list()
#   for (nu1 in NUs){
#     for (gamma1 in GAMMAs){
#       for(kernel1 in KERNELs){
#         print(Sys.time())
#         iter <- iter + 1
#         print(paste0("Association Encoding: ", associations_argument))
#         print(paste0("Iteration: ", iter))
#         print(paste0("Reported Outliers : ", IJ))
#         correct_data1 <- as.data.table(Results_by_threshold_AEs[[args1]])
#         correct_data <- data.table::copy(correct_data1)
#         correct_data[, Predicted_Label_900PCs:=NULL]
#         correct_data[, index:= 1:.N]
#         Malicious_Data <- correct_data[Predicted_Label_allSubspaces == "Malicious"]
#         malicious_index <- as.data.table(Malicious_Data[, index])
#         Malicious_Data[, Predicted_Label_allSubspaces:=NULL]
#         Malicious_Data[, index:= NULL]
#         
#         Normal_Data <- correct_data[Predicted_Label_allSubspaces == "Normal"]
#         Normal_Data[, Predicted_Label_allSubspaces:=NULL]
#         normal_index <- as.data.table(Normal_Data[, index])
#         Normal_Data[, index:= NULL]
#         
#         outlier_scores <- as.data.table(OCSVM_train_test_900PCs(My_Nu = nu1, My_Gamma = gamma1, My_Kernel = kernel1, 
#                                                                 Association_string = associations_argument,
#                                                                 NumberReportedOutliers_string = NumberReportedOutliers1,
#                                                                 Normaldata_input = Normal_Data,
#                                                                 Maliciousdata_input = Malicious_Data))
#         
#         malicious_index[, OCSVM_sores:=outlier_scores$`0`]
#         malicious_index[, AE:= associations_argument]
#         malicious_index[, Procedure:= "Produced by All Subspaces"]
#         malicious_index[, Reported_Outliers:= NumberReportedOutliers1]
#         Labels_DT <- Results_by_threshold_AEs[[6]]
#         true_label <- Labels_DT[index_1 %in% malicious_index$V1, V1]
#         list_scores[[iter]] <- malicious_index[, Label:= true_label]
#         
#         
#       }
#     }
#     list_AEs[[i]] <- as.data.table(do.call(cbind, list_scores))
#     gc()
#   }
#   final_OCSVMs_produced_by_AllSubspaces <- rbindlist(list_AEs)
#   fwrite(final_OCSVMs_produced_by_AllSubspaces, 
#          paste0("Latest_directory/OCSVM_results_scripts/OCSVMs_produced_by_AllSubspaces_reportedOutliers_Association_", associations_argument, ".csv"))
# }
# 










  
















