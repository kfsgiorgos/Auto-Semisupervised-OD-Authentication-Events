# source & libraries ------------------------------------------------------

library(doParallel)
library(data.table)
library(foreach)
library(ggplot2)
library(lubridate)
library(logisticPCA)
library(rARPACK)
library(mltools)
library(FNN)
library(pROC)
library(esquisse)
options(scipen = 9999)
source("Latest_directory/supporting-functions.R")


# 1) LOF ---------------------------------------------------------------------

get_LOFresults_sklearn <- function(NumberofPCs, given_k){
  library(data.table)
  library(reticulate)
  reticulate::source_python("~/Videos/R-Classification-LosAlamos/Latest_directory/sklearn_detectors.py")
  
  LabelsAllUsers <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/LabelsAllUsers_LosAlamos.csv", nThread = 20)
  LogisticPCs_1 <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/PCs900_LosAlamos.csv", nThread = 20)
  givenDT_1 <- copy(LogisticPCs_1[, 1:NumberofPCs])
  givenDT_1[, nRow:= 4500000:4650000]
  givenDT_1[, Label:= LabelsAllUsers[nRowIndex %in% c(4500000:4650000), Label]]
  
  SampleLogisticPCA <- copy(givenDT_1)
  dim(SampleLogisticPCA)
  LABELS <- SampleLogisticPCA[, Label]
  INDEX <- SampleLogisticPCA[, nRow]
  SampleLogisticPCA[, c("nRow", "Label"):=NULL]
  
  print(paste0("Start of iteration:   ", i))
  print(Sys.time())
  LOF_k3 <- calculate_LOF(given_DT = SampleLogisticPCA, given_neighbors = as.integer(given_k))
  print(Sys.time())
  LOF_1 <- data.table(V1 = LOF_k3)
  LOF_1[, Label:= LABELS]
  print(LOF_1[V1>median(V1), .N, by=Label])
  LOF_1[, indexs:= INDEX]
  gc()
  fwrite(LOF_1, paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/Lof_sklearn_Scores_Neighbors_", given_k, "_",dim(SampleLogisticPCA)[2], "PCs.csv"))
}

for(j in c(20, 50, 100, 150, 200, 500, 900)){
  for(i in c(5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100)){
    Sys.time()
    system.time({get_LOFresults_sklearn(NumberofPCs = j, given_k = i)})
  } 
}


# 1.1) Read LOF results  -------------------------------------------------------

#for(j in c(20, 50, 100, 150, 200, 500, 900)){

get_LOF_DT <- function(number_PCs) {
  NumberPCs <- number_PCs
  list_LOF <- list()
  k <- 0
  for(i in c(5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100)){
    k <- k + 1 
    temp <- fread(paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/Lof_sklearn_Scores_Neighbors_", i, "_", NumberPCs, "PCs.csv"))
    temp1 <- temp$Label
    temp[, `:=` (Label = NULL, indexs = NULL)]
    list_LOF[[k]] <- temp
  } 
  DT_final <- as.data.table(do.call(cbind, list_LOF))
  DT_final[, Label:= temp1]
  return(DT_final)
}


# 20 PCs ------------------------------------------------------------------
DT_20 <-get_LOF_DT(20)
fwrite(DT_20, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_20.csv")
# 50 PCs ------------------------------------------------------------------
DT_50 <-get_LOF_DT(50)
fwrite(DT_50, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_50.csv")
# 100 PCs ------------------------------------------------------------------
DT_100 <-get_LOF_DT(100)
fwrite(DT_100, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_100.csv")
# 150 PCs ------------------------------------------------------------------
DT_150 <-get_LOF_DT(150)
fwrite(DT_150, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_150.csv")
# 200 PCs ------------------------------------------------------------------
DT_200 <-get_LOF_DT(200)
fwrite(DT_200, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_200.csv")
# 500 PCs ------------------------------------------------------------------
DT_500 <-get_LOF_DT(500)
fwrite(DT_500, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_500.csv")
# 900 PCs ------------------------------------------------------------------
DT_900 <-get_LOF_DT(900)
fwrite(DT_900, "/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_900.csv")
# 2) iForest ------------------------------------------------------

TrainIF_20 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_20/TrainData/")
TestIF_20 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_20/TestData/")
length(TrainIF_20) == length(TestIF_20) 
length(TrainIF_20)
###  50PCs ####
TrainIF_50 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_50/TrainData/")
TestIF_50 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_50/TestData/")
length(TrainIF_50) == length(TestIF_50) 
length(TrainIF_50)
length(TestIF_50)
### 100PCs ####
TrainIF_100 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_100/TrainData/")
TestIF_100 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_100/TestData/")
length(TrainIF_100) == length(TestIF_100) 
length(TrainIF_100)
### 150PCs ####
TrainIF_150 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_150/TrainData/")
TestIF_150 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_150/TestData/")
length(TrainIF_150) == length(TestIF_150) 
length(TrainIF_150)
###  200PCs ####
TrainIF_200 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_200/TrainData/")
TestIF_200 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_200/TestData/")
length(TrainIF_200) == length(TestIF_200) 
length(TrainIF_200)
###  500PCs ####
TrainIF_500 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_500/TrainData/")
TestIF_500 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_500/TestData/")
length(TrainIF_500) == length(TestIF_500) 
length(TrainIF_500)
### 900PCs ####
TrainIF_900 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_900/TrainData/")
TestIF_900 <- list.files(path = "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/PCs_900/TestData/")
length(TrainIF_900) == length(TestIF_900) 
length(TrainIF_900)

getIFlist <- function(NumberofPCs, TrainVecFiles, TestVecFiles){
  LabelsAllUsers <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/LabelsAllUsers_LosAlamos.csv", nThread = 20)
  LabelsVec <- LabelsAllUsers[nRowIndex %in% c(4500000:4650000), Label]
  My_path <- "~/Videos/R-Classification-LosAlamos/Generalized_Outlier_Ensemble_Methodology/IF_LANL_New/"
  List_IF <- list()
  List_IF_tocombine <- list()
  j <- 0
  for(i in 1:length(TrainVecFiles)){
    j <- j + 1 
    print(paste0("Iteration: ", j))
    tempTrain <- fread(paste0(My_path, "PCs_", NumberofPCs, "/TrainData/", TrainVecFiles[i]))
    tempTrain1 <- tempTrain[-1]
    tempTest <- fread(paste0(My_path, "PCs_", NumberofPCs, "/TestData/", TestVecFiles[i]))
    tempTest1 <- tempTest[-1]
    DT <- rbindlist(list(tempTrain1, tempTest1))
    DT[, V1:=NULL]
    List_IF_tocombine[[j]] <- copy(DT)
    DT[V2 < mean(V2), PredLabel:= 1L]
    DT[V2 >= mean(V2), PredLabel:= 0L]
    setnames(DT, c("V2", "PredLabel"), c(paste0("V2_", i), paste0("PredLabel_", i)))
    List_IF[[j]] <- DT
  }
  
  DT_PCs_IF <- as.data.table(do.call(cbind, List_IF))
  DT_PCs_IF[, Label:= LabelsVec]
  SUMS <- rowSums(DT_PCs_IF[, .SD, .SDcols = paste0("PredLabel_", 1:length(TrainVecFiles))])
  DT_PCs_IF[, sumsLabel:= SUMS]
  ## Combine with Average & Maximum 
  DT_PCs_IF_tocombine <- as.data.table(do.call(cbind, List_IF_tocombine))
  DT_PCs_IF_transpose <- data.table::transpose(DT_PCs_IF_tocombine)
  Maximums <- DT_PCs_IF_transpose[, lapply(.SD, function(x) max(x)), .SDcols = 1:dim(DT_PCs_IF_transpose)[2]]
  Maximums1 <- data.table::transpose(Maximums)
  DT_PCs_IF[, `:=` (Maximum  = Maximums1$V1, 
                    Mean = rowMeans(DT_PCs_IF_tocombine))]
  gc()
  return(DT_PCs_IF)
}

SelectedNumberFiles <- length(TrainIF_900)
IndexSample20 <- sample(length(TrainIF_20), SelectedNumberFiles)
IndexSample50 <- sample(length(TrainIF_50), SelectedNumberFiles)
IndexSample100 <- sample(length(TrainIF_100), SelectedNumberFiles)
IndexSample150 <- sample(length(TrainIF_150), SelectedNumberFiles)
IndexSample200 <- sample(length(TrainIF_200), SelectedNumberFiles)
IndexSample500 <- sample(length(TrainIF_500), SelectedNumberFiles)

SampleTrain20 <- TrainIF_20[IndexSample20]
SampleTest20 <- TestIF_20[IndexSample20]

SampleTrain50 <- TrainIF_50[IndexSample50]
SampleTest50 <- TestIF_50[IndexSample50]

SampleTrain100 <- TrainIF_100[IndexSample100]
SampleTest100 <- TestIF_100[IndexSample100]

SampleTrain150 <- TrainIF_150[IndexSample150]
SampleTest150 <- TestIF_150[IndexSample150]

SampleTrain200 <- TrainIF_200[IndexSample200]
SampleTest200 <- TestIF_200[IndexSample200]

SampleTrain500 <- TrainIF_500[IndexSample500]
SampleTest500 <- TestIF_500[IndexSample500]

#### Read iForest Results  #####
DT_20PCs <- getIFlist(NumberofPCs = 20, TrainVecFiles = SampleTrain20, TestVecFiles = SampleTest20)
DT_50PCs <- getIFlist(NumberofPCs = 50, TrainVecFiles = SampleTrain50, TestVecFiles = SampleTest50)
DT_100PCs <- getIFlist(NumberofPCs = 100, TrainVecFiles = SampleTrain100, TestVecFiles = SampleTest100)
DT_150PCs <- getIFlist(NumberofPCs = 150, TrainVecFiles = SampleTrain150, TestVecFiles = SampleTest150)
DT_200PCs <- getIFlist(NumberofPCs = 200, TrainVecFiles = SampleTrain200, TestVecFiles = SampleTest200)
DT_500PCs <- getIFlist(NumberofPCs = 500, TrainVecFiles = SampleTrain500, TestVecFiles = SampleTest500)
DT_900PCs <- getIFlist(NumberofPCs = 900, TrainVecFiles = TrainIF_900, TestVecFiles = TestIF_900)

fwrite(DT_20PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs20_Allcombinations.csv")
fwrite(DT_50PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs50_Allcombinations.csv")
fwrite(DT_100PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs100_Allcombinations.csv")
fwrite(DT_150PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs150_Allcombinations.csv")
fwrite(DT_200PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs200_Allcombinations.csv")
fwrite(DT_500PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs500_Allcombinations.csv")
fwrite(DT_900PCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/PCs900_Allcombinations.csv")


# 3) Read LOF & iForest ------------------------

iForest_20PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs20_Allcombinations.csv")
iForest_50PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs50_Allcombinations.csv")
iForest_100PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs100_Allcombinations.csv")
iForest_150PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs150_Allcombinations.csv")
iForest_200PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs200_Allcombinations.csv")
iForest_500PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs500_Allcombinations.csv")
iForest_900PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs900_Allcombinations.csv")

# 3.1) function to combine LOF & iForest -----------------------------------------------------------------------

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


# 3.2) create & write  DT with all AUCs  ---------------------------------------
all_subspaces_AUCs <- rbindlist(list(calculate_AUC_LOF_iForest_Combined(number_PCs = 20)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 50)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 100)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 150)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 200)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 500)[[1]],
                                     calculate_AUC_LOF_iForest_Combined(number_PCs = 900)[[1]]))
fwrite(all_subspaces_AUCs, "~/Videos/R-Classification-LosAlamos/Latest_directory/all_subspaces_AUCs.csv")

# 3.3) read DT with all AUCs  ---------------------------------------
all_subspaces_AUCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/all_subspaces_AUCs.csv")

# 3.4) function for Precision & Recall the combined Labels -----------------------------------------

calculate_Recall_Precision_LOF_iForest_combined <- function(number_PCs, first_X_rows){
  temp <- calculate_AUC_LOF_iForest_Combined(number_PCs = number_PCs)[[2]]
  res1 <- temp[order(Means, decreasing = T)][1:first_X_rows, .N, by = Label]
  if(dim(res1[Label == "Malicious"])[1] == 0){
    # mal_DT <- data.table(Label = "malicious", N = 0)
    # res1 <- rbindlist(list(res1, mal_DT))
    Presicion <- 0
    Recall <- 0
    return(list(res_precision = Presicion, 
                res_recall = Recall))
  } else{
    Presicion <- res1[Label == "Malicious", N]/first_X_rows
    Recall <- res1[Label == "Malicious", N]/(res1[Label == "Malicious", N] + (10 - res1[Label == "Malicious", N]) )
    
    return(list(res_precision = Presicion, 
                res_recall = Recall))
  }}


get_Recall_Precision_LOF_iForest_combined <- function(number_PCs){
  Precision_list <- list()
  Recall_list <- list()
  k <- 0
  for(i in seq(from = 100, to = 150000, by = 500)){
    k <- k +1 
    Precision_Recall <- calculate_Recall_Precision_LOF_iForest_combined(number_PCs = number_PCs, first_X_rows = i)
    Precision_list[[k]] <- Precision_Recall[["res_precision"]]
    Recall_list[[k]] <- Precision_Recall[["res_recall"]]
  } 
  
  Presion_recall_DT  <- data.table(Subspace = number_PCs,
                                   Precision = unlist(Precision_list),
                                   Recall = unlist(Recall_list),
                                   lower_bound_scores = seq(from = 100, to = 150000, by = 500))
  
  fwrite(Presion_recall_DT, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT", number_PCs, "_PCs.csv"))
  
}

get_Recall_Precision_LOF_iForest_combined(number_PCs = 20)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 50)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 100)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 150)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 200)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 500)
get_Recall_Precision_LOF_iForest_combined(number_PCs = 900)

# 3.5) Read - Recall Precision Combined results  ----------------------------------------

recall_precision_20 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT20_PCs.csv")
recall_precision_50 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT50_PCs.csv")
recall_precision_100 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT100_PCs.csv")
recall_precision_150 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT150_PCs.csv")
recall_precision_200 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT200_PCs.csv")
recall_precision_500 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT500_PCs.csv")
recall_precision_900 <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/Presion_recall_DT900_PCs.csv")

recall_precision <- rbindlist(list(recall_precision_20, recall_precision_50, recall_precision_100,
                                   recall_precision_150, recall_precision_200, recall_precision_500,
                                   recall_precision_900))
fwrite(recall_precision, "~/Videos/R-Classification-LosAlamos/Latest_directory/PresionRecall_DT_all_PCs.csv")

recall_precision[, Subspace:= as.factor(Subspace)]
All_recall_DT <- copy(recall_precision[, .(Subspace, Recall, lower_bound_scores)])
All_precision_DT <- copy(recall_precision[, .(Subspace, Precision, lower_bound_scores)])


# 4) Precision and Recall for LOF - each subspace ----------------------------
calculate_Recall_Precision_LOF <- function(number_PCs1, first_X_rows){
  
  LOF_DT <- fread(paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_", number_PCs1,".csv"))
  LABELS <- copy(LOF_DT[, Label])
  LOF_DT[, Label:=NULL]
  LOF_DT[, Mean:=rowMeans(LOF_DT)]
  LOF_DT[, Label:=LABELS]
  LOF_numeric <- LOF_DT[, 1:12]
  ## We change signs because iForest produce Scores that are "the lower the more the outlierness" "
  
  LOF_normalized <- LOF_numeric[, lapply(.SD, function(x) get_mean_sd_normalization(x)), 
                                .SDcols = 1:dim(LOF_numeric)[2]]
  LOF_normalized[, Means:= rowMeans(LOF_normalized)]
  LOF_normalized[, Label:= LABELS]
  
  res1 <- LOF_normalized[order(Means, decreasing = T)][1:first_X_rows, .N, by = Label]
  if(dim(res1[Label == "Malicious"])[1] == 0){
    # mal_DT <- data.table(Label = "malicious", N = 0)
    # res1 <- rbindlist(list(res1, mal_DT))
    Presicion <- 0
    Recall <- 0
    return(list(res_precision = Presicion, 
                res_recall = Recall))
  } else{
    Presicion <- res1[Label == "Malicious", N]/first_X_rows
    Recall <- res1[Label == "Malicious", N]/(res1[Label == "Malicious", N] + (10 - res1[Label == "Malicious", N]) )
    
    return(list(res_precision = Presicion, 
                res_recall = Recall))
  
  }
}


get_Recall_Precision_LOF <- function(number_PCs1){
  Precision_list <- list()
  Recall_list <- list()
  k <- 0
  for(i in seq(from = 100, to = 150000, by = 500)){
    k <- k +1 
    Precision_Recall <- calculate_Recall_Precision_LOF(number_PCs = number_PCs1, first_X_rows = i)
    Precision_list[[k]] <- Precision_Recall[["res_precision"]]
    Recall_list[[k]] <- Precision_Recall[["res_recall"]]
  } 
  
  Presion_recall_DT  <- data.table(Subspace = number_PCs1,
                                   Precision = unlist(Precision_list),
                                   Recall = unlist(Recall_list),
                                   lower_bound_scores = seq(from = 100, to = 150000, by = 500))
  return(Presion_recall_DT)
  fwrite(Presion_recall_DT, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", number_PCs1, "_PCs.csv"))
  
}


# 4.1) Get -LOF- Precision & Recall for each subspace -------------------

temp1 <- get_Recall_Precision_LOF(number_PCs1 = 20)
fwrite(temp1, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 20, "_PCs.csv"))
temp2 <- get_Recall_Precision_LOF(number_PCs1 = 50)
fwrite(temp2, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 50, "_PCs.csv"))
temp3 <- get_Recall_Precision_LOF(number_PCs1 = 100)
fwrite(temp3, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 100, "_PCs.csv"))
temp4 <- get_Recall_Precision_LOF(number_PCs1 = 150)
fwrite(temp4, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 150, "_PCs.csv"))
temp5 <- get_Recall_Precision_LOF(number_PCs1 = 200)
fwrite(temp5, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 200, "_PCs.csv"))
temp6 <- get_Recall_Precision_LOF(number_PCs1 = 500)
fwrite(temp6, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 500, "_PCs.csv"))
temp7 <- get_Recall_Precision_LOF(number_PCs1 = 900)
fwrite(temp7, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT", 900, "_PCs.csv"))

# 4.2) Read -LOF- Precision & Recall for each subspace -------------------
recall_precision_20_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT20_PCs.csv")
recall_precision_50_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT50_PCs.csv")
recall_precision_100_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT100_PCs.csv")
recall_precision_150_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT150_PCs.csv")
recall_precision_200_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT200_PCs.csv")
recall_precision_500_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT500_PCs.csv")
recall_precision_900_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_Presion_recall_DT900_PCs.csv")

recall_precision_LOF <- rbindlist(list(recall_precision_20_LOF, recall_precision_50_LOF, recall_precision_100_LOF,
                                   recall_precision_150_LOF, recall_precision_200_LOF, recall_precision_500_LOF,
                                   recall_precision_900_LOF))

fwrite(recall_precision_LOF, "~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_PresionRecall_DT_all_PCs.csv")


# end of LOF section  -----------------------------------------------------

# 5) iForest ---------------------------------------------------------------
calculate_Recall_Precision_iForest <- function(number_PCs, first_X_rows){


  LOF_DT <- fread(paste0("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/PCs", number_PCs,"_Allcombinations.csv"))
  LABELS <- copy(LOF_DT[, Label])
  LOF_DT[, Label:=NULL]
  LOF_DT[, Mean:=rowMeans(LOF_DT)]
  LOF_DT[, Label:=LABELS]
  LOF_numeric <- LOF_DT[, 1:12]
  ## We change signs because iForest produce Scores that are "the lower the more the outlierness" "
  
  LOF_normalized <- LOF_numeric[, lapply(.SD, function(x) get_mean_sd_normalization(x)), 
                                .SDcols = 1:dim(LOF_numeric)[2]]
  LOF_normalized[, Means:= rowMeans(LOF_normalized)]
  LOF_normalized[, Label:= LABELS]
  
  res1 <- LOF_normalized[order(Means, decreasing = F)][1:first_X_rows, .N, by = Label]
  if(dim(res1[Label == "Malicious"])[1] == 0){
    # mal_DT <- data.table(Label = "malicious", N = 0)
    # res1 <- rbindlist(list(res1, mal_DT))
    Presicion <- 0
    Recall <- 0
    return(list(res_precision = Presicion, 
                res_recall = Recall))
  } else{
    Presicion <- res1[Label == "Malicious", N]/first_X_rows
    Recall <- res1[Label == "Malicious", N]/(res1[Label == "Malicious", N] + (10 - res1[Label == "Malicious", N]) )
    
    return(list(res_precision = Presicion, 
                res_recall = Recall))
    
  }
}


get_Recall_Precision_iForest <- function(number_PCs){
  Precision_list <- list()
  Recall_list <- list()
  k <- 0
  for(i in seq(from = 100, to = 150000, by = 500)){
    k <- k +1 
    Precision_Recall <- calculate_Recall_Precision_iForest(number_PCs = number_PCs, first_X_rows = i)
    Precision_list[[k]] <- Precision_Recall[["res_precision"]]
    Recall_list[[k]] <- Precision_Recall[["res_recall"]]
  } 
  
  Presion_recall_DT  <- data.table(Subspace = number_PCs,
                                   Precision = unlist(Precision_list),
                                   Recall = unlist(Recall_list),
                                   lower_bound_scores = seq(from = 100, to = 150000, by = 500))
  return(Presion_recall_DT)
  fwrite(Presion_recall_DT, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", number_PCs, "_PCs.csv"))
  
}

# 5.1) Get -iForest- Precision & Recall for each subspace -------------------

temp1 <- get_Recall_Precision_iForest(number_PCs = 20)
fwrite(temp1, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 20, "_PCs.csv"))
temp2 <- get_Recall_Precision_iForest(number_PCs = 50)
fwrite(temp2, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 50, "_PCs.csv"))
temp3 <- get_Recall_Precision_iForest(number_PCs = 100)
fwrite(temp3, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 100, "_PCs.csv"))
temp4 <- get_Recall_Precision_iForest(number_PCs = 150)
fwrite(temp4, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 150, "_PCs.csv"))
temp5 <- get_Recall_Precision_iForest(number_PCs = 200)
fwrite(temp5, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 200, "_PCs.csv"))
temp6 <- get_Recall_Precision_iForest(number_PCs = 500)
fwrite(temp6, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 500, "_PCs.csv"))
temp7 <- get_Recall_Precision_iForest(number_PCs = 900)
fwrite(temp7, paste0("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT", 900, "_PCs.csv"))



# 5.2) Read -iForest- Precision & Recall for each subspace -------------------
recall_precision_20_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT20_PCs.csv")
recall_precision_50_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT50_PCs.csv")
recall_precision_100_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT100_PCs.csv")
recall_precision_150_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT150_PCs.csv")
recall_precision_200_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT200_PCs.csv")
recall_precision_500_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT500_PCs.csv")
recall_precision_900_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_Presion_recall_DT900_PCs.csv")


recall_precision_iForest <- rbindlist(list(recall_precision_20_iForest, recall_precision_50_iForest, recall_precision_100_iForest,
                                           recall_precision_150_iForest, recall_precision_200_iForest, recall_precision_500_iForest,
                                           recall_precision_900_iForest))

fwrite(recall_precision_iForest, "~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_PresionRecall_DT_all_PCs.csv")


# end of iForest section  -----------------------------------------------------


# 6) Read all results of iForest, LOF, Combined - PRECISION & RECALL ----------------------------

# 6.1)  AUCs for each subspace & Detector -----------------------------------------------------
all_subspaces_AUCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/all_subspaces_AUCs.csv")

## 6.2) Combined Presicion & Recall results -----------------------------------------------------

recall_precision_combo <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PresionRecall_DT_all_PCs.csv")
recall_precision_combo[, Subspace:= as.factor(Subspace)]
All_recall_DT_combo <- copy(recall_precision_combo[, .(Subspace, Recall, lower_bound_scores)])
All_precision_DT_combo <- copy(recall_precision_combo[, .(Subspace, Precision, lower_bound_scores)])

### 6.3) LOF Presicion & Recall results -----------------------------------------------------
recall_precision_LOF <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/LOF_PresionRecall_DT_all_PCs.csv")

### 6.4) iForest Presicion & Recall results -----------------------------------------------------
recall_precision_iForest <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/iForest_PresionRecall_DT_all_PCs.csv")



# 7) Combine LOF ensembles & iForest Ensembles -------------------------------

iForest_20PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs20_Allcombinations.csv")
iForest_50PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs50_Allcombinations.csv")
iForest_100PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs100_Allcombinations.csv")
iForest_150PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs150_Allcombinations.csv")
iForest_200PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs200_Allcombinations.csv")
iForest_500PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs500_Allcombinations.csv")
iForest_900PCs <- fread("~/Videos/R-Classification-LosAlamos/Latest_directory/PCs900_Allcombinations.csv")

LOF_20PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_20.csv")
LOF_50PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_50.csv")
LOF_100PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_100.csv")
LOF_150PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_150.csv")
LOF_200PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_200.csv")
LOF_50PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_500.csv")
LOF_900PCs <- fread("/home/giorgosk/Videos/R-Classification-LosAlamos/Latest_directory/ALL_LofScores_PCs_900.csv")


### We find the labels for the training set by aggregating the votes of each LOF&iForest combined Ensemble 
### if the majority of the votes of all the subspaces LOF&iForest combined Ensembles 

# PCs 20
combo_LOF_iForest20PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 20)[[2]]
combo_LOF_iForest20PCs[, index:= 1:.N]
mal_index20 <-  combo_LOF_iForest20PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest20PCs[, Predicted_label_20:= 0L]
combo_LOF_iForest20PCs[index %in% mal_index20, Predicted_label_20:= 1L]
combo_LOF_iForest20PCs[Predicted_label_20 == 1L, .N, by = Label]
# PCs 50
combo_LOF_iForest50PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 50)[[2]]
combo_LOF_iForest50PCs[, index:= 1:.N]
mal_index50 <-  combo_LOF_iForest50PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest50PCs[, Predicted_label_50:= 0L]
combo_LOF_iForest50PCs[index %in% mal_index50, Predicted_label_50:= 1L]
# PCs 100
combo_LOF_iForest100PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 100)[[2]]
combo_LOF_iForest100PCs[, index:= 1:.N]
mal_index100 <-  combo_LOF_iForest100PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest100PCs[, Predicted_label_100:= 0L]
combo_LOF_iForest100PCs[index %in% mal_index100, Predicted_label_100:= 1L]
# PCs 150
combo_LOF_iForest150PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 150)[[2]]
combo_LOF_iForest150PCs[, index:= 1:.N]
mal_index150 <-  combo_LOF_iForest150PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest150PCs[, Predicted_label_150:= 0L]
combo_LOF_iForest150PCs[index %in% mal_index150, Predicted_label_150:= 1L]
# PCs 200
combo_LOF_iForest200PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 200)[[2]]
combo_LOF_iForest200PCs[, index:= 1:.N]
mal_index200 <-  combo_LOF_iForest200PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest200PCs[, Predicted_label_200:= 0L]
combo_LOF_iForest200PCs[index %in% mal_index200, Predicted_label_200:= 1L]
# PCs 500
combo_LOF_iForest500PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 500)[[2]]
combo_LOF_iForest500PCs[, index:= 1:.N]
mal_index500 <-  combo_LOF_iForest500PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest500PCs[, Predicted_label_500:= 0L]
combo_LOF_iForest500PCs[index %in% mal_index500, Predicted_label_500:= 1L]
# PCs 900
combo_LOF_iForest900PCs <- calculate_AUC_LOF_iForest_Combined(number_PCs = 900)[[2]]
combo_LOF_iForest900PCs[, index:= 1:.N]
mal_index900 <-  combo_LOF_iForest900PCs[order(Means, decreasing = T)][1:1500, index]
combo_LOF_iForest900PCs[, Predicted_label_900:= 0L]
combo_LOF_iForest900PCs[index %in% mal_index900, Predicted_label_900:= 1L]


predicted_labels_allSubspaces <- as.data.table(do.call(cbind, list(combo_LOF_iForest20PCs[, Predicted_label_20], 
                                                                   combo_LOF_iForest50PCs[, Predicted_label_50],
                                                                   combo_LOF_iForest100PCs[, Predicted_label_100], 
                                                                   combo_LOF_iForest150PCs[, Predicted_label_150],
                                                                   combo_LOF_iForest200PCs[, Predicted_label_200],
                                                                   combo_LOF_iForest500PCs[, Predicted_label_500])))
# 7.1) Create predicted labels based on the collective ensembles --------------
predicted_labels_allSubspaces[, SUMS:= rowSums(predicted_labels_allSubspaces)]
predicted_labels_allSubspaces[, Final_predicted_label:= "Normal"]
predicted_labels_allSubspaces[SUMS>=3, Final_predicted_label:= "Malicious"]
predicted_labels_allSubspaces[, Label:= combo_LOF_iForest900PCs$Label]
predicted_labels_allSubspaces[Final_predicted_label == "Malicious", .N, by = Label]


predicted_Malicious_all_subspaces_DT <- copy(predicted_labels_allSubspaces)
predicted_Malicious_all_subspaces_DT[, index:= 1:.N]
fwrite(predicted_Malicious_all_subspaces_DT, "Latest_directory/predicted_Malicious_all_subspaces_DT.csv")

combo_LOF_iForest900PCs[Predicted_label_900 == 1L, .N, by = Label]
fwrite(combo_LOF_iForest900PCs, "Latest_directory/predicted_Malicious_900PCs_subspace_DT.csv")

# combo_LOF_iForest500PCs[Predicted_label_500 == 1L, .N, by = Label]
# fwrite(combo_LOF_iForest500PCs, "Latest_directory/predicted_Malicious_500PCs_subspace_DT.csv")

combo_LOF_iForest900PCs <- fread("Latest_directory/predicted_Malicious_900PCs_subspace_DT.csv")
combo_LOF_iForest900PCs[Predicted_label_900 == 1L, .N, by = Label]

predicted_Malicious_all_subspaces_DT <- fread("Latest_directory/predicted_Malicious_all_subspaces_DT.csv")
predicted_Malicious_all_subspaces_DT[Final_predicted_label == "Malicious", .N, by = Label]


