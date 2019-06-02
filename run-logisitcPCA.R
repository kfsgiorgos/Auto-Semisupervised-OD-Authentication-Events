ResultMerge <- fread("ResultAllUsers.csv")

LabelsAllUsers <- copy(ResultMerge[, .(TimeInSeconds, Source_User, 
	Destination_Comp, Label, Time)])
LabelsAllUsers[, nRowIndex:= 1:.N]
## Decrease the dataset to make the Onehot Enoding feasible
LabelsAllUsers[4500000:4650000, .N, by = "Label"]

subsetColsAll <- copy(ResultMerge[, !c("Time", "TimeInSeconds", 
	"Label", "dayTime")])
SubsetColsRows <- copy(subsetColsAll[4500000:4650000])
SubsetColsRows[, names(SubsetColsRows)[c(1:10)]:= lapply(.SD, 
	function(x) as.factor(as.numeric(x))), .SDcols = c(1:10)]
str(SubsetColsRows)

system.time(DT_Onehot <- one_hot(SubsetColsRows))
## Check if we have 0 or 1 in some columns for all rows in order to exclude those columns
temp <- transpose(DT_Onehot[, lapply(.SD, sum), .SDcols = names(DT_Onehot)])
temp[, colNames:= names(DT_Onehot[, lapply(.SD, sum), 
	.SDcols = names(DT_Onehot)])]
temp[V1 == 0 | V1 == dim(temp)[1]]
 
logPCA_result <- logisticPCA(DT_Onehot, k = 900, m = 14, max_iters = 5000,
	partial_decomp = TRUE, random_start = TRUE)
saveRDS(logPCA_result, "LogisticPCA_900k_14ms.rds")