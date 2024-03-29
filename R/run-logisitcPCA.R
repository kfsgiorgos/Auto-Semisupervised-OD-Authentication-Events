
# Read the dataset with all users
ResultMerge <- fread("ResultAllUsers.csv")

# Subset columns. Time will not be included in our analysis
subsetColsAll <- copy(ResultMerge[, !c("Time", "TimeInSeconds", 
	"Label", "dayTime")])

# the subset that we are working refers to these rows "4500000:4650000"
SubsetColsRows <- copy(subsetColsAll[4500000:4650000])
SubsetColsRows[, names(SubsetColsRows)[c(1:10)]:= lapply(.SD, 
	function(x) as.factor(as.numeric(x))), .SDcols = c(1:10)]

# Do one-hot-encoding
system.time(DT_Onehot <- one_hot(SubsetColsRows))

## Check if we have 0 or 1 in some columns for all rows in order to exclude those columns
temp <- data.table::transpose(DT_Onehot[, lapply(.SD, sum), .SDcols = names(DT_Onehot)])
temp[, colNames:= names(DT_Onehot[, lapply(.SD, sum), 
	.SDcols = names(DT_Onehot)])]
temp[V1 == 0 | V1 == dim(temp)[1]]

# Apply Logistic PCA. 
# A proposed rule of thumb for selecting the number of components is to
# reduce the dimansionality of DT_Onehot by 2/3.  
logPCA_result <- logisticPCA(DT_Onehot, k = 900, m = 14, max_iters = 5000,
	partial_decomp = TRUE, random_start = TRUE)

saveRDS(logPCA_result, "LogisticPCA_900k_14ms.rds")
