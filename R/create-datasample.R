# install & load the required packages
pkg <- c("ggplot2", "logisticPCA", "lubridate", "foreach", "data.table", 
	"rARPACK", "dplyr", "mltools", "tidyverse", "igragh")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
  lapply(new.pkg, require, character.only = TRUE)
} else{
  lapply(pkg, require, character.only = TRUE)
}

options(scipen = 9999)

# Red Team Dataset
redTeam <- fread("redteam.txt")
redTeamUnique <- unique(redTeam)
redTeamUnique[, V5 := lubridate::as_datetime(V1)]
setnames(redTeamUnique, c("V1", "V2", "V3", "V4", "V5"),
         c("TimeInSeconds", "Source_User","Source_Comp", 
           "Destination_Comp", "Time"))
redTeamUnique[, Label := "Malicious"]

# Read the datasets we have already created for each user
# ----- Normal Users -----
U22 <- fread("user22.txt", nThread = 40)
# reduce the number of rows for each User
U22 <- U22[V1 < 2600000]

Anonymous<- fread("ANONYMOUS_LOGON_C586.txt", nThread = 40)
Anonymous <- Anonymous[V1 < 2600000]

C480 <- fread("userC480.txt", nThread = 40)
C480 <- C480[V1 < 2600000]

U194 <- fread("U194DOM1_Norm.txt", nThread = 40)
U194 <- U194[V1 < 2600000]

C830 <- fread("C830_Norm.txt", nThread = 40)
C830 <- C830[V1 < 2600000]

U228 <- fread("U228DOM1_Norm.txt", nThread = 40)
U228 <- U228[V1 < 2600000]
NormalUsers <- rbindlist(list(U22, Anonymous, C480, U194, C830, U228))
setkey(NormalUsers, V1)

# ----- Malicious Users -----
U3005 <- fread("U3005_DOM1_Mal.txt", nThread = 40)
U3005 <- U3005[V1 < 2600000]

U66 <- fread("user66.txt", nThread = 40)
U66 <- U66[V1 < 2600000]

U737 <- fread("U737_Mal.txt", nThread = 40)
U737 <- U737[V1 < 2600000]

U748 <- fread("U748_Mal.txt", nThread = 40)
U748 <- U748[V1 < 2600000]

U293 <- fread("U293_Mal.txt", nThread = 40)
U293 <- U293[V1 < 2600000]

MaliciousUsers <- data.table::rbindlist(list(U3005, U66, U737, U748, U293))
setkey(MaliciousUsers, V1)
## Combine NormalUsers & MaliciousUsers & then order by Time
AllUsers <- data.table::rbindlist(list(NormalUsers, MaliciousUsers))
setkey(AllUsers, V1)

## Change column Names
data.table::setnames(AllUsers, names(AllUsers), 
	c("TimeInSeconds", "Source_User", "Destination_User", 
	"Source_Comp", "Destination_Comp", "Authentication_Type", 
	"LogonType", "Authnetication_Orientation", 
	"Success_Failure"))

AllUsers[, Time := lubridate::as_datetime(TimeInSeconds)]
AllUsers[, dayTime := day(Time)]
# order dataset by Time both 
setkey(AllUsers, TimeInSeconds, Source_User, Source_Comp, Destination_Comp)
setkey(redTeamUnique, TimeInSeconds, Source_User, Source_Comp, Destination_Comp)

# join by merging to label the resulted dataset
ResultMerge <- merge(AllUsers, redTeamUnique, all.x=TRUE)


MalUsers <- c("U3005@DOM1", "U66@DOM1", "U737@DOM1", "U748@DOM1", "U293@DOM1")
redTeamUnique[Source_User %in% MalUsers]
### ---- Check if the merge was correct based on the redTeam dataset
# Events that belong to RedTeam but do not exist in the Authentication Usage Dataset
UU <- merge(redTeamUnique[Source_User %in% MalUsers], ResultMerge[Label == "Malicious"], all.x = T)
UU[is.na(Label.y)]
## Those are missing as Malicious

ResultMerge[TimeInSeconds %in% c(752613, 758915) & Source_User == "U737@DOM1" & Source_Comp == "C17693"]

### Fix it
ResultMerge[TimeInSeconds %in% c(752613, 758915) & Source_User == "U737@DOM1" & 
	    Source_Comp == "C17693", Label:= "Malicious"]

### Check again if everything is Correct
dim(ResultMerge[Label == "Malicious"])
dim(redTeamUnique[Source_User %in% MalUsers])

###  check if MERGE is correct
ResultMerge[is.na(Label), Label:= "Normal"]
ResultMerge[, Time.y:=NULL]
setnames(ResultMerge, "Time.x", "Time")


# Create 4 new columns based on destination user & source user characteristics
ResultMerge[, DestUser:= grepl(x = Destination_User, pattern = "/?[$]")]
ResultMerge[DestUser == TRUE, DestUser_Dollar:= 1L]
ResultMerge[DestUser == FALSE, DestUser_Dollar:= 0L]
ResultMerge[, DestUser:=NULL]
ResultMerge[Source_User != Destination_User, SuspiciousAuth:= 1L]
ResultMerge[Source_User == Destination_User, SuspiciousAuth:= 0L]

ResultMerge[, names(ResultMerge)[c(2:9, 11, 13:14)]:= 
	    lapply(.SD, function(x) as.numeric(as.factor(x)) ), .SDcols = c(2:9, 11, 13:14)]

fwrite(ResultMerge, "ResultAllUsers.csv", nThread = 40)
