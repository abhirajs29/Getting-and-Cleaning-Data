## ---------------- load packages -----------------------------------------------------------
message("\n\n | Importing packages...")
require(data.table)
require(readr)		## | for data import
require(dplyr)		## | for data manipulation
require(tidyr)		## | for data tidying
require(tibble)		## | for tibbles (enhanced data frames)--part of tidying data
require(stringr)	## | for strings--part of tidying data


## ---------------- create directory --------------------------------------------------------
message("\n\n | | Creating directory to store data...")
if (!file.exists("data"))
{
	dir.create("data")
}


## ---------------- get link location -------------------------------------------------------
message("\n\n | Requesting URL...")
fileUrl      <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

message(" | | Importing packages...")
##-list parameters for [download.file]
download.file %>% args() %>% print()
#download.file(fileUrl, destfile="./data/Dataset.zip", method="curl")


## ---------------- record date downloaded --------------------------------------------------
message("\n\n | Recording date downloaded...")
downloadedOn <- date()
print(downloadedOn)

message("\n | | Verifying files in created directory...")
print(list.files("./data"))


## ---------------- read-in data ------------------------------------------------------------
message("\n\n | Reading data...")
##-list all package functions for [...]
#ls("package:readr") %>% print()

xtrain 	     <- read_delim("./data/train/X_train.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))
xtest        <- read_delim("./data/test/X_test.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))


## ---------------- manipulate data ---------------------------------------------------------
message("\n\n | Organizing data...")
##-merge train and test data
message(" | | 1. merging train and test data...")
x_train_test <- bind_rows(xtrain, xtest) %>% print()

##-add feature varaibles
message("\n | | 2. adding feature varaibles...")
features     <- read_delim("./data/features/features.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_character()))
fVec1        <- features$X2
fVec2        <- str_to_lower(fVec1)
fVec3        <- gsub("-", "", fVec2)
fVec4        <- gsub("[[:punct:]]", "", fVec3)

setnames(x_train_test, names(x_train_test), fVec4) %>% print()

##-select variables related to the [mean] and [standard deviation] from dataframe
message("\n | | 3. selecting variables related to the [mean] and [standard deviation] from dataframe...")
xtt2         <- x_train_test[1:6] %>% print()

##-add descriptive activites
message("\n | | 4. adding descriptive activites...")
ytrain       <- read_delim("./data/labels/Y_train.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))
ytest        <- read_delim("./data/labels/Y_test.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))

y_train_test <- bind_rows(ytrain, ytest)

act          <- read_delim("./data/labels/activity_labels.txt", delim = " ", col_names = FALSE, col_types = cols(
				X1 = col_number(),
				X2 = col_factor())
		)

ytt2 	     <- left_join(y_train_test, act)

res1         <- bind_cols(ytt2, xtt2) 
res2         <- rename(res1, activval=X1, activdesc=X2) %>% print()
res2[[2]]         <- gsub("_", "", res2[[2]])

##-add subjects
message("\n | | 5. adding subjects...")
subject1     <- read_delim("./data/train/subject_train.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))
subject2     <- read_delim("./data/test/subject_test.txt", delim = " ", col_names = FALSE, col_types = cols(.default = col_number()))
subject3     <- bind_rows(subject1, subject2)

res3         <- bind_cols(subject3, res2)
res4         <- rename(res3, subject=X1) %>% print()

##-find avg for each variable by [activity and subject]
message("\n | | 6. finding the [average] for each variable by [activity and subject]...")
res5         <- group_by(res4, subject, activval, activdesc) %>% summarise_at(vars(tbodyaccmeanx:tbodyaccstdz), mean)

print(res5)

##-fix widths and convert tibble to data.frame for compatability with write.fwf() function
resdf        <- as.data.frame(res5)

##-display final results
message("\n | | [ ] displaying final results...")
write.fwf(x = resdf, sep="\t\t", quote=FALSE, colnames=T, scientific = TRUE)

##-save results in .txt file
write.fwf(x = resdf, file = "results.txt", sep="\t\t", quote=FALSE, colnames=T, scientific = TRUE)

file.show("results.txt")

##-clear objects after task completed
rm(list=ls())
