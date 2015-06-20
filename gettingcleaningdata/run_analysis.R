library(reshape2)

# file paths
features.file <- "./features.txt"
activity.file <- "./activity_labels.txt"
xtrain.file   <- "./train/X_train.txt"
ytrain.file   <- "./train/y_train.txt"
strain.file   <- "./train/subject_train.txt"
xtest.file    <- "./test/X_test.txt"
ytest.file    <- "./test/y_test.txt"
stest.file    <- "./test/subject_test.txt"

## step 1: read in data from the various files for the test and 
## training data and combine them

# read features and activity labels
features <- read.table(features.file, stringsAsFactors=FALSE, col.names=c("origcol","origname"))
activities <- read.table(activity.file, stringsAsFactors=FALSE)

# read rest data & add as column "activity" 
xtest <- read.table(xtest.file)
ytest <- read.table(ytest.file)
xtest$activity <- ytest[, 1]

# read stest & add as column "subject" 
stest <- read.table(stest.file)
xtest$subject <- stest[, 1]

# read train data
xtrain <- read.table(xtrain.file)
ytrain <- read.table(ytrain.file)
xtrain$activity <- ytrain[, 1]

# read strain & add as column "subject" 
strain <- read.table(strain.file)
xtrain$subject <- strain[, 1]

# combine data
combined <- rbind(xtest, xtrain)

## step 1 completed

## step 2: find the names that match "-mean()" or "-std()"
## and subset those columns.

# extract mean & stdev
fcols <- grep("-mean\\(\\)|-std\\(\\)", features[,2])

# add two extra columns for "activity" and "subject"
cols <- c(fcols,ncol(combined)-1,ncol(combined))

# select the required subset of columns
selected <- combined[,cols]

## step 3: replace the activity number with its name 

# change activity column to name
selected$activity <- activities[selected$activity, 2]

## step 4: create a tidy version of the feature name by removing 
## dots, dashes & brackets and converting to lower case; apply
## the right subset as column names.

features$newname <- tolower(gsub("[\\.\\(\\)-]", "", features$origname))

names(selected) <- c(features$newname[fcols], "activity", "subject")

## step 5: melt the data into a standard form; recast it to calculate
## mean of each variable for each subject and activity

melted <- melt(selected, c("subject", "activity"))

means <- dcast(melted, subject + activity ~ variable, mean)

## save new data set to a file; write column names to a file
write.table(means, file="means.txt", row.names=FALSE)
write.table(features[fcols,c(3,1,2)], file="cols.txt", row.names=FALSE)