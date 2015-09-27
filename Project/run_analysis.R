#######################################################################################################################
# You should create one R script called run_analysis.R that does the following.                                       
# 
#  1. Merges the training and the test sets to create one data set.
#  2. Extracts only the measurements on the mean and standard deviation for each measurement.
#  3. Uses descriptive activity names to name the activities in the data set.
#  4. Appropriately labels the data set with descriptive activity names.
#  5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#######################################################################################################################  

# Set work directory
setwd("./Project")
path <- getwd()

# Load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# Get the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f))

# Unzip the file in ./UCI HAR Dataset
unzip(file.path(path, f), files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".", unzip = "internal",
             setTimes = FALSE)

# Extracting zip file it put all the files in a folder named `UCI HAR Dataset`. 
# Set this folder as the input path and list the files:
pathInput <- file.path(path, "UCI HAR Dataset")
list.files(pathInput, recursive=TRUE)

# Read the files required
## 1 - subject files

subjectTrain <- fread(file.path(pathInput, "train", "subject_train.txt"))
subjectTest  <- fread(file.path(pathInput, "test" , "subject_test.txt" ))

## 2 - activity files

activityTrain <- fread(file.path(pathInput, "train", "y_train.txt"))
activityTest  <- fread(file.path(pathInput, "test" , "y_test.txt" ))

## 3 - features data files
fileToDataTable <- function (f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dataTrain <- fileToDataTable(file.path(pathInput, "train", "X_train.txt"))
dataTest  <- fileToDataTable(file.path(pathInput, "test" , "X_test.txt" ))

############## 1 - Merge the training and the test sets ###################

# a) Concatenate the data tables.
subject <- rbind(subjectTrain, subjectTest)
setnames(subject, "V1", "subject")
activity <- rbind(activityTrain, activityTest)
setnames(activity, "V1", "activityNum")
data <- rbind(dataTrain, dataTest)
# b) Merge columns.
subject <- cbind(subject, activity)
data <- cbind(subject, data)
# c) Set key.
setkey(data, subject, activityNum)

################### 2 Extract only the mean and standard deviation ##################
# Read the `features.txt` file. 
# This tells which variables in 'data' are measurements for the mean and standard deviation.
features <- fread(file.path(pathInput, "features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))

# Subsetting only measurements for the mean and standard deviation
features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Convert the column numbers to a vector of variable names matching columns in `data`
features$featureCode <- features[, paste0("V", featureNum)]
# Subset using variable names
selectFeature <- c(key(data), features$featureCode)
data <- data[, selectFeature, with=FALSE]

######################## 3 - Use descriptive activity names #############################

# Read `activity_labels.txt` file to add descriptive names to the activities.
activityNames <- fread(file.path(pathInput, "activity_labels.txt"))
setnames(activityNames, names(activityNames), c("activityNum", "activityName"))

####################### 4 - Appropriately label with descriptive activity names #########

# Merge activity labels.
data <- merge(data, activityNames, by="activityNum", all.x=TRUE)

# Add `activityName` as a key
setkey(data, subject, activityNum, activityName)

# Melt the data table to reshape it 
data <- data.table(melt(data, key(data), variable.name="featureCode"))

# Merge activity name
data <- merge(data, features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Create a new equivalent variables
data$activity <- factor(data$activityName)
data$feature <- factor(data$featureName)

