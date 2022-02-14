#------------------------------------------------------#
# Created by: Kevin Jones
# Coursera Getting and Cleaning Data: Week 4 Assignment
#------------------------------------------------------#

# You should create one R script called run_analysis.R that does the following. 

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4 Appropriately labels the data set with descriptive variable names. 
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load packages
library(plyr)
library(downloader)
library(knitr)

# Test and create directory if needed....just in case.
if(!file.exists("projectDataFile")){
  dir.create("projectDataFile")
}
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# If the zip file doesn't exist download it from the above url
if(!file.exists("projectDataFile/project_Dataset.zip")){
  download.file(Url,destfile="projectDataFile/project_Dataset.zip", mode = "wb")
}

# Unzip 
if(!file.exists("projectDataFile/UCI HAR Dataset")){
  unzip(zipfile="projectDataFile/project_Dataset.zip",exdir="projectDataFile")
}

# List files
path <- file.path("projectDataFile" , "UCI HAR Dataset")
files<-list.files(path, recursive=TRUE)

# train data set
labelTrain <- read.table(file.path(path, "train", "y_train.txt"), header = FALSE)
# test data set
labelTest  <- read.table(file.path(path, "test" , "y_test.txt" ), header = FALSE)
# Subjects
subTrain <- read.table(file.path(path, "train", "subject_train.txt"), header = FALSE)
subTest  <- read.table(file.path(path, "test" , "subject_test.txt"), header = FALSE)

# Set training and test data sets
setTrain <- read.table(file.path(path, "train", "X_train.txt"), header = FALSE)
setTest  <- read.table(file.path(path, "test" , "X_test.txt" ), header = FALSE)

# Merge training and test data sets
rSub <- rbind(subTrain, subTest)
rLabel<- rbind(labelTrain, labelTest)
rSet<- rbind(setTrain, setTest)

# Set names
names(rSub)<-c("subject")
names(rLabel)<- c("activity")
rSetNames <- read.table(file.path(path, "features.txt"), head=FALSE)
names(rSet)<- rSetNames$V2

# Merge data set columns
dataCombine <- cbind(rSub, rLabel)
merge <- cbind(rSet, dataCombine)

# Mean and Standard Dev
subrSetNames<-rSetNames$V2[grep("mean\\(\\)|std\\(\\)", rSetNames$V2)]
selectedNames<-c(as.character(subrSetNames), "subject", "activity" )
merge<-subset(merge,select=selectedNames)

#Read in the Activity Labels document
activityLabels <- read.table(file.path(path, "activity_labels.txt"),header = FALSE)
merge$activity<-factor(merge$activity,labels=activityLabels[,2])

# Label merged data set
names(merge)<-gsub("^t", "time", names(merge))
names(merge)<-gsub("^f", "frequency", names(merge))
names(merge)<-gsub("Gyro", "Gyroscope", names(merge))
names(merge)<-gsub("Acc", "Accelerometer", names(merge))
names(merge)<-gsub("BodyBody", "Body", names(merge))
names(merge)<-gsub("Mag", "Magnitude", names(merge))

# output tidy data set result
newData<-aggregate(. ~subject + activity, merge, mean)
newData<-newData[order(newData$subject,newData$activity),]
write.table(newData, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')
