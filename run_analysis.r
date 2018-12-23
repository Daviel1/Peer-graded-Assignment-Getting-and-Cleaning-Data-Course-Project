# Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project
R script file for cleaning UCI HAR Dataset in sequential steps

## Download the data and unzip

#Convert string variables for download file

filename <- "UCIHARdata.zip"
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Database"

#Verify the download file, if it doesn't exist then downloading to the working directory

if(!file.exists(filename)){
        download.file(URL,filename, mode = "wb") 
}

#Verify the unzip file, if it doesn't exist then unzip the download file

if(!file.exists(dir)){
        unzip("UCIdata.zip", files = NULL, exdir=".")
}

## Read file

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

## Analysis

# Part 1. Merge the test and training datasets together in order to create one dataset
dataset <- rbind(X_train,X_test)

# Part 2. Extract only the measurements on the mean and standard deviation for each measurement.
# Create a vector of only mean and standard deviation, use the vector to subset.
meanorstd <- grep("mean()|std()", features[, 2]) 
dataset <- dataset[,meanorstd]

# Part 3. Appropriately labels the data set with descriptive activity names.
# Create vector of names without "()" and then apply to the dataSet to rename labels.
simplename <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(dataset) <- simplename[meanorstd]

# combine test and train of subject data and activity data and then give descriptive labels
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'

# combine subject, activity, and mean and standard deviation data set to create final data set.
dataset <- cbind(subject,activity, dataset)

# Part 4. Uses descriptive activity names to name the activities in the data set
# group the activity column of "dataset", re-name label by levels with "activity_lebels", and then apply it to "dataset".
group <- factor(dataset$activity)
levels(group) <- activity_labels[,2]
dataset$activity <- group

# Part 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# check if reshape2 package is installed
if (!"reshape2" %in% installed.packages()) {
        install.packages("reshape2")
}
library("reshape2")

# reshape/melt the data to tall skinny data remove means (average).  Write the tidydata to the working directory as "tidy_data.txt"
basedata <- melt(dataset,(id.vars=c("subject","activity")))
seconddataSet <- dcast(basedata, subject + activity ~ variable, mean)
names(seconddataSet)[-c(1:2)] <- paste("[mean of]" , names(seconddataSet)[-c(1:2)] )
write.table(seconddataSet, "tidy_data.txt", sep = ",")
