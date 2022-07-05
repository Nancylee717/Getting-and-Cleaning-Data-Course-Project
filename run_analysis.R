install.packages("R.utils")
library(R.utils)
library(R.oo)
library(R.methodsS3)
library(readr)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "Dataset.zip")
unzip(zipfile = "Dataset.zip",list=TRUE)
features <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/features.txt", col.names = c("f_id","functions"))
activities <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test<- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/test/X_test.txt",col.names = features$functions)
y_test <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/test/y_test.txt",col.names = "code")
x_train <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/train/X_train.txt",col.names = features$functions)
y_train <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/train/y_train.txt",col.names = "code")
subject_train <- read.table("D:/NancyLee/Rpractice/r4ds/Getting and Cleaning Data Course Project/Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

##1.Merges the training and the test sets to create one data set
x_te <- cbind(y_test,x_test)
y_tr <- cbind(y_train,x_train)
subject <- rbind(subject_test,subject_train)
xy <- rbind(y_tr,x_te)
tidy <- cbind(subject,xy)

##2.Extracts only the measurements on the mean and standard deviation for each measurement
library(dplyr)
tidy_sd <- tidy %>%
  select(subject,code,contains("mean"),contains("std"))

##3.Uses descriptive activity names to name the activities in the data set
tidy_sd$code <- activities[tidy_sd$code,2]

##4.Appropriately labels the data set with descriptive variable names
View(tidy_sd)
names(tidy_sd)[2] <- "activity"
names(tidy_sd)<-gsub("Acc", "Accelerometer", names(tidy_sd))
names(tidy_sd)<-gsub("Gyro", "Gyroscope", names(tidy_sd))
names(tidy_sd)<-gsub("BodyBody", "Body", names(tidy_sd))
names(tidy_sd)<-gsub("Mag", "Magnitude", names(tidy_sd))
names(tidy_sd)<-gsub("^t", "Time", names(tidy_sd))
names(tidy_sd)<-gsub("^f", "Frequency", names(tidy_sd))
names(tidy_sd)<-gsub("tBody", "TimeBody", names(tidy_sd))
names(tidy_sd)<-gsub("-mean()", "Mean", names(tidy_sd), ignore.case = TRUE)
names(tidy_sd)<-gsub("-std()", "Std", names(tidy_sd), ignore.case = TRUE)
names(tidy_sd)<-gsub("-freq()", "Frequency", names(tidy_sd), ignore.case = TRUE)
names(tidy_sd)<-gsub("angle", "Angle", names(tidy_sd))
names(tidy_sd)<-gsub("gravity", "Gravity", names(tidy_sd))

##5.From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.
tidym_sd <- tidy_sd %>%
  group_by(subject,activity) %>%
  summarise_all(funs(mean))
write.table(tidym_sd,"tidym_sd.txt",row.name=FALSE)





