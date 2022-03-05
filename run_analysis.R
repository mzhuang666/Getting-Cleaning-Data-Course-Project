library(dplyr)

#dowload dataset

filename <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}


# Checking if folder exists
unzip(filename) 

#get data
features <- read.table("UCI HAR Dataset/features.txt", 
                       col.names = c("n","functions"))

activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("code", "activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     col.names = features$functions)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", 
                     col.names = "code")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                      col.names = features$functions)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                      col.names = "code")

#Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)
Merged_Data<-as_tibble(Merged_Data)

#Extracts only the measurements on the 
#mean and standard deviation for each measurement
extrac_dat <- select(Merged_Data,subject,code,contains("mean"),contains("std"))

#Uses descriptive activity names to name the activities in the data set.
extrac_dat$code <- activities[extrac_dat$code, 2]

#Appropriately labels the data set with descriptive variable names.
names(extrac_dat)

names(extrac_dat)[2] = "activity"

names(extrac_dat)<-gsub("Acc", "Accelerometer", names(extrac_dat))
names(extrac_dat)<-gsub("Gyro", "Gyroscope", names(extrac_dat))
names(extrac_dat)<-gsub("BodyBody", "Body", names(extrac_dat))
names(extrac_dat)<-gsub("Mag", "Magnitude", names(extrac_dat))
names(extrac_dat)<-gsub("^t", "Time", names(extrac_dat))
names(extrac_dat)<-gsub("^f", "Frequency", names(extrac_dat))
names(extrac_dat)<-gsub("tBody", "TimeBody", names(extrac_dat))
names(extrac_dat)<-gsub("-mean()", "Mean", names(extrac_dat), 
                        ignore.case = TRUE)
names(extrac_dat)<-gsub("-std()", "STD", names(extrac_dat), 
                        ignore.case = TRUE)
names(extrac_dat)<-gsub("-freq()", "Frequency", names(extrac_dat), 
                        ignore.case = TRUE)
names(extrac_dat)<-gsub("angle", "Angle", names(extrac_dat))
names(extrac_dat)<-gsub("gravity", "Gravity", names(extrac_dat))

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
tidydata<-extrac_dat%>%
    group_by(subject, activity)%>%
    summarise_all(funs(mean))




