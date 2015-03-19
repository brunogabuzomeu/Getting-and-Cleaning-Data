
##
## Get the original data
##
# setwd("C:\\Moje Dokumenty\\Documents\\3ST\\R_programming")
# download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="UCI_HAR_Dataset.zip")
# unzip("UCI_HAR_Dataset.zip")

# Read files
features        <- read.table("UCI HAR Dataset/features.txt",        header=FALSE, stringsAsFactors=FALSE)
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, stringsAsFactors=FALSE)

x_test        <- read.table("UCI HAR Dataset/test/X_test.txt",       header=FALSE)
y_test        <- read.table("UCI HAR Dataset/test/y_test.txt",       header=FALSE)
subject_test  <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
test_data     <- cbind(subject_test,y_test,x_test)

x_train       <- read.table("UCI HAR Dataset/train/X_train.txt",       header=FALSE)
y_train       <- read.table("UCI HAR Dataset/train/y_train.txt",       header=FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
train_data    <- cbind(subject_train,y_train,x_train)

# 1. Merge test and training sets
test_data$Set_origin  <- "test"
train_data$Set_origin <- "train"
data <- rbind(train_data,test_data)
colnames(data) <- c("Subject_id","activity_id",features[,2],"Set_origin")

# Free memory
rm(x_test,  y_test, subject_test, test_data)
rm(x_train, y_train,subject_train,train_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Three columns added on the left
data <- data[,c(1,2,dim(data)[2],grep("-mean()|-std()",features[,2])+2)]

# 3. Uses descriptive activity names to name the activities in the data set
colnames(activity_labels) <- c("activity_id","Activity")
data <- merge(data,activity_labels)
data$activity_id <- NULL

# 4. Appropriately labels the data set with descriptive variable names. 
# (Done in steps 1 and 3).

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
data$Subject_id <- as.factor(data$Subject_id)
data$Activity   <- as.factor(data$Activity)
avgData <- aggregate(data, 
                     by=list(Subject_id=data$Subject_id, Activity=data$Activity,Set_origin=data$Set_origin), 
                     FUN=mean)
avgData[,dim(avgData)[2]] <- NULL
avgData[,5] <- NULL
avgData[,4] <- NULL

write.table(avgData, file = "Average data per subject and activity.txt", row.name=FALSE)

# Free memory
rm(data,  avgData, activity_labels,  features)
