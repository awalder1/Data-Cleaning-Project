## One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
##http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
##Here are the data for the project:
##  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##this program  takes the above data set, merges the test and training data sets
##remove all variables with mean and standard deviation and create a new dataset

library(dplyr)
library(reshape2)

setwd("C:/Users/Annette/Documents/Data Science/Data Cleaning/course project/UCI/UCI HAR Dataset")
##Bring in labels and features

activityLabels <- read.table("activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("features.txt")
features[,2] <- as.character(features[,2])




##Bring in test data and merge subject and features

testSubjects <- read.table("./test/subject_test.txt")
colnames(testSubjects) <- c("subject") 

##add labels to activities
testActivities <- read.table("./test/Y_test.txt")
testActivities$activity <- factor(testActivities$V1, levels = activityLabels[,1], labels = activityLabels[,2]) 
activity <- testActivities[,2]
head(activity)
##bring in test data and grab mean and std

test <- read.table("./test/X_test.txt") 
##want to extract only mean and std
MeanStd <- grep(".*mean.*|.*std.*", features[,2])
MeanStd.names <- features[MeanStd,2]

##remove parenthesis and dashes from name
##other programs do not take these

MeanStd.names = gsub('-mean', 'Mean', MeanStd.names)
MeanStd.names = gsub('-std', 'Std', MeanStd.names)
MeanStd.names <- gsub('[-()]', '', MeanStd.names)
test <- test[, MeanStd]
test <- setNames(test,MeanStd.names)

#combine all files
##add group = test so we can distinguish which data set the
##data originally came from
test <- cbind(testSubjects, grp="Test", activity, test)


##Bring in training data and merge subject and features

trainSubjects <- read.table("./train/subject_train.txt")
colnames(trainSubjects) <- c("subject")

##add labels to activity

trainActivities <- read.table("./train/Y_train.txt")
trainActivities$activity <- factor(trainActivities$V1, levels = activityLabels[,1], labels = activityLabels[,2]) 
activity <- trainActivities[,2]

##bring in dataset and only grab means and std

train <- read.table("./train/X_train.txt")
##want to extract only mean and std
MeanStd <- grep(".*mean.*|.*std.*", features[,2])
MeanStd.names <- features[MeanStd,2]

##remove parenthesis and dashes from name
##other programs do not take these

MeanStd.names = gsub('-mean', 'Mean', MeanStd.names)
MeanStd.names = gsub('-std', 'Std', MeanStd.names)
MeanStd.names <- gsub('[-()]', '', MeanStd.names)

train <- train[, MeanStd]
train <- setNames(train,MeanStd.names)

##merge all files
##add group = test so we can distinguish which data set the
##data originally came from
train <- cbind(trainSubjects, grp="Training", activity , train)

##merge data . Since both groups have the same variables we
## will merge by stacking the data

FinalData <- rbind(train, test)
FinalData$subject <- as.factor(FinalData$subject)
FinalData$activity <- as.factor(FinalData$activity)
FinalData$grp <- as.factor(FinalData$grp)
 
FinalData.melted <- melt(FinalData, id = c("subject", "grp", "activity"))
FinalData.mean <- dcast(FinalData.melted, subject + grp + activity ~ variable, mean)


## Output the dataset

write.table(FinalData.mean, "HumanActivityMeans.txt", row.names = FALSE, quote = FALSE)
