


setwd("C:/Users/kfagan/Documents/Coursera/CleanData/week4")

library(dplyr)
library(sqldf)

temp = tempfile()
fileURL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, temp, mode='wb')
unzip(temp)
unlink(temp)

# Read in files
features = read.table('./UCI HAR Dataset/features.txt') # 561   2
activityLabels  = read.table('./UCI HAR Dataset/activity_labels.txt')
colnames(activityLabels) <- c('activityId','activityType')

train = read.table('./UCI HAR Dataset/train/X_train.txt')  # 7352  561
activityTrain = read.table('./UCI HAR Dataset/train/y_train.txt') # 7352    1
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt') # 7352    1

test = read.table('./UCI HAR Dataset/test/X_test.txt') # 2947  561
activityTest = read.table('./UCI HAR Dataset/test/y_test.txt') # 2947    1
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt') # 2947    1

# Add column labels to test and train datasets
identifyingInfo = c('subjectId', 'activityId')

train = cbind(subjectTrain, activityTrain, train)
colnames(train) = c(identifyingInfo, as.character(features[,2]))

test = cbind(subjectTest, activityTest, test)
colnames(test) = c(identifyingInfo, as.character(features[,2]))

# Append test and train data
mergedData = rbind(train, test)

# Extract only the columns containing mean and std measurements
colWanted = c(identifyingInfo, grep("mean\\(\\)|std\\(\\)", names(mergedData), value = TRUE))
colWanted
meanStd = mergedData[,colWanted]

# Remove special characters in column names and make variable names readable by
# capitalizing mean and std and descriptive by spelling out abbreviations 

names(meanStd) = names(meanStd) %>% 
  gsub("[(]", '', .) %>% 
  gsub("[)]", '', .) %>% 
  gsub("[-]", '', .) %>%
  gsub("[,]", '', .) %>% 
  gsub("mean", 'Mean', .) %>% 
  gsub("std", 'Std', .) %>%
  gsub("^t", 'time', .) %>% 
  gsub("^f", 'frequency', .) %>%
  gsub("Acc", 'Acceleration', .) %>% 
  gsub("Gyro", 'Gyroscopic', .) %>% 
  gsub("Mag", 'Magnitude', .) %>%
  gsub("BodyBody", 'Body', .)

# Replace activity ids with the activity id description
activityNames = merge(meanStd, activityLabels, by = 'activityId')
activityNames$activityType = tolower(activityNames$activityType)

final = activityNames %>% select(subjectId, activityType, timeBodyAccelerationMeanX:frequencyBodyGyroscopicJerkMagnitudeStd)


# Create tidy dataset with column averages by activity and subject

tidy = final %>% group_by(activityType, subjectId) %>% summarise_all(mean)


write.table(tidy, "GCDTidyData.txt", row.name=FALSE)
