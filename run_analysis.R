#1. Merges the training and the test sets to create one data set

## download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./FUCIDataSet.zip")

## Read data
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
feature <- read.table("./UCI HAR Dataset/features.txt")
dt_xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
dt_ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
dt_train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

dim(dt_xtrain)
dim(dt_ytrain)
dim(dt_train_subject)

# give column Name
colnames(activity) =c("activityId","activityName")
colnames(dt_train_subject) ="subjectId"
colnames(dt_ytrain) = "activityId" 
colnames(dt_xtrain) = feature[,2]

# bind all the columns and create final Data
trainData <- cbind(dt_ytrain,dt_train_subject,dt_xtrain)
dim(trainData)
head(trainData,2)

# Read Test Data
dt_xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
dt_ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
dt_test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")


dim(dt_test)
dim(activity)
dim(dt_test_subject)

head(dt_ytest,2)

# Give column name
colnames(dt_test_subject) ="subjectId"
colnames(dt_ytest) = "activityId" 
colnames(dt_xtest) = feature[,2]

#bind all column for test data and create final data
testData <- cbind(dt_ytest,dt_test_subject,dt_xtest)
dim(testDate)
dim(trainData)
intersect(names(trainData),names(testData))
# Create a  combine dataset from testData and trainData
completeData <- rbind(trainData,testData)

### Get the colnames from completeData

colNames  = colnames(completeData); 

##2.  Create logical Vectore where activity, subject, mean, std
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
                   grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) |
                   grepl("-std..",colNames) & !grepl("-std()..-",colNames))

sum(logicalVector==TRUE)

completeData = completeData[logicalVector==TRUE];

dim(completeData)


##3. Activity Name descriptive

completeData = merge(completeData,activity,by='activityId',all.x=TRUE)

colnames(completeData)

## 4. Appropriately labels the data set with descriptive variable names

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colNames

colnames(completeData) = colNames
colnames(completeData)
dim(completeData)
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dim(completeData)
colnames(completeData)


tidyData    = aggregate(completeData[,names(completeData) != c('activityId','subjectId')],by=list(activityId=completeData$activityId,subjectId = completeData$subjectId),mean);
tidyData    = merge(tidyData,activity,by='activityId',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
