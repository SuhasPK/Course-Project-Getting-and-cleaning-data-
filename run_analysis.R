
# define the path of the project directory
pathdata = file.path(getwd(),"UCI HAR Dataset")
# Create a file which has the 28 file names
files = list.files(pathdata, recursive = TRUE)
files


# The objective here is to make the test and train data as per the sequence stated above.
# Four basic level data sets will be defined and created:
# 1. test dataset
# 2. train dataset
# 3. features dataset
# 4. activity labels dataset

# 1.Reading training tables - xtrain, ytrain, subject train
xtrain <- read.table(file.path(pathdata,"train","X_train.txt"), header=FALSE)
ytrain <- read.table(file.path(pathdata,"train","y_train.txt"), header=FALSE)
subject_train <- read.table(file.path(pathdata,"train","subject_train.txt"), header = FALSE)

# 2.Reading testing tables xtest, ytest, subject_test
xtest <- read.table(file.path(pathdata,"test","X_test.txt"), header = FALSE)
ytest <- read.table(file.path(pathdata,"test","y_test.txt"), header = FALSE)
subject_test <- read.table(file.path(pathdata,"test","subject_test.txt"), header = FALSE)

# 3.Read the features data
features <- read.table(file.path(pathdata,"features.txt"), header = FALSE)

# 4.Read activity labels data
activityLabels <- read.table(file.path(pathdata,"activity_labels.txt"), header = FALSE)

## Uses descriptive activity names to name the activities in the data set.
# Create Sanity and Column Values to the Train Data.
colnames(xtrain) <-features[,2]
colnames(ytrain) <-"activityId" 
colnames(subject_train) <- "subjectId"

# Create Sanity and Column Values to the Test Data.
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityId"
colnames(subject_test) <- "subjectId"

#Create Sanity and Column Values to the activity label Data.
colnames(activityLabels) <- c('activityId','activityType')

## MERGING TEST AND TRAIN DATA
merge_train <- cbind(ytrain,subject_train,xtrain)
merger_test <- cbind(ytest,subject_test,xtest)
# Create the main data table merging both tables.
setAllInOne <- rbind(merge_train,merger_test)

## Extracting only the measurements on the mean and standard deviation for each measurement.
# read all the values that are available.
colNames <- colnames(setAllInOne)
# Need to get a subset of all the mean and standards and the correspondongin activityID and subjectID.
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
#A subtset has to be created to get the required dataset
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

## Use descriptive activity names to name the activities in the data set.
setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)
View(setWithActivityNames)

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Get the average of each variable for each activity.
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
View(secTidySet)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
View(secTidySet)

## Saving the new data
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
