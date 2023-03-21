library(data.table)
library(tidyverse)

# define the path of the project directory
pathdata = file.path(getwd(),"UCI HAR Dataset")
# Create a file which has the 28 file names
files = list.files(pathdata, recursive = TRUE)
files

#the feature and activity data-set
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#the train data-set
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
y_Train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
x_Train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#the test data-set
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
y_Test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
x_Test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# merging data-sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(y_Train, y_Test)
features <- rbind(x_Train, x_Test)

# appropriate nomenclature for the columns
colnames(features) <- featureNames[,2]

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
View(completeData)

# extracting only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
View(columnsWithMeanSTD)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

# this gives numbers from 1 to 6.
extractedData$Activity

# this gives the activity name based on numbers assigned.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
    extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
# check now
extractedData$Activity

extractedData$Activity <- as.factor(extractedData$Activity)

names(extractedData)

# Thought of making this in a loop but this made more sense.
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


names(extractedData)

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# creating a new tidy data-set and saving it.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
View(tidyData)
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)


