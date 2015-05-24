## This script assumes that the workind directory is the folder created by unziping 
## the data set zip file

#u se reshape 2 for mlet and cast functions
library(reshape2)

aggregateData <- function(dataname) {
  # Read the data set
  data <- read.table(paste0("./",dataname,"/X_",dataname,".txt"))
  # Read  feature names
  featureNames <- read.table("./features.txt")
  # Set the names in the train data
  names(data) <- featureNames[,2]
  
  # Select only the columns for the mean and standard deviations of the values.
  meanIdx <- grep("mean\\(", featureNames[,2])
  stdIdx <- grep("std\\(", featureNames[,2])
  columnIdx <- c(meanIdx, stdIdx)
  # Arrange the indexes in ascending order so as not to shuffle the columns.
  columnIdx <- sort(columnIdx)
  data <- data[,columnIdx]
  
  # Read in the subjects for the data set
  subjects <- read.table(paste0("./",dataname,"/subject_",dataname,".txt"))
  data$subject <- subjects[,1]
  
  # Read in the activities (the labels) for the data set
  activities <- read.table(paste0("./",dataname,"/y_",dataname,".txt"))
  data$activityId <- activities[,1]
  
  # Read in the activity labels for the data set
  activityLabels <- read.table("./activity_labels.txt")
  names(activityLabels) <- c("activityId", "activityName")
  # Merges the activity labels with the data to provide descriptive activity 
  data <- merge(data, activityLabels, sort = FALSE)
  # Reorder the columns to return the activityId column to the original position
  data <- data[,c(2:67,68,1,69)]  
}

cleanData <- function() {
  # Compiles the training and test data
  trainData <- aggregateData("train")
  testData <- aggregateData("test")
  # Bind data sets together
  data <- rbind(trainData, testData)
  
  #melt data sets
  dataMelt <- melt(data, id = c("subject","activityId","activityName"), 
                   measure.vars = names(data)[!names(data) %in% c("subject","activityId","activityName")]
  )
  
  # Cast the data to calculate the mean
  summaryData <- dcast(dataMelt, subject + activityId + activityName ~ variable, mean)    
}

# Compiles the tidy data
data <- cleanData()

# Writes the data to a file
write.table(data, "./tidy_smartphone_data.txt", row.names = FALSE)