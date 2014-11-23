#Download the file 
#furl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(furl  ,destfile = "", method = "curl")

#1. Merges the training and the test sets to create one data set.

trainX1 <- read.table("UCI HAR Dataset/train/X_train.txt")      #train set
testX1 <- read.table("UCI HAR Dataset/test/X_test.txt")         #test set

X <- rbind(trainX1, testX1) #row merge the data


trainY1 <- read.table("UCI HAR Dataset/train/y_train.txt")      #train labels
testY1 <- read.table("UCI HAR Dataset/test/y_test.txt")         #test labels

Y <- rbind(trainY1, testY1) #row merge the data

trainS1 <- read.table("UCI HAR Dataset/train/subject_train.txt") #train subject information
testS1 <- read.table("UCI HAR Dataset/test/subject_test.txt")    #test subject information

S <- rbind(trainS1, testS1) #row merge the data

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

fea <- read.table("UCI HAR Dataset/features.txt")               #read the list of all features
extrctMeanStd <- grep("-mean\\(\\)|-std\\(\\)", fea[, 2])       #extract only mean & std list
X <- X[, extrctMeanStd]                                         #extract mean and std measurements only
names(X) <- fea[extrctMeanStd, 2]                               #update col names



#3 Uses descriptive activity names to name the activities in the data set

act <- read.table("UCI HAR Dataset/activity_labels.txt")        #read activity info
Y[,1] = act[Y[,1], 2]                                           #update activity info
names(Y) <- "activity_type"                                     #update col names


#4 Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
tidyData <- cbind(S, Y, X)                                       #column info
write.table(tidyData, "tidy_data_step4.txt", row.names=FALSE,sep=",")

#tidyData_Avg <- apply(tidyData[ tidyData$subject==1, 3:68], 2, mean) #apply didn't work

#5 Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueS = unique(S)[,1]
cntS = length(unique(S)[,1])
countA = length(act[,1])
cols = dim(tidyData)[2]
tidyData_Avg  = tidyData[1:(cntS*countA), ]

row = 1
for (i in 1:cntS) {
        for (g in 1:countA) {
                tidyData_Avg [row, 1] = uniqueS[i]
                tidyData_Avg [row, 2] = act[g, 2]
                temp <- tidyData[tidyData$subject==i & tidyData$activity_type==act[g, 2], ]
                tidyData_Avg [row, 3:cols] <- colMeans(temp[, 3:cols])
                row = row+1
        }
}
write.table(tidyData_Avg , "tidy_data_set.txt",  row.names=FALSE)
