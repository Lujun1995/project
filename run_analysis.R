##1\get the data from the website and load into R
if(!file.exists("./data")) dir.create("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/datasets.zip")
unzip("datasets.zip", exdir = "./data" )
train.x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
train.subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test.x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
test.subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)


##2\Merges the training and the test sets to create one data set
train <- cbind(train.subject, train.y, train.x)
test <- cbind(test.subject, test.y, test.x)
HAR <- rbind(train, test)
colnames(HAR)[1]<-"subject"
colnames(HAR)[2]<-"acitivity"

##3\Extracts only the measurements on the mean and standard deviation for each measurement.
featureName <- features[,2]
nameindex <- grep("mean|std", featureName)
HAR_clear <- HAR[,c(1,2,nameindex+2)]
colnames(HAR_clear)[3:(length(nameindex)+2)] <- featureName[nameindex]

##4\Uses descriptive activity names to name the activities in the data set.
HAR_clear$acitivity <- factor(HAR_clear$acitivity, levels = 1:6, labels = activity[,2])

##5\Appropriately labels the data set with descriptive variable names.
names(HAR_clear) <- gsub("\\()", "", names(HAR_clear))
names(HAR_clear) <- gsub("^t", "time", names(HAR_clear))
names(HAR_clear) <- gsub("^f", "frequency", names(HAR_clear))
names(HAR_clear) <- gsub("-mean", "Mean", names(HAR_clear))
names(HAR_clear) <- gsub("-std", "Std", names(HAR_clear))
write.table(HAR_clear, "./data/HAR_clear.txt", row.names = FALSE)

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
HAR_group <- group_by(HAR_clear, subject, acitivity)
HAR_average <- summarise_all(HAR_group, funs(mean))
write.table(HAR_average, "./data/HAR_average.txt", row.names = FALSE)
