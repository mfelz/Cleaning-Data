#Download neccesary packages
library(plyr)
library(dplyr)

#Download file from the URL given
temp <- tempfile()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,temp)
#get list of files in zip folder
list.files <- unzip(temp, list = TRUE)

#Get activity and feature labels from zip folder file 1 & 2
activity.labels <- read.table(unzip(temp,file = list.files[1,1])) 
features.text <- read.table(unzip(temp,file = list.files[2,1])) 

#Get test set files. Files 16, 17 & 18
test.subject <- read.table(unzip(temp,file = list.files[16,1]))
test.set <- read.table(unzip(temp,file = list.files[17,1]))
test.labels <- read.table(unzip(temp,file = list.files[18,1]))

#Get training set files. Files 30, 31 & 32
train.subject <- read.table(unzip(temp,file = list.files[30,1]))
train.set <- read.table(unzip(temp,file = list.files[31,1]))
train.labels <- read.table(unzip(temp,file = list.files[32,1]))

#close file link when data has been gathered
unlink(temp)

#get the index of the columns that have labels containing means or std devs
cols.target.logical <- sapply(features.text[,2], function(x) grepl("mean|std",x))
cols.target <- features.text[cols.target.logical == TRUE,1] # returns vector of indices

#combine the training and test sets and give labels names using features.txt
total.set <- rbind(train.set, test.set)
names(total.set) <- features.text[,2]

#combine the activity labels for the training and test set
total.labels <- rbind(train.labels, test.labels)
names(total.labels) <- "activity"

#combine the subject labels for the training and test set
total.subject <- rbind(train.subject, test.subject)
names(total.subject) <- "subject"

#combine the subject labels, activity lables, and data set columns
total <- cbind(total.subject, total.labels, total.set)

#offset the column indicies for the total set by 2 to account for
# the subject and activity labels
cols.target.corrected <- cols.target + 2

# replaces the numbers in the activity lables with the activities names
total$activity <- activity.labels[total$activity,2]

# take the data set and get the activity and subject labels along with the 
# targetted measurements (mean and std dev)
total.target <- total[,c(1,2,cols.target.corrected)]
total.target.names <- names(total.target)

#clean up the names of the data set
total.target.names <- gsub("\\()", "",total.names)    #remove parentheses
total.target.names <- gsub("-", ".",total.names)      #replace - with .  
total.target.names <- gsub("^t", "time.",total.names) #replace t with time
total.target.names <- gsub("^f", "freq.",total.names) #replace f with freq

#change the names according to the actions above
names(total.target) <- total.target.names 

#The complete tidy data set is now stored as total.target
#Create summary table with means for each subject for each activity 
summary <- aggregate(.~activity + subject, data = total.target, mean)

