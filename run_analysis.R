#0-1. First import all the files for the testing sets
setwd("C:/Coursera/Data_Science/Getting_and_Cleaning_Data/GCData_Project/UCI_HAR_Dataset/test")
#0-1a. Import subject_test.txt
subject_test <- read.table("subject_test.txt",col.names="ID")
#0-1b. Import X_test.txt
xtest <- read.table("X_test.txt",header=F,nrows=2947,quote="e")
#0-1c. Import y_test.txt
ytest <- read.table("y_test.txt",header=F,nrows=2947)

#0-2. Import all the data for the training set
setwd("C:/Coursera/Data_Science/Getting_and_Cleaning_Data/GCData_Project/UCI_HAR_Dataset/train")
#0-2a. Import subject_train.txt
subject_train <- read.table("subject_train.txt",col.names="ID")
#0-2b. Import X_train.txt
xtrain <- read.table("X_train.txt",header=F,nrows=7352,quote="e")
#0-2c. Import y_train.txt
ytrain <- read.table("y_train.txt",header=F,nrows=7352)

#0-3. Read features.txt (This is the column names)
setwd("C:/Coursera/Data_Science/Getting_and_Cleaning_Data/GCData_Project/UCI_HAR_Dataset")
library(data.table)
features <- fread("features.txt")

#0-4. Read activity_labels
activity_labels <- fread("activity_labels.txt")
setnames(activity_labels,old=c("V1","V2"),new=c("ID","activity_label"))

#1. Now we have to slap the file together using the chart in the discussion
  #forums as a guide

#First I'll combine the subject, X, and y testing files
testFile <- cbind(subject_test,xtest,ytest)

#Then to do the same with the training files
trainFile <- cbind(subject_train,xtrain,ytrain)

#Then to combine those into one file with rbind
finalFile <- rbind(trainFile,testFile)

#And now to rename the columns using the column names in the second row
  #of the features file and manually naming the subject (1st) and activity (last)
names(finalFile) <- c("subject",features$V2,"activity")

#The data is all in one place, but let's also merge in the activity labels
finalFile <- merge(finalFile,activity_labels,by.x="activity",by.y="ID")

#2. Filter down the giant data set into only the mean and standard deviation columns

#Make a list of the column names (excluding subject, activity, and activity_label)
myNames <- names(finalFile[1,3:563])
#Use a regular expression to tell me which contain the literal string "mean"
myMeanCols <- regexpr("mean",myNames,ignore.case=T) != -1
#Use a regular expression to tell me which contain the literal string "std"
mySDCols <- regexpr("std",myNames,ignore.case=T) != -1
#Now make a list where either is true
myCols <- myMeanCols | mySDCols

#This resulting T/F vector myCols tells us which of the data columns to keep.

#Now filter the finalFile based on myCols, keeping also the activity, activity_label,
  #and the subject
filterFile <- finalFile[,c(T,T,myCols,T)]
#Now I'll use this cool make.names() function to make better names for the columns
n <- names(filterFile)
n2 <- make.names(n,unique=T)
setnames(filterFile,old=n,new=n2)

#3. Now use aggregate to average means and sd's by activity and subject
aggFile <- aggregate(filterFile[,3:88],
                     by=list(activity=filterFile$activity,
                             activity_label=filterFile$activity_label,
                             subject=filterFile$subject),
                     mean)
#Now rename the data columns with prefix "meanOf_"
n <- names(aggFile)
n2 <- sapply(n[4:89],function(elt) paste("meanOf_",elt,sep=""))
setnames(aggFile,old=n,new=c(n[1:3],n2))
#The resulting file has 180 rows: 6 activities for each of 30 subjects.
#Mission complete. [sunglasses emoji]

#Still need to write the file and add a codebook

write.table(aggFile,"tidyData.txt",row.name=F)
codeBookFields <- names(aggFile)
codeBookBlah <- sapply(n[4:89],function(elt) paste("The average (mean) of the data field",elt,"for the subject and activity."))
#I feel like a super pro using sapply with an anonymous function. [another sunglasses emoji]
codeBookDescriptions <- c("The numeric value of the activity the subject performed while engaging in the activity",
                          "An English description of the activity",
                          "The numeric identifier of the subject (1-30)",
                          codeBookBlah)
codeBook2 <- paste(codeBookFields,codeBookDescriptions,sep=": ")
codeBook2 <- c("This is a narrow data set that calculates the mean of various variables from a Samsung Galaxy II.",
               "This file contains summarized data for both a test and train data set.",
               "No other transformations or calculations were performed",
               "Please see features_info.txt for descriptions of the underlying data.",
               "================================================",
               codeBook2)
write.table(codeBook2,"codeBook.md",row.name=F,col.name=F)