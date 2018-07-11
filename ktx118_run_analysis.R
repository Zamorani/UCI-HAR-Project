########################################################################################################################################################
## AUTHOR: Nick Zamora                                                                                                                            ##
## DATE: 2018-07-11     

## Data and Other Files From:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Instructions:                                                                                                                                              ##
## 1. Merges the training and the test sets to create one data set.                                                                                   ##
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.                                                         ##
## 3. Uses descriptive activity names to name the activities in the data set                                                                          ##
## 4. Appropriately labels the data set with descriptive variable names.                                                                              ##
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  ##
########################################################################################################################################################

library(reshape2)
library(dplyr)

path_to_UCI<-"C:/Users/nickz/Desktop/DataTNT.Campbell/UCI HAR Dataset"
setwd(path_to_UCI)

features<-read.table("features.txt")
variablelist<-features$V2[grepl("mean\\(\\)|std\\(\\)",features$V2)==TRUE]

activity_labels<-read.table("activity_labels.txt")
names(activity_labels)<-c("actlabelnum","activitylabel")

subject_test<-read.table("./test/subject_test.txt")
names(subject_test)<-"subjectID"
subject_test$dataset="TEST"

x_test<-read.table("./test/x_test.txt")
names(x_test)<-features$V2
x_test<-x_test[paste(variablelist,sep=",")]

y_test<-read.table("./test/y_test.txt")
names(y_test)<-"actlabelnum"

body_acc_x_test<-read.table('./test/Inertial Signals/body_acc_x_test.txt')      
names(body_acc_x_test)<-paste("bodyaccx",1:128,sep="")       

body_acc_y_test<-read.table('./test/Inertial Signals/body_acc_y_test.txt')      
names(body_acc_y_test)<-paste("bodyaccy",1:128,sep="")       

body_acc_z_test<-read.table('./test/Inertial Signals/body_acc_z_test.txt')
names(body_acc_z_test)<-paste("bodyaccz",1:128,sep="")       

body_gyro_x_test<-read.table('./test/Inertial Signals/body_gyro_x_test.txt')
names(body_gyro_x_test)<-paste("bodygyrox",1:128,sep="")       
 
body_gyro_y_test<-read.table('./test/Inertial Signals/body_gyro_y_test.txt')
names(body_gyro_y_test)<-paste("bodygyroy",1:128,sep="")       

body_gyro_z_test<-read.table('./test/Inertial Signals/body_gyro_z_test.txt')
names(body_gyro_z_test)<-paste("bodygyroz",1:128,sep="")       
 
total_acc_x_test<-read.table('./test/Inertial Signals/total_acc_x_test.txt')
names(total_acc_x_test)<-paste("totalaccx",1:128,sep="")       

total_acc_y_test<-read.table('./test/Inertial Signals/total_acc_y_test.txt')
names(total_acc_y_test)<-paste("totalaccy",1:128,sep="")       
 
total_acc_z_test<-read.table('./test/Inertial Signals/total_acc_z_test.txt')
names(total_acc_z_test)<-paste("totalaccz",1:128,sep="")       

subject_train<-read.table("./train/subject_train.txt")
names(subject_train)<-"subjectID"

subject_train$dataset="TRAIN"
x_train<-read.table("./train/x_train.txt")
names(x_train)<-features$V2
x_train<-x_train[paste(variablelist,sep=",")]

y_train<-read.table("./train/y_train.txt")
names(y_train)<-"actlabelnum"
 
body_acc_x_train<-read.table('./train/Inertial Signals/body_acc_x_train.txt')
names(body_acc_x_train)<-paste("bodyaccx",1:128,sep="")       
 
body_acc_y_train<-read.table('./train/Inertial Signals/body_acc_y_train.txt')
names(body_acc_y_train)<-paste("bodyaccy",1:128,sep="")       

body_acc_z_train<-read.table('./train/Inertial Signals/body_acc_z_train.txt')
names(body_acc_z_train)<-paste("bodyaccz",1:128,sep="")       
 
body_gyro_x_train<-read.table('./train/Inertial Signals/body_gyro_x_train.txt')
names(body_gyro_x_train)<-paste("bodygyrox",1:128,sep="")       

body_gyro_y_train<-read.table('./train/Inertial Signals/body_gyro_y_train.txt')
names(body_gyro_y_train)<-paste("bodygyroy",1:128,sep="")       

body_gyro_z_train<-read.table('./train/Inertial Signals/body_gyro_z_train.txt')
names(body_gyro_z_train)<-paste("bodygyroz",1:128,sep="")       

total_acc_x_train<-read.table('./train/Inertial Signals/total_acc_x_train.txt')
names(total_acc_x_train)<-paste("totalaccx",1:128,sep="")       

total_acc_y_train<-read.table('./train/Inertial Signals/total_acc_y_train.txt')      
names(total_acc_y_train)<-paste("totalaccy",1:128,sep="")       
 
total_acc_z_train<-read.table('./train/Inertial Signals/total_acc_z_train.txt')
names(total_acc_z_train)<-paste("totalaccz",1:128,sep="")       

all<-rbind(cbind(subject_train,x_train,y_train),cbind(subject_test,x_test,y_test))
names(all)<-tolower(gsub("[^0-9A-Za-z]","",names(all)))
all$dataset<-as.factor(all$dataset)
finaldata<-merge(all,activity_labels)

finaldata2<-finaldata
finaldata2$activitylabel<-NULL
finaldata3<-finaldata
finaldata3$actlabelnum<-NULL

sumdata = aggregate(finaldata2[,names(finaldata2) != c('subjectid','actlabelnum','dataset')],by=list(subjectid=finaldata2$subjectid,actlabelnum=finaldata2$actlabelnum),mean,na.rm=TRUE)
sumdata<-sumdata[,3:70]
finalsumdata<-merge(sumdata,activity_labels)

finalfinalsumdata<-select(finalsumdata,activitylabel, subjectid,tbodyaccmeanx,tbodyaccmeany,tbodyaccmeanz,tbodyaccstdx,tbodyaccstdy,tbodyaccstdz,tgravityaccmeanx,tgravityaccmeany,tgravityaccmeanz,tgravityaccstdx,tgravityaccstdy,tgravityaccstdz,
tbodyaccjerkmeanx,tbodyaccjerkmeany,tbodyaccjerkmeanz,tbodyaccjerkstdx,tbodyaccjerkstdy,tbodyaccjerkstdz,tbodygyromeanx,tbodygyromeany,tbodygyromeanz,tbodygyrostdx,tbodygyrostdy,tbodygyrostdz,
tbodygyrojerkmeanx,tbodygyrojerkmeany,tbodygyrojerkmeanz,tbodygyrojerkstdx,tbodygyrojerkstdy,tbodygyrojerkstdz,tbodyaccmagmean,tbodyaccmagstd,tgravityaccmagmean,tgravityaccmagstd,
tbodyaccjerkmagmean,tbodyaccjerkmagstd,tbodygyromagmean,tbodygyromagstd,tbodygyrojerkmagmean,tbodygyrojerkmagstd,fbodyaccmeanx,fbodyaccmeany,fbodyaccmeanz,fbodyaccstdx,fbodyaccstdy,fbodyaccstdz,
fbodyaccjerkmeanx,fbodyaccjerkmeany,fbodyaccjerkmeanz,fbodyaccjerkstdx,fbodyaccjerkstdy,fbodyaccjerkstdz,fbodygyromeanx,fbodygyromeany,fbodygyromeanz,fbodygyrostdx,fbodygyrostdy,fbodygyrostdz,
fbodyaccmagmean,fbodyaccmagstd,fbodybodyaccjerkmagmean,fbodybodyaccjerkmagstd,fbodybodygyromagmean,fbodybodygyromagstd,fbodybodygyrojerkmagmean,fbodybodygyrojerkmagstd)

write.csv(finaldata3,"C:/Users/nickz/Desktop/DataTNT.Campbell/tidydata.csv")
write.table(finaldata3,"C:/Users/nickz/Desktop/DataTNT.Campbell/tidydata.txt",row.name=FALSE)
write.table(finalfinalsumdata,"C:/Users/nickz/Desktop/DataTNT.Campbell/ktx118_tidysumdata.txt",row.name=FALSE)

sink(file='C:/Users/nickz/Desktop/DataTNT.Campbell/datadictionary.txt')
cat("FULL DATA SET")
str(finaldata,list.len=ncol(finaldata),vec.len=3)
cat("\n")
cat("\n")
cat("SUMMARIZED DATA SET")
str(finalfinalsumdata,list.len=ncol(finaldata),vec.len=3)
unlink('C:/Users/nickz/Desktop/DataTNT.Campbell/datadictionary.txt')