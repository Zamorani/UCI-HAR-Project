# UCI-HAR-Project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 

########################################################################################################################################
## AUTHOR: Nick Zamora                                                                                                                  
## DATE: 2018-07-11     
## Data and Other Files From:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## Instructions:                                                                                                                        
## 1. Merges the training and the test sets to create one data set.                                                                     
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.                                           
## 3. Uses descriptive activity names to name the activities in the data set                                                           
## 4. Appropriately labels the data set with descriptive variable names.                                                              
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
########################################################################################################################################

##LOAD LIBRARIES
library(reshape2)
library(dplyr)

## MANIPULATE THE PATH TO THE FOLDER MIMICKING THE STRUCTURE OF THE ZIP FOLDER
path_to_UCI<-"C:/Users/nickz/Desktop/DataTNT.Campbell/UCI HAR Dataset"

## DON'T CHANGE ANYTHING ELSE ##

## SET THE WORKING DIRECTORY
setwd(path_to_UCI)

## READ IN FEATURES DATA SET
features<-read.table("features.txt")

## CREATE LIST OF VARIABLE NAMES CONTAIN MEAN OR STD
variablelist<-features$V2[grepl("mean\\(\\)|std\\(\\)",features$V2)==TRUE]

## READ IN ACTIVITY LABELS DATA SET
activity_labels<-read.table("activity_labels.txt")

## RENAME NONDESCRIPT VARIABLE NAMES IN ACTIVITY LABELS DATA SET
names(activity_labels)<-c("actlabelnum","activitylabel")

## READ IN SUBJECT TEST DATA
subject_test<-read.table("./test/subject_test.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN SUBJECT TEST DATA
names(subject_test)<-"subjectID"

## CREATE DATASET VARIABLE
subject_test$dataset="TEST"

## READ IN X TEST DATA
x_test<-read.table("./test/x_test.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN SUBJECT TEST DATA
names(x_test)<-features$V2

## KEEP ONLY MEAN AND STD VARIABLES IN X TEST DATA
x_test<-x_test[paste(variablelist,sep=",")]

## READ IN Y TEST DATA
y_test<-read.table("./test/y_test.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN Y TEST DATA
names(y_test)<-"actlabelnum"

## BODY ACCELEROMETER X TEST DATA SET  
body_acc_x_test<-read.table('./test/Inertial Signals/body_acc_x_test.txt')      

## RENAME BODY ACCELEROMETER X TEST DATA SET VARIABLES
names(body_acc_x_test)<-paste("bodyaccx",1:128,sep="")       

## BODY ACCELEROMETER Y TEST DATA SET  
body_acc_y_test<-read.table('./test/Inertial Signals/body_acc_y_test.txt')      

## RENAME BODY ACCELEROMETER Y TEST DATA SET VARIABLES
names(body_acc_y_test)<-paste("bodyaccy",1:128,sep="")       

## BODY ACCELEROMETER Z TEST DATA SET  
body_acc_z_test<-read.table('./test/Inertial Signals/body_acc_z_test.txt')      

## RENAME BODY ACCELEROMETER Z TEST DATA SET VARIABLES
names(body_acc_z_test)<-paste("bodyaccz",1:128,sep="")       

## BODY GYROSCOPE X TEST DATA SET  
body_gyro_x_test<-read.table('./test/Inertial Signals/body_gyro_x_test.txt')      

## RENAME BODY GYROSCOPE X TEST DATA SET VARIABLES
names(body_gyro_x_test)<-paste("bodygyrox",1:128,sep="")       

## BODY GYROSCOPE Y TEST DATA SET  
body_gyro_y_test<-read.table('./test/Inertial Signals/body_gyro_y_test.txt')      

## RENAME BODY GYROSCOPE Y TEST DATA SET VARIABLES
names(body_gyro_y_test)<-paste("bodygyroy",1:128,sep="")       

## BODY GYROSCOPE Z TEST DATA SET  
body_gyro_z_test<-read.table('./test/Inertial Signals/body_gyro_z_test.txt')      

## RENAME BODY GYROSCOPE Z TEST DATA SET VARIABLES
names(body_gyro_z_test)<-paste("bodygyroz",1:128,sep="")       

## TOTAL ACCELEROMETER X TEST DATA SET  
total_acc_x_test<-read.table('./test/Inertial Signals/total_acc_x_test.txt')      

## RENAME TOTAL ACCELEROMETER X TEST DATA SET VARIABLES
names(total_acc_x_test)<-paste("totalaccx",1:128,sep="")       

## TOTAL ACCELEROMETER Y TEST DATA SET  
total_acc_y_test<-read.table('./test/Inertial Signals/total_acc_y_test.txt')      

## RENAME TOTAL ACCELEROMETER Y TEST DATA SET VARIABLES
names(total_acc_y_test)<-paste("totalaccy",1:128,sep="")       

## TOTAL ACCELEROMETER Z TEST DATA SET  
total_acc_z_test<-read.table('./test/Inertial Signals/total_acc_z_test.txt')      

## RENAME TOTAL ACCELEROMETER Z TEST DATA SET VARIABLES
names(total_acc_z_test)<-paste("totalaccz",1:128,sep="")       

## READ IN SUBJECT TRAIN DATA
subject_train<-read.table("./train/subject_train.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN SUBJECT TRAIN DATA
names(subject_train)<-"subjectID"

## CREATE DATASET VARIABLE
subject_train$dataset="TRAIN"

## READ IN X train DATA
x_train<-read.table("./train/x_train.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN SUBJECT TRAIN DATA
names(x_train)<-features$V2

## KEEP ONLY MEAN AND STD VARIABLES IN X TRAIN DATA
x_train<-x_train[paste(variablelist,sep=",")]

## READ IN Y TRAIN DATA
y_train<-read.table("./train/y_train.txt")

## RENAME NONDESCRIPT VARIABLE NAME IN Y TRAIN DATA
names(y_train)<-"actlabelnum"

## BODY ACCELEROMETER X TRAIN DATA SET  
body_acc_x_train<-read.table('./train/Inertial Signals/body_acc_x_train.txt')      

## RENAME BODY ACCELEROMETER X TRAIN DATA SET VARIABLES
names(body_acc_x_train)<-paste("bodyaccx",1:128,sep="")       

## BODY ACCELEROMETER Y TRAIN DATA SET  
body_acc_y_train<-read.table('./train/Inertial Signals/body_acc_y_train.txt')      

## RENAME BODY ACCELEROMETER Y TRAIN DATA SET VARIABLES
names(body_acc_y_train)<-paste("bodyaccy",1:128,sep="")       

## BODY ACCELEROMETER Z TRAIN DATA SET  
body_acc_z_train<-read.table('./train/Inertial Signals/body_acc_z_train.txt')      

## RENAME BODY ACCELEROMETER Z TRAIN DATA SET VARIABLES
names(body_acc_z_train)<-paste("bodyaccz",1:128,sep="")       

## BODY GYROSCOPE X TRAIN DATA SET  
body_gyro_x_train<-read.table('./train/Inertial Signals/body_gyro_x_train.txt')      

## RENAME BODY GYROSCOPE X TRAIN DATA SET VARIABLES
names(body_gyro_x_train)<-paste("bodygyrox",1:128,sep="")       

## BODY GYROSCOPE Y TRAIN DATA SET  
body_gyro_y_train<-read.table('./train/Inertial Signals/body_gyro_y_train.txt')      

## RENAME BODY GYROSCOPE Y TRAIN DATA SET VARIABLES
names(body_gyro_y_train)<-paste("bodygyroy",1:128,sep="")       

## BODY GYROSCOPE Z TRAIN DATA SET  
body_gyro_z_train<-read.table('./train/Inertial Signals/body_gyro_z_train.txt')      

## RENAME BODY GYROSCOPE Z TRAIN DATA SET VARIABLES
names(body_gyro_z_train)<-paste("bodygyroz",1:128,sep="")       

## TOTAL ACCELEROMETER X TRAIN DATA SET  
total_acc_x_train<-read.table('./train/Inertial Signals/total_acc_x_train.txt')      

## RENAME TOTAL ACCELEROMETER X TRAIN DATA SET VARIABLES
names(total_acc_x_train)<-paste("totalaccx",1:128,sep="")       

## TOTAL ACCELEROMETER Y TRAIN DATA SET  
total_acc_y_train<-read.table('./train/Inertial Signals/total_acc_y_train.txt')      

## RENAME TOTAL ACCELEROMETER Y TRAIN DATA SET VARIABLES
names(total_acc_y_train)<-paste("totalaccy",1:128,sep="")       

## TOTAL ACCELEROMETER Z TRAIN DATA SET  
total_acc_z_train<-read.table('./train/Inertial Signals/total_acc_z_train.txt')      

## RENAME TOTAL ACCELEROMETER Z TRAIN DATA SET VARIABLES
names(total_acc_z_train)<-paste("totalaccz",1:128,sep="")       

## BIND DATA TOGETHER
all<-rbind(cbind(subject_train,x_train,y_train),cbind(subject_test,x_test,y_test))

## TAKE OUT SPECIAL CHARACTERS FROM VARIABLE NAMES
names(all)<-tolower(gsub("[^0-9A-Za-z]","",names(all)))

## MAKE DATASET VARIABLE A FACTOR VARIABLE
all$dataset<-as.factor(all$dataset)

## GET ACTIVITY LABELS
finaldata<-merge(all,activity_labels)

## KILL ACTIVITY LABEL SINCE APPARENTLY AGGREGATE FUNCTION HATES IT
finaldata2<-finaldata
finaldata2$activitylabel<-NULL

## KILL ACTIVITY LABEL NUMBERS SINCE DESCRIPTIVE LABEL EXISTS
finaldata3<-finaldata
finaldata3$actlabelnum<-NULL

## SUMMARIZE FINAL DATA SET WITH MEAN OF EACH VARIABLE BY SUBJECT AND ACTIVITY
sumdata = aggregate(finaldata2[,names(finaldata2) != c('subjectid','actlabelnum','dataset')],by=list(subjectid=finaldata2$subjectid,actlabelnum=finaldata2$actlabelnum),mean,na.rm=TRUE)

## KILL REDUNDANT FIELDS
sumdata<-sumdata[,3:70]

## GET ACTIVITY LABELS BACK
finalsumdata<-merge(sumdata,activity_labels)

## REORDER SUMMARY DATASET
finalfinalsumdata<-select(finalsumdata,activitylabel, subjectid,tbodyaccmeanx,tbodyaccmeany,tbodyaccmeanz,tbodyaccstdx,tbodyaccstdy,tbodyaccstdz,tgravityaccmeanx,tgravityaccmeany,tgravityaccmeanz,tgravityaccstdx,tgravityaccstdy,tgravityaccstdz,
tbodyaccjerkmeanx,tbodyaccjerkmeany,tbodyaccjerkmeanz,tbodyaccjerkstdx,tbodyaccjerkstdy,tbodyaccjerkstdz,tbodygyromeanx,tbodygyromeany,tbodygyromeanz,tbodygyrostdx,tbodygyrostdy,tbodygyrostdz,
tbodygyrojerkmeanx,tbodygyrojerkmeany,tbodygyrojerkmeanz,tbodygyrojerkstdx,tbodygyrojerkstdy,tbodygyrojerkstdz,tbodyaccmagmean,tbodyaccmagstd,tgravityaccmagmean,tgravityaccmagstd,
tbodyaccjerkmagmean,tbodyaccjerkmagstd,tbodygyromagmean,tbodygyromagstd,tbodygyrojerkmagmean,tbodygyrojerkmagstd,fbodyaccmeanx,fbodyaccmeany,fbodyaccmeanz,fbodyaccstdx,fbodyaccstdy,fbodyaccstdz,
fbodyaccjerkmeanx,fbodyaccjerkmeany,fbodyaccjerkmeanz,fbodyaccjerkstdx,fbodyaccjerkstdy,fbodyaccjerkstdz,fbodygyromeanx,fbodygyromeany,fbodygyromeanz,fbodygyrostdx,fbodygyrostdy,fbodygyrostdz,
fbodyaccmagmean,fbodyaccmagstd,fbodybodyaccjerkmagmean,fbodybodyaccjerkmagstd,fbodybodygyromagmean,fbodybodygyromagstd,fbodybodygyrojerkmagmean,fbodybodygyrojerkmagstd)


## WRITE OUT FILES
write.csv(finaldata3,"C:/Users/nickz/Desktop/DataTNT.Campbell/tidydata.csv")
write.table(finalfinalsumdata,"C:/Users/nickz/Desktop/DataTNT.Campbell/campbell_tidysumdata.txt",row.name=FALSE)

## CREATE DATA DICTIONARY
sink(file='C:/Users/nickz/Desktop/DataTNT.Campbell/datadictionary.txt')
cat("FULL DATA SET")
str(finaldata,list.len=ncol(finaldata),vec.len=3)
cat("\n")
cat("\n")
cat("SUMMARIZED DATA SET")
str(finalfinalsumdata,list.len=ncol(finaldata),vec.len=3)
unlink('C:/Users/nickz/Desktop/DataTNT.Campbell/datadictionary.txt')
