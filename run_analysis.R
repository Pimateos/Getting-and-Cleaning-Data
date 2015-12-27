extracted.features <- c(1, 2, 3, 4,   5,   6,   7,   8,  43,  44,  45,  46,  47,  48,  83,  84,  85,  86,  87,  88, 123, 124, 125,
                        126, 127, 128, 163, 164, 165, 166, 167, 168, 203, 204, 216, 217, 229, 230, 242, 243, 255, 256, 268, 269,
                        270, 271, 272, 273, 347, 348, 349, 350, 351, 352, 426, 427, 428, 429, 430, 431, 505, 506,
 518, 519, 531, 532, 544, 545)


extracted.features.names <- c("subject_id","activity_id","tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-stlad()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")

 

download_data<-function()
{
  if(!file.exists("./data/Dataset.zip"))
  {
  dir.create("./data")
  fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, "./data/Dataset.zip")
  unzip("./data/Dataset.zip")
  }
}



merge_test_train<-function()
{
   
 ## Load data
 
  subjects.train<-read.table("./UCI HAR Dataset/train/subject_train.txt") 
  colnames(subjects.train) <- "subject_id" 
   datos.train<-read.table("./UCI HAR Dataset/train/X_train.txt")
   activity.train<-read.table("./UCI HAR Dataset/train/Y_train.txt")
   colnames(activity.train)<-"activity_id"
   datosfinal.train<-cbind(subjects.train, activity.train, datos.train)
   
   subjects.test<-read.table("./UCI HAR Dataset/test/subject_test.txt") 
   colnames(subjects.test) <- "subject_id" 
   
   datos.test<-read.table("./UCI HAR Dataset/test/X_test.txt")
   activity.test<-read.table("./UCI HAR Dataset/test/Y_test.txt")
   colnames(activity.test)<-"activity_id"
   datosfinal.test<-cbind(subjects.test, activity.test, datos.test)
   clean.data<-rbind(datosfinal.train, datosfinal.test)
    clean.data
}

extract_mean_std <- function(datos)
{
  
  #4. Appropriately labels the data set with descriptive variable names.
  
 meanstd.data<-datos[extracted.features]
 names(meanstd.data) <-extracted.features.names
 clean.data<-meanstd.data
}

activity_names <- function(datos)
{
  
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",col.names=c("activity_id","activity_name"))
  descrnames <- merge(activity_labels,datos,by.x="activity_id",by.y="activity_id",all=TRUE)
}

##1. Merges the training and the test sets to create one data set.
download_data()
testtrain.data<-merge_test_train()
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
meanstd.data<-extract_mean_std(testtrain.data)
##3. Uses descriptive activity names to name the activities in the data set
act.data<-activity_names(meanstd.data)
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each
#variable for each activity and each subject.
data_melt <- melt(act.data,id=c("activity_id","activity_name","subject_id"))
datos.final <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)
write.table(datos.final,"./cleaned_data.txt",row.names=FALSE)
