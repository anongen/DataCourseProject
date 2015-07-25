# course project

## arrange files 
#setInternet2(use = TRUE)
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="for_project.zip")
#unzip("for_project.zip")

## assume the Samsung data is in your working directory

## upload data from the files that were extracted:
# 30 volunteers (21 training + 9 test)
# Each person performed six activities:
# WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING

X_train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
#size 7352x561
y_train_data <- read.table("./UCI HAR Dataset/train/y_train.txt")
#size 7352x1, numbers 1-6
subject_train_data <- read.table("./UCI HAR Dataset/train/subject_train.txt")
#size 7352x1, numbers 1-30 (not all)
X_test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
#size  2947x561
y_test_data <- read.table("./UCI HAR Dataset/test/y_test.txt")
#size 2947x1, numbers 1-6
subject_test_data <- read.table("./UCI HAR Dataset/test/subject_test.txt")
#size 2947x1, numbers 1-30 (not all)
features_data <- read.table("./UCI HAR Dataset/features.txt")

## combine columns for train and test data seperately
train_data <- cbind(subject_train_data,y_train_data,X_train_data)
test_data <- cbind(subject_test_data,y_test_data,X_test_data)

## combine rows from train and test data
all_data <- rbind(train_data,test_data)
#dim(all_data): 10299   563

## add the name of the columns
the_features <- as.vector(features_data[,2])
names(all_data) <- c("subject","activity", the_features)


#we were asked for: Extracts only the measurements on 
#the mean and standard deviation for each measurement

#after considering features info and comparing to the features names,
#would like to extract the following values:
#tBodyAcc-mean()-XYZ
#tBodyAcc-std()-XYZ
#tGravityAcc-mean()-XYZ
#tGravityAcc-std()-XYZ
#tBodyAccJerk-mean()-XYZ
#tBodyAccJerk-std()-XYZ
#tBodyGyro-mean()-XYZ
#tBodyGyro-std()-XYZ
#tBodyGyroJerk-mean()-XYZ
#tBodyGyroJerk-std()-XYZ
#tBodyAccMag-mean()
#tBodyAccMag-std()
#tGravityAccMag-mean()
#tGravityAccMag-std()
#tBodyAccJerkMag-mean()
#tBodyAccJerkMag-std()
#tBodyGyroMag-mean()
#tBodyGyroMag-std()
#tBodyGyroJerkMag-mean()
#tBodyGyroJerkMag-std()
#fBodyAcc-mean()-XYZ
#fBodyAcc-std()-XYZ
#fBodyAccJerk-mean()-XYZ
#fBodyAccJerk-std()-XYZ
#fBodyGyro-mean()-XYZ
#fBodyGyro-std()-XYZ
#fBodyAccMag-mean()
#fBodyAccMag-std()
#fBodyBodyAccJerkMag-mean()
#fBodyBodyAccJerkMag-std()
#fBodyBodyGyroMag-mean()
#fBodyBodyGyroMag-std()
#fBodyBodyGyroJerkMag-mean()
#fBodyBodyGyroJerkMag-std()

## extract the wanted columns:
mean_ind <- grep("mean()",the_features,fixed=TRUE)
std_ind <- grep("std()",the_features,fixed=TRUE)
all_ind <- c(mean_ind,std_ind)
select_data <- all_data[,c(1,2,all_ind[order(all_ind)]+2)]
#dim(select_data): 10299 68

## create tidy data set with the average of each variable 
## for each activity and each subject
almost_tidy_data <- aggregate(select_data,list(select_data[,"activity"],select_data[,"subject"]),mean)

#two new columns were added: Group.1 and Group.2 
#In order to fix this:
almost_tidy_data <- almost_tidy_data[,c(3:70)]
#dim(almost_tidy_data) : 180 68

## change the activity label to meaningful name 
tidy_data <- almost_tidy_data 
for (i in 1:180){tidy_data[i,"activity"] <- 
               if (tidy_data[i,"activity"]==1) {
                     "WALKING"
                  } else if (tidy_data[i,"activity"]==2) {
                     "WALKING UPSTAIRS"
                  } else if (tidy_data[i,"activity"]==3) {
                     "WALKING_DOWNSTAIRS" 
                  } else if (tidy_data[i,"activity"]==4) {
                     "SITTING" 
                  } else if (tidy_data[i,"activity"]==5){
                     "STANDING" 
                  } else {
                     "LAYING" 
                  }
               }
               

## create txt file with the tidy data
write.table(tidy_data,"tidy_data.txt",row.name=FALSE)