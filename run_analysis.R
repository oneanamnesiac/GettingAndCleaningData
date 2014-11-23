run_analysis <- function() {
   
   # Dataset
   # https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
   
   # Dataset description
   # http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
   
   # Local Directory Structure
   # "workingDirectory"/UCI HAR Dataset
   
   # Load Libraries
   library(dplyr)
   library(tidyr)
   
   # 1 Merges the training and the test sets to create one data set.
   # 1 Part 1 of 2
      
   X_total <- rbind(read.table("UCI HAR Dataset\\train\\X_train.txt"),
                    read.table("UCI HAR Dataset\\test\\X_test.txt"))
      
   y_total <- rbind(read.table("UCI HAR Dataset\\train\\y_train.txt"),
                   read.table("UCI HAR Dataset\\test\\y_test.txt"))
   
   subject_total <- rbind(read.table("UCI HAR Dataset\\train\\subject_train.txt"),
                          read.table("UCI HAR Dataset\\test\\subject_test.txt"))
   
   
   # 2 Extract only the measurements on the mean and standard deviation for each measurement.
      
   # Set column names using the features.txt file
   features <- read.table("UCI HAR Dataset\\features.txt")
   colnames(X_total) <- features[,"V2"]
   rm("features")
   
   # Find column indexes for "mean()" and "std()" measurement.
   colsWanted <- c(grep("mean()", names(X_total), fixed = T),
                   grep("std()", names(X_total), fixed = T))
   colsWanted <- sort(colsWanted)
     # note: used grep (rather use select()) due to duplicate feature names.

   # Select only "mean()" and "std()" measurement variables from X_total.
   X_total <- X_total[,colsWanted]
   rm("colsWanted")

   
   # 3 Uses descriptive activity names to name the activities in the data set
   
   # Load activity labels from file.
   activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt")
   
   # y_total contains the activity type per test.  Rename column name to be descriptive.
   colnames(y_total) <- "activity"
   
   # replace activity ID with descriptive name in y_total table using activity_labels table.
   y_total$activity <- activity_labels[y_total[,"activity"],"V2"]
   
   rm("activity_labels")


   # 4 Appropriately labels the data set with descriptive variable names. 
   
   # "X" table variable names were set in Section 2 using the features.txt file.
   # "y" table variable name was set in Section 3 as "activity".
   # Set "subject" table variable name set as "subject"
   colnames(subject_total) <- "subject"
   
   
   # 1 Merges the training and the test sets to create one data set.
   # 1 Part 2 of 2

   # Combine "X", "y", and "subject" tables into a single data set.
   dataset <- cbind(subject_total, y_total, X_total)
   rm(list = c("subject_total", "y_total", "X_total"))
   

   # 5 From the data set in step 4, creates a second, independent tidy data set with the
   #   average of each variable for each activity and each subject.
   
   # Gather all features variables into a "feature, value" pair of variables.
   dataset <- gather(dataset, feature, value, -subject, -activity)
   
   # Group by subject, activity, and feature (aka variable).
   # Return the average value for the grouped variables.
   dataset <- group_by(dataset, subject, activity, feature) %>%      
      summarize(averageValue = mean(value))
   
   return(dataset)
   
}