## This script analyzes the data available here:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## the output is a dataset with the mean and standard deviation of the data collected in the study
## and a second dataset with the average of each variable for each activity and subject.
## Before running the script it is necessary to download and unzip the dataset and to modify the first line
## to direct R to the appropriate folder.
## Notice that the dataset includes additional files that are not used in the script, but can be read, if needed,
## by uncommenting the green lines below.

# change to the directory where you unzipped the files!!
setwd("C:\\Users\\gpergol2\\OneDrive - Università degli Studi di Bari\\github\\datasciencecoursera\\Getting and cleaning data\\UCI HAR Dataset")
library(tidyverse) # this package includes dplyr and some additional functions

# introducing filenames and loading files
ISM <- "Inertial Signals"
#ISMeasures <- c("body_acc_x", "body_acc_y", "body_acc_z")  #  these lines serve to import additional datasets not needed for the assignment
#ISMeasures[4:6] <- c("body_gyro_x", "body_gyro_y", "body_gyro_z")
#ISMeasures[7:9] <- c("total_acc_x", "total_acc_y", "total_acc_z")
condition <- c("train", "test")
vars <- c("subject", "X", "y")
activity_labels <- read.delim("activity_labels.txt", sep = " ", header = FALSE)
features <- read.delim("features.txt", sep = " ", header = FALSE)

  for (i in seq_along(condition)) { # these loops are used with the variables above to read the files
    
    for (ii in seq_along(vars)) {
      txt_name <- paste(vars[ii], condition[i], sep = "_")

      if (ii != 2) {
        assign(txt_name, read.table(paste(getwd(), "\\", condition[i], "\\", txt_name, ".txt" , sep = ""), header = FALSE))
      }
      else {
        assign(txt_name, read.table(paste(getwd(), "\\", condition[i], "\\", txt_name, ".txt" , sep = ""), header = FALSE, col.names = features[,2]))

      }
    }
    # The following lines are also optional for additional data import
    #for (ii in seq_along(ISMeasures)) {
    #  txt_name <- paste(ISMeasures[ii], condition[i], sep = "_")
    #  assign(txt_name, read.table(paste(getwd(), "\\", condition[i], "\\", ISM,"\\", txt_name, ".txt" , sep = ""), header = FALSE))
    #}

  }



# these two lines merge the data to create a tidy dataset
TRAIN <- add_column(X_train, "Subject" = subject_train[[1]], "Activity" = y_train[[1]], "Group" = rep("TRAIN", length.out = dim(X_train)[1]), .before = 1)
TEST <- add_column(X_test, "Subject" = subject_test[[1]], "Activity" = y_test[[1]], "Group" = rep("TEST", length.out = dim(X_test)[1]), .before = 1)

# This pipeline transforms the dataset to have a single dataframe with training an test data,
# extracts the measurements on mean and standard deviation, labels the activities measured, and relabels the variables.
TIDY <- rbind(TRAIN, TEST) %>%
  select(Subject, Activity, Group, contains("mean", ignore.case = TRUE), contains("std", ignore.case = TRUE)) %>%
  arrange(Subject, Activity) %>%
  mutate(Activity = case_when(Activity == activity_labels[1,1] ~ activity_labels[1,2],
                              Activity == activity_labels[2,1] ~ activity_labels[2,2],
                              Activity == activity_labels[3,1] ~ activity_labels[3,2],
                              Activity == activity_labels[4,1] ~ activity_labels[4,2], 
                              Activity == activity_labels[5,1] ~ activity_labels[5,2],  
                              Activity == activity_labels[6,1] ~ activity_labels[6,2])) %>%
  group_by(Subject, Activity, Group)
  
# This line creates a second dataset with data averaged by subject and activity
TIDY_mean <- summarise_at(TIDY, vars(4:dim(TIDY)[2]), mean) 

# The following line must be commented to have a look at the intermediate files
rm(list = ls()[grep("TIDY", ls(), invert = TRUE)])

# This line prints the final dataset
write.table(TIDY_mean, file = "TIDY_mean.txt", row.names = FALSE)