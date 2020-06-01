# Coursera Getting and Cleaning Data Course Project 
library(data.table)

setwd("C:/Users/ASUS/Getting-Cleaning-Data-Course-Project")

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( url, destfile = "data.zip" )
unzip("data.zip")

# list.files(), reset the working directory
setwd("C:/Users/ASUS/Getting-Cleaning-Data-Course-Project/UCI HAR Dataset")

# Files that will be used includes the following
# test/subject_test.txt  , test/X_test.txt  , test/y_test.txt
# train/subject_train.txt, train/X_train.txt, train/y_train.txt
# exclude the file Inertial Signals
trainfile <- list.files( "train", full.names = TRUE )[-1]
testfile  <- list.files( "test" , full.names = TRUE )[-1]

# Read in all six files
file <- c( trainfile, testfile )
data <- lapply( file, read.table, stringsAsFactors = FALSE, header = FALSE )


# ---------------------------------------------------------------------
# Step 1 : Merges the training and the test sets to create one data set
# rbind the train and test data by each variable
data1 <- mapply ( rbind, data[ c(1:3) ], data[ c(4:6) ] )

# data2: the whole single dataset
# column 1 = subject, column 2~562 = feature,  column 563 = activity
data2 <- do.call( cbind, data1 )


# ----------------------------------------------------------------------
# Step 2 : For the feature column, extracts only the measurements on the 
# mean and standard deviation for each measurement

# match it using features.txt(second file in list.file() )
# featurename is in the second column(V2)
featurenames <- fread( list.files()[2], header = FALSE, stringsAsFactor = FALSE )

# set the column names for data2, does the task required in 
# Step 4 : Appropriately labels the data set with descriptive variable names.
setnames( data2, c(1:563), c( "subject", featurenames$V2, "activity" ) )

# Extract only the column that have mean() or std() in the end
# Add 1 to it, cuz the first column in data2 is subject not feature
# Don't just use mean when doing matching, this will include meanFreq()
# Each backslash must be expressed as \\
measurements <- grep( "std|mean\\(\\)", featurenames$V2 ) + 1

# data3 : contains only the mean and standard deviation for feature column 
data3 <- data2[, c( 1, measurements, 563 ) ]


# ------------------------------------------------------------------------------
# Step 3 : Use descriptive activity names to name the activities in the data set

# match it using activity_labels.txt(first file in list.file() )
activitynames <- fread( list.files()[1], header = FALSE, stringsAsFactor = FALSE )

data3$activity <- activitynames$V2[ match( data3$activity, activitynames$V1 ) ]


# ---------------------------------------------------------------------------------
# Step 5 : From the data set in step 4, creates a second, independent tidy data set, 
# with the average of each variable for each activity and each subject.
data4 <- aggregate( . ~ subject + activity, data = data3, FUN = mean )

# write out data4
setwd("C:/Users/ASUS/Getting-Cleaning-Data-Course-Project")
write.table( data4, "averagedata.txt", row.names = FALSE )
