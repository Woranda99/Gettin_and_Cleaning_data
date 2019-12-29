library(dplyr) 

# train data 
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# test data 
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# features
features <- read.table("./features.txt") 

# activity labels 
activity_labels <- read.table("./activity_labels.txt") 

# merge
x_total   <- rbind(x_train, x_test)
y_total   <- rbind(y_train, y_test) 
sub_total <- rbind(sub_train, sub_test) 

# Extracts only the measurements on the mean and standard deviation for each measurement.
sel_features <- features [grep(".*mean\\(\\)|std\\(\\)", features[,2], ignore.case = FALSE),]
x_total      <- x_total[,sel_features[,1]]

# Uses descriptive activity names to name the activities in the data set
colnames(y_total)   <- "activity"
colnames(sub_total) <- "subject"
total <- cbind(sub_total, y_total, x_total)

total$activity <- factor(total$activity, levels = activity_labels[,1], labels = activity_labels[,2]) 
total$subject  <- as.factor(total$subject) 

# Appropriately labels the data set with descriptive variable names.
colnames(x_total)   <- sel_features[,2]

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
total_mean <- total %>% group_by(activity, subject) %>% summarize_all(funs(mean)) 
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE) 