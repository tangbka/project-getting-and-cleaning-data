# Download and unzip the raw data.
file.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file.url,destfile="./data.zip")
unzip("./data.zip")

# Part 1 - Read the training set and test set and combine them.

trg_set <- read.table("./UCI HAR Dataset/train/X_train.txt",numerals="no.loss")
test_set <- read.table("./UCI HAR Dataset/test/X_test.txt",numerals="no.loss")
merged_set <- rbind(trg_set,test_set)


# Part 2 - Extract only the measurements for mean and standard deviation.

# Get the indices from the 561 variables that represent only mean() and std().
var_s <- read.table("./UCI HAR Dataset/features.txt")
index_mean <- grep("mean()",var_s[,2],fixed=T)
index_std <- grep("std()",var_s[,2],fixed=T)

# Combine the indices for mean() and for std()
final_index <- sort(c(index_mean,index_std))

# Use the indices to extract the measurements we want.
merged_subset <- merged_set[,final_index]

# Remove memory-heavy objects
rm(list=c("trg_set","test_set","merged_set"))


# Part 3 - Use descriptive activity names to name the activities in the data set.

act_label_code <- read.table("./UCI HAR Dataset/activity_labels.txt")
train_index <- read.table("./UCI HAR Dataset/train/y_train.txt")
test_index <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Employ the use of the dplyr package
if (!any(installed.packages() %in% "dplyr")) install.packages("dplyr")
library(dplyr)

# Append activity names to the numeric labels for each observation.
train_label <- left_join(train_index,act_label_code,by="V1")
test_label <- left_join(test_index,act_label_code,by="V1")

master_act_label <- rbind(train_label,test_label)
# Add activity as the 67th column to the merged data set.
merged_subset <- mutate(merged_subset,activity=master_act_label[,2])


# Part 4 - Label the dataset with variable names.

var_name=append(as.character(var_s[final_index,2]),"activity",after=66)
names(merged_subset) <- var_name


# Part 5 - Create a tidy data set with the average of each variable for each activity 
# and each subject.

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
master_sub_list <- rbind(subject_train,subject_test)

merged_subset$subject <- master_sub_list
names(merged_subset[,68]) <- "subject"

b <- merged_subset %>% group_by(activity,subject)
tidy_tbl <- b %>% summarise_each(funs(mean),-c(activity,subject))

# writes the tidy data set into a file names "TidyData.txt"
write.table(tidy_tbl,file="./TidyData.txt",row.name=F)
