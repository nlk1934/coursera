# Assignment Requirments:
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, 
#    independent tidy data set with the average of each variable for each activity and each subject.

library(RSQLite)
library(tidyr)
library(dplyr)



##########################################################################
# Assignment Requirment 1: 
# Merges the training and the test sets to create one data set.
##########################################################################
# read the files to lines
test_data <- readLines("test/X_test.txt")
test_data <- readLines("test/X_test.txt")
# trim leading and tailing spaces
test_data <- gsub("^\\s+|\\s+$", "", test_data)
# replace middle spaces with comma
test_data <- gsub("\\s+", ",", test_data)

#read colnames, triming, cleaning
col_name <- readLines("features.txt")
col_name <- gsub("\\(|\\)|-|,","_", col_name) #replace "(", ")", "-" with "_"
col_name <- gsub("_+","_", col_name) # replace duplacate "_" with single "_"
col_name <- gsub("_$","", col_name) # remove any tailing "_"
col_name <- read.table(text=col_name, header=FALSE, sep=" ")
col_name <- t(col_name[2])

test_data <- read.csv(text=test_data, header = FALSE, col.names=col_name)

#get the lables and subject columns
test_lable <- read.table("test/y_test.txt", col.names="activity_id", colClasses = "character")
test_subject <- read.table("test/subject_test.txt", col.names="subject")

#put test data together
test_data <- cbind(test_lable, test_subject, test_data)

################## do the same to train data ################################
train_data <- readLines("train/X_train.txt")
train_data <- gsub("^\\s+|\\s+$", "", train_data)
train_data <- gsub("\\s+", ",", train_data)
train_data <- read.csv(text=train_data, header = FALSE, col.names=col_name)
train_lable <- read.table("train/y_train.txt", col.names="activity_id", colClasses = "character")
train_subject <- read.table("train/subject_train.txt", col.names="subject")
train_data <- cbind(train_lable, train_subject, train_data)
############################################################################

###put it all together
tbl <- rbind(test_data,train_data)
rm(test_data, train_data)

##########################################################################
# Assignment Requirment 2: 
# Extracts only the measurements on the 
# mean and standard deviation for each measurement.
##########################################################################
col_name <- col_name[grepl("(?i)std|mean", col_name)]
tbl <- tbl[,c("activity_id","subject",col_name)]
# a deplyr tbl %>% select(contrains()) is also possible

##########################################################################
# Assignment Requirment 3&4
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
##########################################################################


activity_tbl <- read.table("activity_labels.txt", 
        sep=" ", colClasses = c("character","character"), 
        col.names=c("id","activity")) 
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "data_tbl", tbl)
dbWriteTable(con, "activity_tbl", activity_tbl)
tbl <- dbGetQuery(con, 
        "SELECT * FROM activity_tbl, data_tbl 
                WHERE data_tbl.activity_id = activity_tbl.id")
dbDisconnect(con)

##########################################################################
# Assignment Requirment 5
# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable 
# for each activity and each subject.
##########################################################################
tbl <- tbl %>%
        select( -activity_id, -id) %>%
        gather(variablename, result, -activity, -subject) %>%
        group_by(variablename, activity, subject) %>%
        summarize(average = mean(result))

write.table(tbl, file="datasets.txt", row.name=FALSE) 

        
    

