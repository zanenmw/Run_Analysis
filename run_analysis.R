setwd("C:/Projecten 2018/Training data scientists/R-folder/Assignement Module 2")
#Module 2 - Getting and cleaning data: Assignement
library(dplyr)

# 1.Merges the training and the test sets to create one data set
#Readin the data
train_x <- tbl_df(read.table("./UCI HAR Dataset/train/X_train.txt"))
train_y <- tbl_df(read.table("./UCI HAR Dataset/train/y_train.txt"))
subject_train <- tbl_df(read.table("./UCI HAR Dataset/train/subject_train.txt"))
test_x <- tbl_df(read.table("./UCI HAR Dataset/test/X_test.txt"))
test_y <- tbl_df(read.table("./UCI HAR Dataset/test/y_test.txt"))
subject_test <- tbl_df(read.table("./UCI HAR Dataset/test/subject_test.txt"))
test_y <- tbl_df(read.table("./UCI HAR Dataset/test/y_test.txt"))
View(subject_test)
View(test_x)
summary(subject_test)

#Giving the y's proper names
colnames(train_y) <- c("Activity Name")
colnames(test_y) <- c("Activity Name")
colnames(subject_train) <- c("Subject")
View(subject_train)

#Reading features names
feature_names<-read.table("./UCI HAR Dataset/features.txt")
feature_names<-feature_names[,2]
activity.labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
View(activity.labels)

#Giving the training and test data proper names
colnames(train_x) <- feature_names
colnames(test_x) <- feature_names
colnames(subject_test) <- c("Subject")
labeledTrainingSet <- bind_cols(subject_train,train_x,train_y)
labeledTestSet <- bind_cols(subject_test,test_x,test_y)
labeledDataSet <- bind_rows(labeledTrainingSet,labeledTestSet)
names(labeledDataSet)

# 2. Extracts only the measurements on the mean and standard deviation for each  
# measurement 
search <- grep("-mean|-std", colnames(labeledDataSet))
data.mean.std <- labeledDataSet[,c(1,563,search)]

# 4. Combines this with the real activity name
AllData = merge(data.mean.std,activity.labels,by.x = "Activity Name",by.y= "V1",
all = TRUE)
AllData$`Activity Name`<- AllData$V2
AllData <- rename(AllData, Activity = "Activity Name")
View(AllData)
write.csv(AllData, "AllData.csv")

# 5. Create an independent tiny data set with averages for each subject for each activity

TidyData <- aggregate( AllData[,3] ~ Subject+Activity , data = AllData, FUN= "mean" )
for(i in 4:ncol(AllData)){
  TidyData[,i] <- aggregate( AllData[,i] ~ Subject+Activity, data = AllData, FUN= "mean" )[,3]
}
colnames(TidyData)[3:ncol(TidyData)] <- colnames(AllData)[3:ncol(AllData)]

write.table(TidyData, file = "TidyData.txt", row.names = FALSE)



