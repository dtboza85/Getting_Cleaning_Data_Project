library(dplyr)

filename <- "data.zip"

###### Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename)
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

######### Load activity labels and features

aLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
aLabels[,2] <- as.character(aLabels[,2])
colnames(aLabels) <- c("ActivityId", "ActivityDescription")
allFeatures <- read.table("UCI HAR Dataset/features.txt")

########### Extract only the data on mean and standard deviation
selectedFeatures <- allFeatures [grep(".*mean.*|.*std.*", as.character(allFeatures[,2])),2]

# remove hyper, and parenthesis from names
fselectedFeatures <- gsub('mean', 'Mean', selectedFeatures)
fselectedFeatures <- gsub('std', 'Std', fselectedFeatures)
fselectedFeatures <- gsub('-', '', fselectedFeatures)
fselectedFeatures <- gsub('[()]', '', fselectedFeatures)

###### Load Train Dataset
train <- read.table("UCI HAR Dataset/train/X_train.txt")
# select only target features
train <- train[,selectedFeatures]
colnames(train)<- fselectedFeatures
#Load activity data
trnActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
colnames(trnActivities)<- "ActivityId"
# Add activity description column
trnActivities <- merge(trnActivities,aLabels, by =  "ActivityId", all = TRUE)
# Load subject
trnSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
colnames(trnSubjects)<- "Subject"

###### Load Test Dataset
test <- read.table("UCI HAR Dataset/test/X_test.txt")
# select only target features
test <- test[,selectedFeatures]
colnames(test)<- fselectedFeatures
# Load Activity Data
tstActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
colnames(tstActivities)<- "ActivityId"
# Add activity description column
tstActivities <- merge(tstActivities,aLabels, by =  "ActivityId", all = TRUE)
#Load subject data
tstSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
colnames(tstSubjects)<- "Subject"

######## merge datasets 
allFeaturesTrain <- cbind(train, trnActivities, trnSubjects)
allFeaturesTest <- cbind(test, tstActivities, tstSubjects)
df_Data <- rbind(allFeaturesTrain,allFeaturesTest)

df_Data$ActivityDescription <- as.factor(df_Data$ActivityDescription)
df_Data$ActivityId <- as.factor(df_Data$ActivityId)
df_Data$Subject <- as.factor(df_Data$Subject)

######### Aggergate by subject and Activity
tidy_Data <- aggregate(.~Subject + ActivityId + ActivityDescription,df_Data, mean)
tidy_Data <- arrange(tidy_Data,Subject,ActivityId)

######### Write tidy data in output txt file
write.table(tidy_Data, "tidy.txt", row.names = FALSE, quote = FALSE)
