#
# Script Name: run_analysis.R
# 
# You should create one R script called run_analysis.R that does the following.
# 

#
# Objectives:
# ----------
# 0. Read the Data set(s)
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#     average of each variable for each activity and each subject.
#

#
# You will be required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work 
#    that you performed to clean up the data called CodeBook.md. You should also include 
#    a README.md in the repo with your scripts. This repo explains how all of the scripts 
#    work and how they are connected.
#


library(plyr)

features       <- read.table("./data/features.txt")             # Get features data
activitylabels <- read.table("./data/activity_labels.txt")      # get activity labels

# get Test Data
subjecttest    <- read.table("./data/test/subject_test.txt")    # Get subject_test.txt
xtest          <- read.table("./data/test/X_test.txt")          # Get X_test.txt
ytest          <- read.table("./data/test/y_test.txt")          # Get y_test.txt

# get Train Data
subjecttrain   <- read.table("./data/train/subject_train.txt")  # Get subject_train.txt
xtrain         <- read.table("./data/train/X_train.txt")        # Get X_train.txt
ytrain         <- read.table("./data/train/y_train.txt")        # Get y_train.txt

# Put Names to Columns (features)
names(xtest)   <- features[,2]
names(xtrain)  <- features[,2]
names(subjecttest)  <- c("Volunteers")
names(subjecttrain) <- c("Volunteers")

#
# Combine Train, Test and Subject Data (Order train, test)
#
AllActivityData     <- rbind(xtrain, xtest)             # Activity
LabelsData          <- rbind(ytrain, ytest)             # Labels
SubjectData         <- rbind(subjecttrain, subjecttest) # Volunteers

rm("xtest","xtrain","ytest","ytrain")

#
# Get which values of features contain mean and std readings
#
stdmean         <- grep("mean\\(\\)|std", features[,2])
featuresdata    <- features[stdmean,]

# Subset Data for mean and std readings
ActivityData            <- AllActivityData[,stdmean]
ActivityData$Activity   <- LabelsData[,1]
ActivityData$Volunteer  <- SubjectData[,1]

for (i in 1:6){
    # Using Supresswarnings to supress warning Messages:
    # Warning messages: NAs introduced by coercion
    suppressWarnings(ActivityData$Activity[as.numeric(ActivityData$Activity) == i] <- as.character(activitylabels[i,2]))
    # print(i)
    # ActivityData$Activity[as.numeric(ActivityData$Activity) == i] <- as.character(activitylabels[i,2])
}

#
# Redo Columns (Activity & Subject) as factors;
#
ActivityData$Activity  <- as.factor(ActivityData$Activity)
ActivityData$Activity  <- as.factor(ActivityData$Activity)
ActivityData$Volunteer <- as.factor(ActivityData$Volunteer)

# Writing Clean Data before processing for means etc.
write.table(ActivityData, file = "./CleanData.txt", row.names=FALSE)

# Split Data
ProcessedActivityData <- split(ActivityData, list(ActivityData$Activity, ActivityData$Volunteer))

#
# start with fresh dataset for means...
#
cm <- c()

#
# Get mean for observations & keep binding means in loop
# i in 1:180 (6 activities * 30 Subjects)
#
for (i in 1:180) {                      
    means <- lapply(ProcessedActivityData[[i]][1:nrow(featuresdata)], mean)
    cm <- rbind(cm, means, deparse.level = 0)
}

rm(i)

Observation_Volunteer <- names(ProcessedActivityData)
tidy_data <- cbind(Observation_Volunteer, cm)

# O/P tidy data in Home Directory.
write.table(tidy_data, file = "./tidy_data.txt", row.names=FALSE)

