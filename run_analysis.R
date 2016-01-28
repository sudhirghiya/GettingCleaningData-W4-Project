#
# title: Name: run_analysis.R
# author: Sudhir Ghiya
# date: 28-Jan-2016
#

library(plyr)

# Get Features & Activity Label Data
features       <- read.table("./data/features.txt")             # Get features data
activitylabels <- read.table("./data/activity_labels.txt")      # get activity labels

# Get Test Data
subjecttest    <- read.table("./data/test/subject_test.txt")    # Get subject_test.txt
xtest          <- read.table("./data/test/X_test.txt")          # Get X_test.txt
ytest          <- read.table("./data/test/y_test.txt")          # Get y_test.txt

# Get Train Data
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

# Remove dataframe's not needed henceforth
rm("xtest","xtrain","ytest","ytrain")

# 
# Update Activity Dataframe, add ActivityLabels and Subject Data
#
AllActivityData$Activity   <- LabelsData[,1]
AllActivityData$Volunteer  <- SubjectData[,1]
for (i in 1:6){
    AllActivityData$ActivityName[as.numeric(AllActivityData$Activity) == i] <- as.character(activitylabels[i,2])
}

write.table(AllActivityData, file = "./CleanDataSet.txt", row.names=FALSE)
zip(zipfile="./CleanDataSet.txt.zip", files="./CleanDataSet.txt")

#
# Get which values of features contain mean and std readings
#
stdmean         <- grep("mean\\(\\)|std", features[,2])
featuresdata    <- features[stdmean,]

# Filter Data for mean and std readings
ActivityData            <- AllActivityData[,stdmean]
ActivityData$Activity   <- LabelsData[,1]            # We have lost this during filtering above
ActivityData$Volunteer  <- SubjectData[,1]           # We have lost this during filtering above

for (i in 1:6){
    ActivityData$Activity[as.numeric(ActivityData$Activity) == i] <- as.character(activitylabels[i,2])
}

#
# Redo Columns (Activity & Subject) as factors;
#
ActivityData$Activity  <- as.factor(ActivityData$Activity)
ActivityData$Activity  <- as.factor(ActivityData$Activity)
ActivityData$Volunteer <- as.factor(ActivityData$Volunteer)

# Split Data
ProcessedActivityData <- split(ActivityData, list(ActivityData$Activity, ActivityData$Volunteer))

#
# start with fresh dataset for means...
#
ActivityMeanData <- c()

#
# Get mean for observations & keep binding means in loop
# i in 1:180 (6 activities * 30 Subjects)
#
for (i in 1:180) {                      
    means <- lapply(ProcessedActivityData[[i]][1:nrow(featuresdata)], mean)
    ActivityMeanData <- rbind(ActivityMeanData, means, deparse.level = 0)
}

Observation_Volunteer <- names(ProcessedActivityData)
tidy_data <- cbind(Observation_Volunteer, ActivityMeanData)

# O/P tidy data in Home Directory.
write.table(tidy_data, file = "./tidy_data.txt", row.names=FALSE)
