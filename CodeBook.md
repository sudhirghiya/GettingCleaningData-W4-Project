---
title: "Code Book: Getting & Cleaning Data-W4 Project"
author: "Sudhir Ghiya"
date: "Thursday, January 28, 2016"
output: html_document
---

I am summarizing the Code here. This can also be run using the R scipt "run_analysis.R".

```{r}
library(plyr)
```

**1. We read the data files (./data/features.txt, ./data/activity_labels.txt). We also Data for Test Volunteers (./data/test/x_test.txt ./data/test/y_test.txt ./data/test/subject_test.txt). We also read the Data for Training Volunteers (./data/train/x_train.txt ./data/train/y_train.txt ./data/train/subject_train.txt). Further, we Assign Names to the Columns for Test & Training Data**

```{r}
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
```

**2. The next step involves merging the Training Data and Test Data set.**

```{r}
#
# Combine Train, Test and Subject Data (Order train, test)
#
AllActivityData     <- rbind(xtrain, xtest)             # Activity
LabelsData          <- rbind(ytrain, ytest)             # Labels
SubjectData         <- rbind(subjecttrain, subjecttest) # Volunteers

# Remove dataframe's not needed henceforth
rm("xtest","xtrain","ytest","ytrain")
```

**This code creates the following datasets**

    + AllActivityData [10299 obs. of 561 variables (Avtivity Data for 30 Participants)]
    + LabelsData      [10299 obs. of 1 Variable (i.e. Activity)]
    + SubjectData     [10299 obs. of 1 Variable (i.e. Volunteer)]

**3. We now add Activity & Volunteer (SUbject) to this dataset.**

```{r}
# 
# Update Activity Dataframe, add ActivityLabels and Subject Data
#
AllActivityData$Activity   <- LabelsData[,1]            # Add Activity Code
AllActivityData$Volunteer  <- SubjectData[,1]           # Add Volunteer 

# Given Activity Code get Activity Name
for (i in 1:6){
  AllActivityData$ActivityName[as.numeric(AllActivityData$Activity) == i] <- 
        as.character(activitylabels[i,2])               
}

write.table(AllActivityData, file = "./CleanDataSet.txt", row.names=FALSE)

# for purposes of Project zip the dataset.
zip(zipfile="./CleanDataSet.txt.zip", files="./CleanDataSet.txt")

```

**This is now our basic Clean dataset, which can be used for any processing. I am saving this in a file called "CleanDataSet.txt". Due to size limits imposed by git on a file, I am zipping the file and uploading the same**

**4. We next proceed to filter those activities which have "mean" and "std" in their names**

```{r}
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
```

**5. We next proceed to compute means for each subject and Activity**

```{r}
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
```

**6. Finally, we output the Data to tidy_data.txt. This contains the mean values for data by Activity and Volunteer/Subject**

```{r}
Observation_Volunteer <- names(ProcessedActivityData)
tidy_data <- cbind(Observation_Volunteer, ActivityMeanData)

# O/P tidy data in Home Directory.
write.table(tidy_data, file = "./tidy_data.txt", row.names=FALSE)
```

**Following files have been uploaded for Submission**

**./run_analysis.R - Program which can be run**

**./CodeBook.md (this File) - CodeBook for the Project**

**./data/data.zip - data downloaded and used**

**./CleanDataSet.txt.zip - This can be used for all calculations**

**./tidy_data.txt - data for mean of Observations by Volunteer and Activity (w/ mean & std in names)**

