# 1. Merges the training and the test sets to create one data set.

x_train <- read.table("UCI_HAR_Dataset/train/X_train.txt")
x_test <- read.table("UCI_HAR_Dataset/test/X_test.txt")
X <- rbind(x_train, x_test)

subject_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
subject_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
subject <- rbind(subject_train, subject_test)

y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")
y_test <- read.table("UCI_HAR_Dataset/test/y_test.txt")
Y <- rbind(y_train, y_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI_HAR_Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("UCI_HAR_Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(subject) <- "subject"
cleaned <- cbind(subject, Y, X)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
     for (a in 1:numActivities) {
          result[row, 1] = uniqueSubjects[s]
          result[row, 2] = activities[a, 2]
          tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
          result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
          row = row+1
     }
}
write.table(result, "UCI_HAR_Dataset/dataset_with_averages.txt", row.name=FALSE)