# step by step - attempt 2

library(dplyr)
library(tidyr)

# filepath to data
filepath <- "~/code/Samsung_Wrangle/UCI_HAR_Dataset/"

subject_test <- read.table(paste(filepath, "test/subject_test.txt", sep = ""))
X_test <- read.table(paste(filepath, "test/X_test.txt", sep = ""))
Y_test <- read.table(paste(filepath, "test/Y_test.txt", sep = ""))

# explore data
unique(Y_test)
# has unique values 1:6 so will refer to activity type
activity_labels <- read.table(paste(filepath, "activity_labels.txt", sep = ""))

# subject train and test - calls of unique show that test and train are split by subject
unique(subject_test)
unique(subject_train)

# label Y_test instances - check number of observations = 2947 still
labYtest <- left_join(Y_test, activity_labels, by = "V1")
labYtest <- setNames(labYtest, c("ActivityLabel", "ActivityName"))
testsub <- bind_cols(subject_test, labYtest)
colnames(testsub)[1] <- "Subject"


# label X_test
features <- read.table(paste(filepath, "features.txt", sep = ""))
features1 <- as.vector(features[["V2"]])

# use make names on features in order to create unique column names
features2 <- make.unique(features1, sep = ".")

X_test_lab <- setNames(X_test, features2)

# add test data
X_test_comp <- bind_cols(testsub, X_test_lab)

# do the same for train!
X_train <- read.table(paste(filepath, "train/X_train.txt", sep = ""))
Y_train <- read.table(paste(filepath, "train/Y_train.txt", sep = ""))
subject_train <- read.table(paste(filepath, "train/subject_train.txt", sep = ""))
unique(Y_train)


# label Y_test instances - check number of observations = 2947 still
labYtrain <- left_join(Y_train, activity_labels, by = "V1")
labYtrain <- setNames(labYtrain, c("ActivityLabel", "ActivityName"))
trainsub <- bind_cols(subject_train, labYtrain)
colnames(trainsub)[1] <- "Subject"


X_train_lab <- setNames(X_train, features2)

# add train data
X_train_comp <- bind_cols(trainsub, X_train_lab)

# create column for the type of data
X_test_comp <- mutate(X_test_comp, test_train = "test")
X_train_comp <- mutate(X_train_comp, test_train = "train")

X_test_comp <- X_test_comp[ , c("Subject", "ActivityLabel", "ActivityName", "test_train", features2)]
X_train_comp <- X_train_comp[ ,c("Subject", "ActivityLabel", "ActivityName", "test_train", features2)]

# put test and train together
comp <- bind_rows(X_test_comp, X_train_comp)

# search for columns containing information on the mean and standard deviation
compMSd <- select(comp, Subject, ActivityLabel, ActivityName, contains("mean"), contains("std"))
comptidy <- compMSd %>%
  group_by(Subject, ActivityLabel, ActivityName) %>%
  summarise_each(funs(mean))

# write tidy data to csv
write.csv(comptidy, file = "SamsungTidy.csv")
















