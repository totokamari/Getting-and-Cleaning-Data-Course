## I will start out working on the training data first
## I begin with downloading the files I'll need with the following script ...

activity_labels <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/features.txt")
subject_train <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/train/x_train.txt")
y_train <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/train/y_train.txt")

## I load dply

library(dplyr)


## Now I notice that the column names for the dataframe x_train are in the features dataframe
## I want to only include columns about mean and startd deviation

x_train <- select(x_train, V1:V6, V41:V46, V81:V86, V121:V126, V161:V166, V201:V202, V214:V215, V227:V228, V240:V241,
                  V253:V254, V266:V271, V345:V350, V424:V429, V503:V504, V516:V517, V529:V530, V542:V543, V555:V561)
features <- slice(features, c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271,
                              345:350, 424:429, 503:504, 516:517, 529:530, 542:543, 555:561))

## I use the following code to bring them together ..

names(x_train) <- features$V2

## I notice the y_train dataframe is simply the activities, so I merge the activity and y_train dataframes

y_train <- merge(y_train, activity_labels, by.x = "V1", by.y = "V1", all.x = TRUE)
y_train <- rename(y_train, activity_name = V2)
subject_train <- rename(subject_train, subject_number = V1)

## Now I want to combine the x_train, y_train and subject_train dataframes, so I'll add an ID column to each

x_train$x_id <- seq.int(nrow(x_train))
x_train_df <- x_train
y_train_df <- y_train %>% mutate(y_id = row_number())
subject_train_df <- subject_train %>% mutate(st_id = row_number())

## An now I merge the three dataframes ..

x_y_merge_train <- merge(x_train_df, y_train_df, by.x = "x_id", by.y = "y_id", all = TRUE)
training_dataframe <- merge(x_y_merge_train, subject_train_df, by.x = "x_id", by.y = "st_id", all = TRUE)

## Now I want to repeat these steps for the test data

subject_test <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/test/x_test.txt")
y_test <- read.table("./Getting and Cleaning Data Course/UCI HAR Dataset/test/y_test.txt")

x_test <- select(x_test, V1:V6, V41:V46, V81:V86, V121:V126, V161:V166, V201:V202, V214:V215, V227:V228, V240:V241,
                  V253:V254, V266:V271, V345:V350, V424:V429, V503:V504, V516:V517, V529:V530, V542:V543, V555:V561)

names(x_test) <- features$V2

y_test <- merge(y_test, activity_labels, by.x = "V1", by.y = "V1", all.x = TRUE)
y_test <- rename(y_test, activity_name = V2)
subject_test <- rename(subject_test, subject_number = V1)

x_test$x_id <- seq.int(nrow(x_test))
x_test_df <- x_test
y_test_df <- y_test %>% mutate(y_id = row_number())
subject_test_df <- subject_test %>% mutate(st_id = row_number())

x_y_merge_test <- merge(x_test_df, y_test_df, by.x = "x_id", by.y = "y_id", all = TRUE)
testing_dataframe <- merge(x_y_merge_test, subject_test_df, by.x = "x_id", by.y = "st_id", all = TRUE)

## Now I combine the training and test dataframes

combined_dataframe <- rbind(testing_dataframe, training_dataframe)


## Now I want to create a second data set with averages for each variable by activity and subject

library(reshape2)

almost_final_dataframe <- melt(combined_dataframe, id.vars = c("subject_number", "activity_name", "x_id"))
final_dataframe <- dcast(almost_final_dataframe, subject_number + activity_name ~ variable, fun.aggregate = mean)
