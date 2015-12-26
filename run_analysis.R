##     1. Merges the training and the test sets to create one data set.
##     2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##     3. Uses descriptive activity names to name the activities in the data set
##     4. Appropriately labels the data set with descriptive variable names. 
##     5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
    
    
##Sets up the working environment
    x_test_data <- read.table("test/X_test.txt")
    y_test_data <- read.table("test/y_test.txt")
    x_train_data <- read.table("train/X_train.txt")
    y_train_data <- read.table("train/y_train.txt")
    subject_test_data <- read.table("test/subject_test.txt")
    subject_train_data <- read.table("train/subject_train.txt")
    features <- read.table("features.txt")
    activity <- read.table("activity_labels.txt")
    
    FullDataSetx <- rbind(x_test_data, x_train_data)
    FullDataSety <- rbind(y_test_data, y_train_data)
    FullDataSets <- rbind(subject_test_data, subject_train_data)
    
    
##Extracts only the measurements on the mean and stdevfor each measurement
    extractFeatures <- grepl("mean|std", features$V2)
    
    
##Uses descriptive activity names to name the activities in the data set
    FullDataSety[,2] = activity$V2[FullDataSety[,1]]
    names(FullDataSety) = c("Activity_ID", "Activity_Label")
    names(FullDataSets) = "Subject"
    
    
##Labels the data set with descriptive variable names
    names(FullDataSetx) = features$V2
    
    
##Merges the training and the test sets to create one data set
    data <- cbind(FullDataSets, FullDataSety, FullDataSetx[, extractFeatures])
    
    
##Creates a second, independent tidy data set with the average
##of each variable for each activity and each subject
    
    TidyData <- matrix(0, nrow=length(unique(data[,1]+data[,2]/10)), 
                       ncol=length(data))
    TidyData <- data.frame(TidyData)
    names(TidyData) = names(data)
    #generates a blank TidyData dataframe based on the anticipated size
    
    currentrow = 1
    for (i in unique(data[,1])) {
        select_i <- which(data[,1] == i)
        
        for (j in unique(data[select_i,2])) {
            select_j <- which(data[,2] == j)

            TidyData[currentrow,1] = i
            TidyData[currentrow,2] = j
            TidyData[currentrow,3] = 0
            TidyData[currentrow,4:length(data)] = 
                colMeans(data[select_j,4:length(data)])
            
            currentrow = currentrow + 1
        }
    }
    TidyData[,3] = activity$V2[TidyData[,2]]
    #adds the activity description to match the numerical value
    write.table(TidyData, file = "TidyData.txt", row.name=FALSE)
