run_analysis <- function() {
  ## Read features
  featbl <- read.table(".\\UCI HAR Dataset\\features.txt")
  ## Get measurement names into a vector
  measnames <- featbl[,2]
  
  ## Read test data
  test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
  
  ## Add meas names to test data
  colnames(test) <- measnames
  
  ## Read test data subjects
  subtbl <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
  ## Add column name to subject table
  colnames(subtbl) <- "Subject"
  
  ## Add subject column to test data
  test$Subject <- subtbl$Subject

  ## Read test data labels
  testlabeltbl <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
  ## Add column name to test data labels
  colnames(testlabeltbl) <- "ActivityCode"
  testlabeltbl$order <- c(1:nrow(testlabeltbl))
  
  ## Read activity labels
  activitytbl <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")
  ## Rename V2 columna
  names(activitytbl)[names(activitytbl)=="V2"] <- "ActivityDesc"
  ## Merge test label and activity
  combtbl <- merge(testlabeltbl, activitytbl, by.x = "ActivityCode", by.y = "V1", sort = FALSE)
  combtbl <- combtbl[order(combtbl$order), ]
  
  ## Add activity code and descriptions to test data
  test$ActivityCode <- combtbl$ActivityCode
  test$ActivityDesc <- combtbl$ActivityDesc
  
  ################################################################
  ################################################################
  ## Read train data
  train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
  
  ## Add meas names to test data
  colnames(train) <- measnames
  
  ## Read test data subjects
  subtrtbl <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
  ## Add column name to subject table
  colnames(subtrtbl) <- "Subject"
  
  ## Add subject column to test data
  train$Subject <- subtrtbl$Subject
  
  ## Read test data labels
  trainlabeltbl <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
  ## Add column name to train data labels
  colnames(trainlabeltbl) <- "ActivityCode"
  trainlabeltbl$order <- c(1:nrow(trainlabeltbl))
  
  ## Read activity labels
  ##activitytbl <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")
  ## Rename V2 columna
  ##names(activitytbl)[names(activitytbl)=="V2"] <- "ActivityDesc"
  ## Merge test label and activity
  combtrtbl <- merge(trainlabeltbl, activitytbl, by.x = "ActivityCode", by.y = "V1", sort = FALSE)
  combtrtbl <- combtrtbl[order(combtrtbl$order), ]
  
  ## Add activity code and descriptions to test data
  train$ActivityCode <- combtrtbl$ActivityCode
  train$ActivityDesc <- combtrtbl$ActivityDesc
  
  #print(head(train))
  #print(tail(train))
  
  ## Now combine the two
  total <- rbind(test,train)
  
  #print(nrow(total))
  #print(ncol(total))
  #head(total)
  cnum <- ncol(total) - 3 # Subtract the number of columns I added, remaining is the number of measurements
  # Compute mean of each measurement
  meanTotal <- colMeans(total[,1:cnum])
  
  # Compute standard deviation of each measurement
  sdTotal <- apply(total[,1:cnum],2,sd)
  
  ## Compute the mean for each measurement for each subject and activity
  subjects <- sort(unique(total$Subject))
  #print(subjects)
  #print(activitytbl$V1)
  
  # Loop over each subject and activity code, compute the column means
  subjcntr = 0
  actcntr = 0
  for (subj in subjects)
  {
    subjcntr <- subjcntr + 1
    for(act in activitytbl$V1)
    {
      actcntr <- actcntr + 1
      if (subjcntr == 1 & actcntr == 1)
      {
        meansubact <- apply(total[total$Subject == subj & total$ActivityCode == act, 1:cnum],2,mean)
        # Populate the vectors used for the subject, activity code, and activity description
        ac <- c(act)
        su <- c(subj)
        des <- c(as.character(activitytbl[activitytbl$V1 == act, 2]))
      }
      else
      {
        meansubact <- rbind(meansubact, apply(total[total$Subject == subj & total$ActivityCode == act, 1:cnum],2,mean))
        # Populate the vectors used for the subject, activity code, and activity description
        ac <- c(ac, act)
        su <- c(su, subj)
        des <- c(des, as.character(activitytbl[activitytbl$V1 == act, 2]))
      }

    } # activity cod loop

  } # subject loop
  
  # Place final averages into a data frame
  final <- suppressWarnings(data.frame(meansubact))
  # Add columns containing the subject, activity code, and activity description
  final$Subject <- su
  final$ActivityCode <- ac
  final$ActivityDesc <- des
  print(final)
  write.table(final, file = "subject_activity_means.txt", row.names = FALSE)

}
  