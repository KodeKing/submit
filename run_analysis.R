# project.R
# C:\Users\Jack\Documents\EducationalCourses\GetCleanData\Proj\project.R
#
# set working dir
setwd("C:/Users/Jack/Documents/EducationalCourses/GetCleanData/Proj")
# read training data
dfTrain   <- read.table("x_train.txt")        # 7352 x 561 vars
actvTrain <- read.table("y_train.txt")        # 7352 x 1 activities
subjTrain <- read.table("subject_train.txt")  # 7352 x 1 subjects

# read test data
dfTest    <- read.table("x_test.txt")         # 2947 x 561 vars
actvTest  <- read.table("y_test.txt")          # 2947 x 1 activities 
subjTest  <- read.table("subject_test.txt")   # 2947 x 1 subjects

# read features (col headings for training and test data)
featrs <- read.table("features.txt",stringsAsFactors=FALSE)

# get rid of col 1 which is numbers 1-561
featrs <- featrs[,-1]     # 1 x 561

# use features vector to replace col names of V1...V561
names(dfTrain) <- featrs
names(dfTest)  <- featrs

# now get rid of all columns that do not end with 
#    std() or mean() in name -- leaves 18 vars
vecNames <- names(dfTrain)
vecKeep <- vecNames[grepl("mean..$",vecNames) | grepl("std..$",vecNames)]
dfTrain <- dfTrain[,vecKeep]
dfTest  <- dfTest[,vecKeep]

cat(ncol(dfTrain)," columns kept\n")

# prepend activity to each data frame
dfTest =cbind(actvTest,dfTest)
dfTrain=cbind(actvTrain,dfTrain)

# prepend subject to each data frame (which has activity and 79 vars)
dfTest =cbind(subjTest,dfTest)
dfTrain=cbind(subjTrain,dfTrain)

# combine data frames
dfBoth <- rbind(dfTest,dfTrain)

# give meaningful names to subject & activity
names(dfBoth)[1] = 'subject'
names(dfBoth)[2] = 'activity'

# all needed data is in dfBoth
# need to summarize by grouping by activity(6) and subject(30)
# and taking average of all other fields to get 180 rows.
col2avg <- seq(from=3,to=ncol(dfBoth))
dfSummary <- aggregate(x=dfBoth[,col2avg],
              by=list(dfBoth$activity,dfBoth$subject),FUN=mean)
# aggregate renames activity to Group.1
# aggregate renames subject to Group.2, rename both:
names(dfSummary)[1] = 'activity'
names(dfSummary)[2] = 'subject'
# need to replace activity number with descriptive name
# first, make a function:
f1 <- function(tparm) {
  if (tparm==1) {return('Walking')}
  if (tparm==2) {return('Walking_Upstairs')}
  if (tparm==3) {return('walking_downstairs')}
  if (tparm==4) {return('sitting')}
  if (tparm==5) {return('standing')}
  if (tparm==6) {return('laying')}
  return('error' )}
dfSummary$activity <- lapply(dfSummary$activity,f1)
# get rid of list attribute of activity
dfSummary$activity <- unlist(dfSummary$activity)
# write a .csv file with .txt suffix since Coursera cannot
#    accept .csv file
write.csv(dfSummary,"Project.txt")