setwd("E:/ljubi/edukacija/Coursera/John Hopkins - Data Science Specialization/cleaning data/week 4/UCI HAR Dataset")
#firstly we load the work directory, note that I put all the essential
#data in one folder, so when I initiate list.files() I can pick all
#of the files I need and analyze them prior(because of dimensions, which
#we must familiarise ourselves when using join/merge/cbind/rbind etc.)
list.files()
library(dplyr)
library(plyr)
activitydescription <- read.table("activity_labels.txt")
#there are 6 different modes of activity, this we will need to assign to
#subject activity and later make "mass calculations" basing on activity
#labels and on subjects, in this case we will get the mean and the std
#just as asked
#in the assignment description
xtest <- read.table("x_test.txt")
dim(xtest)
features <- read.table("features.txt")
dim(features)
features
#note need to use second column when ascribing designated
#columns
# our main "data file has 561 columns, which corresponds to the values of 
# features file, and of course the train file, but firstly before adding
#everything we need to add combing test files into one
# dataframe and then add to the combined train files dataframe
ytest <- read.table("y_test.txt")
table(ytest)
dim(ytest)
# I often use the table command
# which is very useful in brief analyzing datasets, 
#here we see 6 values, so we can easily assume it is about the
# activity modes of all subjects, because the row count is the same
# as the x_test, which are variables which we still need to assign
#but firstly we will use the grepl method to select only columns which
#interes us, the mean and the standard deviation
featuresnames <- features$V2
featuresnames
featuresmeanstd <- features[grep("Mean|mean|std",features$V2),]
#caution, there are column names with Capslock and without, and R is almost
#always upper or lower letter sensitive
featuresmeanstd
xlistnumbers <- xtest[,featuresmeanstd$V1]
xlistnumbers
#there are the numbers we have sorted, now we need to add the colnames
colnames(xlistnumbers) <- featuresmeanstd$V2
xtestmeanstd <- xlistnumbers
#please note - my variable names could be more straightforward and more descriptive
#also I should use commands such as %>% to shorten the scripts
stest <- read.table("subject_test.txt")
table(stest)
stest
testvalues <- cbind(stest,ytest,xtestmeanstd)
dim(testvalues)
head(testvalues)
#we have a problem with V1 AND V2 values, first belongs to stest and
#the other to ytest dataframe, cbind binds by order so we must rename
#the columns not to get confused later on, and maybe in some other
#data project we would be merging by same columnnames, so it is important
#for programming standards and for description. Also we should do the renaming BEFORE adding
#to dataframe, so I made a mistake and now need to "manually" add the column names
colnames(testvalues)[1] <- "subjectID"
colnames(testvalues)[2] <- "activitymode_test"
list.files()
head(testvalues)
#now we are going to create the train dataframe
strain <- read.table("subject_train.txt")
table(strain)
colnames(strain) <- "subjectID"
head(strain)
ytrain <- read.table("y_train.txt")
table(ytrain)
colnames(ytrain) <- "activitymode_test"
xtrain <- read.table("x_train.txt")
dim(xtrain)
#same grepl method can be used on xtrain as we used on xtest
xlistnumbers2 <- xtrain[,featuresmeanstd$V1]
dim(xlistnumbers2)
colnames(xlistnumbers2) <- featuresmeanstd$V2
xtrainmeanstd <- xlistnumbers2
xtrainmeanstd
trainvalues <- cbind(strain,ytrain,xtrainmeanstd)
dim(trainvalues)
dim(testvalues)
MergedData <- rbind(testvalues,trainvalues)
dim(MergedData)
head(MergedData)
table(MergedData$subjectID)
MergedData
#table is being used just to see if the rbind didn't mess something up
#we see all 30 subjects displayed, without "funny" numbers
#we still need to merge the data values(activity modes and descriptions
#for our last Step of this assignment)
list.files()
activitynames <- read.table("activity_labels.txt")
activitynames
#merge through activitymode_test
#there is V1 and V2 , by V1 we can merge the main dataframe with the
#description, by 1-6 values they will "blend", but now we need to assign
#it the same name columns as in our dataframe
names(activitynames)[1] <- "activitymode_test"
names(activitynames)[2] <- "activity_desc"
activitynames
head(MergedData)
MergedData_final <- merge(activitynames,MergedData,by.x="activitymode_test", by.y="activitymode_test")
MergedData_final
colnames(MergedData_final)
#we need to separate the data when doing summaries, not to select
#the values we don't want, for example our table could be using subject id's
#for mean calculation if we don't separate the data correctly
SortedData <- MergedData_final%>%select(activity_desc,subjectID, 4:89)
SortedData
Analysis <- SortedData%>%group_by(activity_desc,subjectID)%>%summarize_all(funs(mean))
Analysis
library(xlsx)
write.xlsx(Analysis,"C:/Analysis.xlsx")
write.table(Analysis,"Analysis-activity.txt",rownames=FALSE)

