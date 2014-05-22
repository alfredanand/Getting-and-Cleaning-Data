download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","./getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",method="curl",)

unzip("./getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", exdir="./")

features=read.table("./UCI HAR Dataset/features.txt")
activity_labels=read.table("./UCI HAR Dataset/activity_labels.txt")

subject_train=read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train=read.table("./UCI HAR Dataset/train/X_train.txt")
y_train=read.table("./UCI HAR Dataset/train/y_train.txt")

subject_test=read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test=read.table("./UCI HAR Dataset/test/X_test.txt")
y_test=read.table("./UCI HAR Dataset/test/y_test.txt")

# Change variable names in features and activities to that which is human-readable
features$V2=gsub("acc","Acceleration",features$V2,ignore.case = TRUE)
features$V2=gsub("std","StandardDeviation",features$V2,ignore.case = TRUE)
features$V2=gsub("mad","MedianAbsoluteDeviation",features$V2,ignore.case = TRUE)
features$V2=gsub("inds","Index",features$V2,ignore.case = TRUE)
features$V2=gsub("mean","Mean",features$V2,ignore.case = TRUE)
features$V2=gsub("max","Maximum",features$V2,ignore.case = TRUE)
features$V2=gsub("min","Minimum",features$V2,ignore.case = TRUE)
features$V2=gsub("energy","Energy",features$V2,ignore.case = TRUE)
features$V2=gsub("iqr","InterQuartileRange",features$V2,ignore.case = TRUE)
features$V2=gsub("entropy","SignalEntropy",features$V2,ignore.case = TRUE)
features$V2=gsub("arCoeff","AutoRegressionCoefficient",features$V2,ignore.case = TRUE)
features$V2=gsub("correlation","CorrelationCoefficient",features$V2,ignore.case = TRUE)
features$V2=gsub("mag","Magnitude",features$V2,ignore.case = TRUE)
features$V2=gsub("freq","Frequency",features$V2,ignore.case = TRUE)
features$V2=gsub("skew","Skew",features$V2,ignore.case = TRUE)
features$V2=gsub("kurt","Kurt",features$V2,ignore.case = TRUE)
features$V2=gsub("band","Band",features$V2,ignore.case = TRUE)
features$V2=gsub("inds","Index",features$V2,ignore.case = TRUE)
features$V2=gsub("gravity","Gravity",features$V2,ignore.case = TRUE)
features$V2=gsub(",","to",features$V2,ignore.case = TRUE)
features$V2=gsub("sma","SignalMagnitudeArea",features$V2,ignore.case = TRUE)
features$V2=gsub("angle","Angle",features$V2,ignore.case = TRUE)
features$V2=gsub("\\(","",features$V2,ignore.case = TRUE)
features$V2=gsub("\\)","",features$V2,ignore.case = TRUE)

activity_labels$V2=gsub("WALKING","Walking",activity_labels$V2,ignore.case = TRUE)
activity_labels$V2=gsub("WALKING_UPSTAIRS","WalkingUpstairs",activity_labels$V2,ignore.case = TRUE)
activity_labels$V2=gsub("WALKING_DOWNSTAIRS","WalkingDownstairs",activity_labels$V2,ignore.case = TRUE)
activity_labels$V2=gsub("SITTING","Sitting",activity_labels$V2,ignore.case = TRUE)
activity_labels$V2=gsub("STANDING","Standing",activity_labels$V2,ignore.case = TRUE)
activity_labels$V2=gsub("LAYING","Laying",activity_labels$V2,ignore.case = TRUE)

#Merges the training and the test sets to create one data set.
X_test[,562]="Test"
X_train[,562]="Train"
X_train=cbind(X_train,subject_train,y_train)
X_test=cbind(X_test,subject_test,y_test)

features[562,]=c(562L,"Source")
features[563,]=c(563L,"Subject")
features[564,]=c(564L,"Activity")

X=rbind(X_train,X_test)

colnames(X)=features[,2]

#Extracts only the measurements on the mean and standard deviation for each measurement. 
toMatch=c("Mean", "StandardDeviation")
subX=X[,grep(paste(toMatch,collapse="|"),colnames(X))]
subX=cbind(subX,X[,562:564])

#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive activity names
i=1
ind=nrow(subX)
while (i <= ind) {
      subX[i,ncol(subX)]=activity_labels[which(activity_labels$V1==subX[i,ncol(subX)]),2]
      i = i + 1
      }
tidydata1=subX
write.csv2(tidydata1,file="./Getting-and-Cleaning-Data/tidydata1.csv",sep=",")

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidydata2=aggregate(tidydata1[,1] ~ tidydata1$Subject * tidydata1$Activity,FUN=mean)
colnames(tidydata2)[1]="Subject"
colnames(tidydata2)[2]="Activity"

j=1
ind=ncol(tidydata1)-3
while (j <= ind) {
      temp=aggregate(tidydata1[,j] ~ tidydata1$Subject * tidydata1$Activity,FUN=mean)
      tidydata2[,j+2]=temp[,3]
      colnames(tidydata2)[j+2]=colnames(tidydata1)[j]
      j = j + 1
}

write.csv2(tidydata2,file="./Getting-and-Cleaning-Data/tidydata2.csv",sep=",")
