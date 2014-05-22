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


#Merges the training and the test sets to create one data set.
X_test[,562]="test"
X_train[,562]="train"
X_train=cbind(X_train,subject_train,y_train)
X_test=cbind(X_test,subject_test,y_test)

features[,2]=as.character(features[,2])
features[562,]=c(562L,"Source")
features[563,]=c(563L,"Subject")
features[564,]=c(564L,"Activity")

X=rbind(X_train,X_test)

colnames(X)=features[,2]

#Extracts only the measurements on the mean and standard deviation for each measurement. 
subX1=X[,grep("mean", colnames(X))]
subX2=X[,grep("std", colnames(X))] 
subX=cbind(subX1,subX2,X[,562:564])

#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive activity names
activity_labels[,2]=as.character(activity_labels[,2])

i=1
ind=nrow(subX)
while (i <= ind) {
      subX[i,ncol(subX)]=activity_labels[which(activity_labels$V1==subX[i,ncol(subX)]),2]
      i = i + 1
      }
tidydata1=subX
write.csv2(tidydata1,file="./tidydata1.csv",sep=",")

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidydata2=aggregate(tidydata1[,1] ~ tidydata1$Subject * tidydata1$Activity,FUN=mean)


j=2
ind=79
while (j <= ind) {
      temp=aggregate(tidydata1[,j] ~ tidydata1$Subject * tidydata1$Activity,FUN=mean)
      tidydata2[,j+2]=temp[,3]
      colnames(tidydata2[j+2])=colnames(tidydata1[j])
      j = j + 1
}

write.csv2(tidydata2,file="./tidydata2.csv",sep=",")
