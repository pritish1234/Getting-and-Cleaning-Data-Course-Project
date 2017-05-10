# Getting and Cleaning Data Project
# Pritish Patil


rm(list=ls())
features<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/features.txt', header = FALSE)
activityType<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/activity_labels.txt',header=FALSE)
subjectTrain<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/train/subject_train.txt',header = FALSE)
xTrain<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/train/X_train.txt',header=FALSE)
yTrain<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/train/y_train.txt',header=FALSE)

colnames(activityType)= c("activityId","activityName")
colnames(subjectTrain)= "subjectId"
colnames(xTrain)<- features[,2]
colnames(yTrain)<- "activity_id"

trainingData<- cbinD(yTrain,subjectTrain,xTrain)

subjectTest<-read.table('/Users/pritishpatil/Downloads/UCIHARDataset/test/subject_test.txt', header= FALSE)
xTest<- read.table("/Users/pritishpatil/Downloads/UCIHARDataset/test/X_test.txt",header = FALSE)
yTest<- read.table('/Users/pritishpatil/Downloads/UCIHARDataset/test/y_test.txt',header=FALSE)

colnames(xTest)<- features[,2]
colnames(subjectTest)<-"subjectId"
colnames(yTest)<- "activityId"

testdata<- cbind(yTest,subjectTest,xTest)

combinedData<- rbind(trainingData,testdata)

colNames<- colnames(combinedData)
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData <- finalData[logicalVector==TRUE]
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE)
colNames  <- colnames(finalData) 
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) <- colNames;
finalDataNoActivityType  <- finalData[,names(finalData) != 'activityType']
tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)
tidyData    <- merge(tidyData,activityType,by='activityId',all.x=TRUE)
write.table(tidyData, '/Users/pritishpatil/Downloads/UCIHARDataset/tidyData.txt',row.names=TRUE,sep='\t')














