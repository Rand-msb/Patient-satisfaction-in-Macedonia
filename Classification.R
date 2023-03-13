
dataset = read.csv('datasetsatisfaction.csv')
getwd()



#data preprocessing
#removing null values
is.na(dataset)
sum(is.na(dataset))
dataset = na.omit(dataset)
#data transformation ordering the data by switching the values of 2 and 4
dataset[dataset == 2]<-"not satisfied"
dataset[dataset == 4]<-"very satisfied"
dataset[dataset =="not satisfied"]<- 4
dataset[dataset =="very satisfied"]<- 2
dataset$satisfaction.in.RM[dataset$satisfaction.in.RM == 4] <- 2


#-------------------------------------------------------------------------------------------------------------------


#classification:
#-----------------------------before removing class label 1
#---------------------train 70% test 30%

# sampling the data by portioning into two data sets training data which consists of 0.70 of the data
# and test data which consist of 0.30 percent
set.seed(1234)
ind = sample(2, nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))#change sizes here
trainData = dataset[ind == 1,]
testData = dataset[ind == 2,]

#build the tree

library(rpart)
formulatr<- satisfaction.in.RM ~ Check.up.appointment + Time.waiting + Admin.procedures + Time.of.appointment + Hygiene.and.cleaning + Quality.experience.dr.+ Specialists.avaliable + Communication.with.dr + Exact.diagnosis + Modern.equipment + friendly.health.care.workers + lab.services + avaliablity.of.drugs+waiting.rooms + hospital.rooms.quality + parking..playing.rooms..caffes
tree = rpart(formulatr, data = trainData)

#plot the tree

library (rpart.plot)
rpart.plot(tree)


library(caret)
#test the prediction of the training data

predictionTrain = predict(tree, type="class")

trainDataTable<-table(predictionTrain, trainData$satisfaction.in.RM)


#evaluate the tree by predicting the test data and saving the value in ourTable variable
predict_testData= predict(tree, newdata = testData, type = "class")
ourTable<-table(predict_testData , testData$satisfaction.in.RM)



# create the confusion matrix for the test data
confusionMatrix(ourTable)

precision(ourTable)


#variabels importance
tree$variable.importance

# create the confusion matrix for the training data

confusionMatrix(trainDataTable)



#-----------------------------after removing class label 1


#rplot the one that worked the best for us
#remove class label 1 
dataset = dataset[!(dataset$satisfaction.in.RM == 1),]

# sampling the data by portioning into two data sets training data which consists of 0.70 of the data
# and test data which consist of 0.30 percent
set.seed(1234)
ind = sample(2, nrow(dataset), replace = TRUE, prob = c(0.70, 0.30))#change sizes here
trainData = dataset[ind == 1,]
testData = dataset[ind == 2,]

#build the tree

library(rpart)
formulatr<- satisfaction.in.RM ~ Check.up.appointment + Time.waiting + Admin.procedures + Time.of.appointment + Hygiene.and.cleaning + Quality.experience.dr.+ Specialists.avaliable + Communication.with.dr + Exact.diagnosis + Modern.equipment + friendly.health.care.workers + lab.services + avaliablity.of.drugs+waiting.rooms + hospital.rooms.quality + parking..playing.rooms..caffes
tree = rpart(formulatr, data = trainData)

#plot the tree

library (rpart.plot)
rpart.plot(tree)


library(caret)
#test the prediction of the training data

predictionTrain = predict(tree, type="class")

trainDataTable<-table(predictionTrain, trainData$satisfaction.in.RM)


#evaluate the tree by predicting the test data and saving the value in ourTable variable
predict_testData= predict(tree, newdata = testData, type = "class")
ourTable<-table(predict_testData , testData$satisfaction.in.RM)



# create the confusion matrix for the test data
confusionMatrix(ourTable)

precision(ourTable)


#variabels importance
tree$variable.importance

# create the confusion matrix for the training data

confusionMatrix(trainDataTable)



#---------------------train 80% test 20%

# sampling the data by portioning into two data sets training data which consists of 0.80 of the data
# and test data which consist of 0.20 percent
set.seed(1234)
ind = sample(2, nrow(dataset), replace = TRUE, prob = c(0.80, 0.20))#change sizes here
trainData = dataset[ind == 1,]
testData = dataset[ind == 2,]

#build the tree

library(rpart)
formulatr<- satisfaction.in.RM ~ Check.up.appointment + Time.waiting + Admin.procedures + Time.of.appointment + Hygiene.and.cleaning + Quality.experience.dr.+ Specialists.avaliable + Communication.with.dr + Exact.diagnosis + Modern.equipment + friendly.health.care.workers + lab.services + avaliablity.of.drugs+waiting.rooms + hospital.rooms.quality + parking..playing.rooms..caffes
tree = rpart(formulatr, data = trainData)

#plot the tree

library (rpart.plot)
rpart.plot(tree)


library(caret)
#test the prediction of the training data

predictionTrain = predict(tree, type="class")

trainDataTable<-table(predictionTrain, trainData$satisfaction.in.RM)


#evaluate the tree by predicting the test data and saving the value in ourTable variable
predict_testData= predict(tree, newdata = testData, type = "class")
ourTable<-table(predict_testData , testData$satisfaction.in.RM)



# create the confusion matrix for the test data
confusionMatrix(ourTable)

precision(ourTable)


#variabels importance
tree$variable.importance

# create the confusion matrix for the training data

confusionMatrix(trainDataTable)



#---------------------train 85% test 15%

# sampling the data by portioning into two data sets training data which consists of 0.70 of the data
# and test data which consist of 0.30 percent
set.seed(1234)
ind = sample(2, nrow(dataset), replace = TRUE, prob = c(0.85, 0.15))#change sizes here
trainData = dataset[ind == 1,]
testData = dataset[ind == 2,]

#build the tree

library(rpart)
formulatr<- satisfaction.in.RM ~ Check.up.appointment + Time.waiting + Admin.procedures + Time.of.appointment + Hygiene.and.cleaning + Quality.experience.dr.+ Specialists.avaliable + Communication.with.dr + Exact.diagnosis + Modern.equipment + friendly.health.care.workers + lab.services + avaliablity.of.drugs+waiting.rooms + hospital.rooms.quality + parking..playing.rooms..caffes
tree = rpart(formulatr, data = trainData)

#plot the tree

library (rpart.plot)
rpart.plot(tree)


library(caret)
#test the prediction of the training data

predictionTrain = predict(tree, type="class")

trainDataTable<-table(predictionTrain, trainData$satisfaction.in.RM)


#evaluate the tree by predicting the test data and saving the value in ourTable variable
predict_testData= predict(tree, newdata = testData, type = "class")
ourTable<-table(predict_testData , testData$satisfaction.in.RM)



# create the confusion matrix for the test data
confusionMatrix(ourTable)

precision(ourTable)


#variabels importance
tree$variable.importance

# create the confusion matrix for the training data

confusionMatrix(trainDataTable)






#---------------------------------------------------------------

#ctree
#this is the tree that didnt work with us
#size of training set 70% test set 30% and sample by random sampling
set.seed(1234)
datasetctree <- as.data.frame(apply(dataset, 2, as.numeric))
ind = sample(2, nrow(datasetctree), replace = TRUE, prob = c(0.70, 0.30))
trainData = datasetctree[ind == 1,]
testData = datasetctree[ind == 2,]



library(party)
formula<- satisfaction.in.RM ~ Check.up.appointment + Time.waiting + Admin.procedures + Time.of.appointment + Hygiene.and.cleaning + Quality.experience.dr.+ Specialists.avaliable + Communication.with.dr + Exact.diagnosis + Modern.equipment + friendly.health.care.workers + lab.services + avaliablity.of.drugs+waiting.rooms + hospital.rooms.quality + parking..playing.rooms..caffes
data_ctree <- ctree(formula, data= trainData)

#plot the ctree this was the tree that didnt plot our data correctly
plot(data_ctree)



#--------------------------------------------------------------------------------------------------------

#the clustering is in the other R file so no errors occur







































 










