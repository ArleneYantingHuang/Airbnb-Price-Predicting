data = read.csv('/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisData.csv')

#data clearning
str(data)
names(data)
head(data)
for (i in 1:91){
  data[which(data[,i]=="N/A",),i] = ""
}

names = names(data)
b = paste0(names,"+")
c = ""
for (i in 1:91) {
  c = paste(c, b[i])
}
for (i in 1:91){
  numeric[i] = is.numeric(data[,i])
}
predictors = data.frame(names, numeric)
numericpredictors = names[numeric]
numericpredictors = numericpredictors[-9]
data = cbind(price = data$price, data[numericpredictors])

write.csv(data, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataClean.csv")


#readdata
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataClean.csv")
data = data[,-1]


#featureselection
library(leaps)
subsets = regsubsets(price~.,data=data, nvmax=31)
summary(subsets)
names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures
which.min(subsets_measures$cp)
a = data.frame(summary(subsets)[1])[25,]
a = a[,a[1,] == T]
names = names(a)
names = str_replace(names, "which.", "")
names = names[-1]
names
b = paste0(names,"+")
c = ""
for (i in 1:91) {
  c = paste(c, b[i])
}

regmodel1 = lm(price~id+ accommodates+ bathrooms+ guests_included+ extra_people+ minimum_nights+ maximum_nights+ maximum_minimum_nights+ minimum_maximum_nights+ maximum_maximum_nights+ minimum_nights_avg_ntm+ maximum_nights_avg_ntm+ availability_365+ number_of_reviews+ number_of_reviews_ltm+ review_scores_rating+ review_scores_accuracy+ review_scores_cleanliness+ review_scores_checkin+ review_scores_communication+ review_scores_location+ review_scores_value+ calculated_host_listings_count+ calculated_host_listings_count_private_rooms+ calculated_host_listings_count_shared_rooms, data = data)
summary(regmodel1)

# Predict
pred = predict(regmodel1)
# Calculate RMSE
rmse_analysis = sqrt(mean((pred-data$price)^2))
rmse_analysis
### Generated predicted price using scoringData
pred = predict(regmodel1, newdata=scoringData)
### Construct submision from predictions
submissionFile_simplelm = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile_simplelm, 'submissionFile.csv',row.names = F)



#decision tree
#readdata
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataClean.csv")
data = data[,-1]
library(rpart)
library(rpart.plot)
set.seed(100)
tree1 = rpart(price~., data = data)
summary(tree1)
rpart.plot(tree1)
# Predict
pred = predict(tree1)
# Calculate RMSE
rmse_analysis = sqrt(mean((pred-data$price)^2))
rmse_analysis
### Generated predicted price using scoringData
pred = predict(tree1, newdata=scoringData)
### Construct submision from predictions
submissionFile_simplelm = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile_simplelm, 'submissionFile=.csv',row.names = F)





#ramdom forest
#readdata
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataClean.csv")
data = data[,-1]
data = data[,-c(10,11)]
for (i in 1:39) {
  data[is.na(data[,i]),i] = 0
}
library(randomForest)
#set.seed(1213)
#split = createDataPartition(data$price, p = 0.8, list = F)
#train = data[split,]
#test = data[-split,]
forest1 = randomForest(price~., data = data, ntree = 1000)
# Predict
pred = predict(forest1)
# Calculate RMSE
rmse_analysis = sqrt(mean((pred-data$price)^2))
rmse_analysis
### Generated predicted price using scoringData
scoringData = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/scoringData.csv")
for (i in 1:90) {
  scoringData[which(scoringData[,i] == "N/A"),i] = 0
}
for (i in 1:90) {
  scoringData[which(scoringData[,i] == NA),i] = 0
}
pred = predict(forest1, newdata=scoringData)
### Construct submision from predictions
submissionFile_simplelm = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile_simplelm, '/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/submissionFile.csv',row.names = F)



#Bagging
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataClean.csv")
data = data[,-1]
for (i in 1:41) {
  data[is.na(data[,i]),i] = 0
}
set.seed(120)
bag = randomForest(price~.,data = data, mtry = ncol(data)-1, ntree = 1000)
# Predict
pred = predict(bag)
# Calculate RMSE
rmse_analysis = sqrt(mean((pred-data$price)^2))
rmse_analysis
### Generated predicted price using scoringData
scoringData = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/scoringData.csv")
for (i in 1:90) {
  scoringData[which(scoringData[,i] == "N/A"),i] = 0
}
for (i in 1:90) {
  scoringData[is.na(scoringData[,i]),i] = 0
}
pred = predict(bag, newdata=scoringData)
### Construct submision from predictions
submissionFile_simplelm = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile_simplelm, '/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/submissionFile.csv',row.names = F)


