#data cleaning
data = read.csv('/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisData.csv', stringsAsFactors = F)
library(stringr)
names = names(data)
numeric = ""
for (i in 1:91){
  numeric[i] = is.numeric(data[,i])
}
logistic = ""
for (i in 1:91){
  logistic[i] = is.logical(data[,i])
}
rm(i)
glimpse = data.frame(cbind(numeric, logistic, t(data[1,])))
amenities = data$amenities
data = data[,-1]
data = data[,-1:-11]
data[,1] = as.Date(data[,1])
data = data[,c(-3,-6)]
data[which(data[,4] == "N/A"),4] = "0%"
data[which(data[,5] == "f"),5] = FALSE
data[which(data[,5] == "t"),5] = TRUE
data[is.na(data[,6]),6] = "N/A"
data[is.na(data[,7]),7] = "0"
data[is.na(data[,8]),8] = "0"
data = data[,-9]
data[which(data[,9] == "f"),9] = FALSE
data[which(data[,9] == "t"),9] = TRUE
data[which(data[,10] == "f"),10] = FALSE
data[which(data[,10] == "t"),10] = TRUE
data[which(data[,22] == "f"),22] = FALSE
data[which(data[,22] == "t"),22] = TRUE
data = data[,-30]
data = data[,-30]
price = data$price
data = data[,-30:-32]
data[is.na(data[,30]),30] = "0"
data[is.na(data[,31]),31] = "0"
data[which(data[,43] == "f"),43] = FALSE
data[which(data[,43] == "t"),43] = TRUE
data[,44] = data[,44]/30
data[,45] = data[,45]/60
data[,46] = data[,46]/90
data[,47] = data[,47]/365
data[,50] = as.Date(data[,50])
data[,51] = as.Date(data[,51])
library(dplyr)
data = mutate(data, reviewactivetime = last_review - first_review)
data = mutate(data, hosttime = Sys.Date() - host_since)
data = data[,-1]
data = data[,-58]
data = data[,-58]
data = data[,-58]
data[which(data[,58] == "f"),58] = FALSE
data[which(data[,58] == "t"),58] = TRUE
data = data[,-59]
data[which(data[,60] == "f"),60] = FALSE
data[which(data[,60] == "t"),60] = TRUE
data[which(data[,61] == "f"),61] = FALSE
data[which(data[,61] == "t"),61] = TRUE
data[is.na(data[,66]),66] = "0"
data = cbind(data,price)
for (i in 1:69) {
  print(i)
  print(sum(is.na(data[,i])))
}
names(data)
#
#
data[is.na(data[,27]),27] = "0"
data = data[-which(is.na(data[,67]) == T),]
data = data[-which(is.na(data[,68]) == T),]
for (i in 1:69) {
  print(sum(is.na(data[,i])))
}
write.csv(data, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv")

#continue cleaning
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv")
data = data[,-1]
data[,3] = str_replace(data[,3],"%","")
data[,3] = as.numeric(data[,3])/100
write.csv(data, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv")
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv", stringsAsFactors = F)
data = data[,-1]
data[str_detect(data$calendar_updated, "months"), 41] = "at least once updated"
data[str_detect(data$calendar_updated, "week"), 41] = "within a month"
data[str_detect(data$calendar_updated, "day"), 41] = "within a week"
data = data[,-c(1,5,10,11,12,14,18,49,50)]
write.csv(data, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv")
zipcode = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/zipcode.csv", stringsAsFactors = F)
colnames(data)[10] = "Zip"
data[which(data[,10] == "11385-2308"),10] = "11385"
data[which(data[,10] == "11413-3220"),10] = "11413"
data[which(data[,10] == "11249\n11249"),10] = "11249"
data[which(data[,10] == "1m"),10] = ""
data$Zip = as.integer(data$Zip)
data = zipcode %>%
  select(Zip, Latitude) %>%
  distinct() %>%
  right_join(data, by = "Zip")
data = zipcode %>%
  select(Zip, Longitude) %>%
  distinct() %>%
  right_join(data, by = "Zip")
data = data %>%
  mutate(distance_to_time_square = sqrt((Latitude-40.7580)^2+(Longitude+73.9855)^2)*111.7)
data = data[,-1:-3]
data = data[!is.na(data[,60]),]
write.csv(data, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisDataTree.csv")




### Generated predicted price using scoringData
scoringData = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/scoringData.csv", stringsAsFactors = F)
#cleaning scoringdata
scoringData = scoringData[,-1]
scoringData = scoringData[,-1:-11]
scoringData[,1] = as.Date(scoringData[,1])
scoringData = scoringData[,c(-3,-6)]
scoringData[which(scoringData[,4] == "N/A"),4] = "0%"
scoringData[which(scoringData[,5] == "f"),5] = FALSE
scoringData[which(scoringData[,5] == "t"),5] = TRUE
scoringData[is.na(scoringData[,6]),6] = "N/A"
scoringData[is.na(scoringData[,7]),7] = "0"
scoringData[is.na(scoringData[,8]),8] = "0"
scoringData = scoringData[,-9]
scoringData[which(scoringData[,9] == "f"),9] = FALSE
scoringData[which(scoringData[,9] == "t"),9] = TRUE
scoringData[which(scoringData[,10] == "f"),10] = FALSE
scoringData[which(scoringData[,10] == "t"),10] = TRUE
scoringData[which(scoringData[,22] == "f"),22] = FALSE
scoringData[which(scoringData[,22] == "t"),22] = TRUE
scoringData = scoringData[,-30]
scoringData = scoringData[,-30:-32]
scoringData[is.na(scoringData[,30]),30] = "0"
scoringData[is.na(scoringData[,31]),31] = "0"
scoringData[which(scoringData[,43] == "f"),43] = FALSE
scoringData[which(scoringData[,43] == "t"),43] = TRUE
scoringData[,44] = scoringData[,44]/30
scoringData[,45] = scoringData[,45]/60
scoringData[,46] = scoringData[,46]/90
scoringData[,47] = scoringData[,47]/365
scoringData[,50] = as.Date(scoringData[,50])
scoringData[,51] = as.Date(scoringData[,51])
scoringData = mutate(scoringData, reviewactivetime = last_review - first_review)
scoringData = mutate(scoringData, hosttime = Sys.Date() - host_since)
scoringData = scoringData[,-1]
scoringData = scoringData[,-58]
scoringData = scoringData[,-58]
scoringData = scoringData[,-58]
scoringData[which(scoringData[,58] == "f"),58] = FALSE
scoringData[which(scoringData[,58] == "t"),58] = TRUE
scoringData = scoringData[,-59]
scoringData[which(scoringData[,60] == "f"),60] = FALSE
scoringData[which(scoringData[,60] == "t"),60] = TRUE
scoringData[which(scoringData[,61] == "f"),61] = FALSE
scoringData[which(scoringData[,61] == "t"),61] = TRUE
scoringData[is.na(scoringData[,66]),66] = "0"
scoringData[is.na(scoringData[,27]),27] = "0"
scoringData[,3] = str_replace(scoringData[,3],"%","")
scoringData[,3] = as.numeric(scoringData[,3])/100
scoringData[str_detect(scoringData$calendar_updated, "months"), 41] = "at least once updated"
scoringData[str_detect(scoringData$calendar_updated, "week"), 41] = "within a month"
scoringData[str_detect(scoringData$calendar_updated, "day"), 41] = "within a week"
scoringData = scoringData[,-c(1,5,10,11,12,14,18,49,50)]
zipcode = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/zipcode.csv", stringsAsFactors = F)
colnames(scoringData)[10] = "Zip"
scoringData[which(scoringData[,10] == "11385-2308"),10] = "11385"
scoringData[which(scoringData[,10] == "11413-3220"),10] = "11413"
scoringData[which(scoringData[,10] == "11249\n11249"),10] = "11249"
scoringData[which(scoringData[,10] == "1m"),10] = ""
scoringData$Zip = as.integer(scoringData$Zip)
scoringData = zipcode %>%
  select(Zip, Latitude) %>%
  distinct() %>%
  right_join(scoringData, by = "Zip")
scoringData = zipcode %>%
  select(Zip, Longitude) %>%
  distinct() %>%
  right_join(scoringData, by = "Zip")
scoringData = scoringData %>%
  mutate(distance_to_time_square = sqrt((Latitude-40.7580)^2+(Longitude+73.9855)^2)*111.7)
scoringData = scoringData[,-1:-3]
scoringData[is.na(scoringData[,59]),59] = median(data$distance_to_time_square)
scoringData[,57] = as.integer(scoringData[,57])
scoringData[,58] = as.integer(scoringData[,58])
scoringData[,1] = as.factor(scoringData[,1])
scoringData[,3] = as.logical(scoringData[,3])
scoringData[,4] = as.integer(scoringData[,4])
scoringData[,5] = as.integer(scoringData[,5])
scoringData[,6] = as.logical(scoringData[,6])
scoringData[,7] = as.logical(scoringData[,7])
scoringData[,8] = as.factor(scoringData[,8])
scoringData[,9] = as.factor(scoringData[,9])
scoringData[,10] = as.factor(scoringData[,10])
scoringData[,11] = as.factor(scoringData[,11])
scoringData[,12] = as.factor(scoringData[,12])
scoringData[,13] = as.logical(scoringData[,13])
scoringData[,14] = as.factor(scoringData[,14])
scoringData[,15] = as.factor(scoringData[,15])
scoringData[,19] = as.integer(scoringData[,19])
scoringData[,20] = as.factor(scoringData[,20])
scoringData[,21] = as.integer(scoringData[,21])
scoringData[,22] = as.integer(scoringData[,22])
scoringData[,33] = as.factor(scoringData[,33])
scoringData[,34] = as.logical(scoringData[,34])
scoringData[,48] = as.logical(scoringData[,48])
scoringData[,49] = as.factor(scoringData[,49])
scoringData[,50] = as.logical(scoringData[,50])
scoringData[,51] = as.logical(scoringData[,51])
scoringData[,59] = as.integer(scoringData[,59])
scoringData[scoringData[,1] == "",1] = "N/A"
scoringData = droplevels(scoringData)
scoringData[scoringData[,9] == "MP",9] = "Ny"
scoringData[scoringData[,10] == "Catskills and Hudson Valley",10] = "New York"
scoringData[scoringData[,10] == "San Francisco",10] = "Other (Domestic)"
scoringData[scoringData[,10] == "London",10] = ""
scoringData = droplevels(scoringData)
scoringData[scoringData[,14] == "Farm stay",14] = "Earth house"
scoringData[scoringData[,14] == "Casa particular (Cuba)",14] = "Other"
scoringData[scoringData[,14] == "Castle",14] = "Boutique hotel"
scoringData = droplevels(scoringData)
for (i in 1:59) {
  print(i)
  print(mode(data[,i]) == mode(scoringData[,i]))
}
levels(scoringData[,1]) %in% levels(data[,1])
levels(scoringData[,8]) %in% levels(data[,8])
levels(scoringData[,9]) %in% levels(data[,9])
levels(scoringData[,10]) %in% levels(data[,10])
levels(scoringData[,11]) %in% levels(data[,11])
levels(scoringData[,12]) %in% levels(data[,12])
levels(scoringData[,14]) %in% levels(data[,14])
levels(scoringData[,15]) %in% levels(data[,15])
levels(scoringData[,20]) %in% levels(data[,20])
levels(scoringData[,33]) %in% levels(data[,33])
levels(scoringData[,49]) %in% levels(data[,49])
id = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/scoringData.csv", stringsAsFactors = F)
id = id[,1]
scoringData = cbind(scoringData,id)
for (i in 1:60) {
  print(sum(is.na(scoringData[,i])))
}
scoringData[is.na(scoringData[,2]),2] = 0
scoringData[is.na(scoringData[,3]),3] = FALSE
scoringData[is.na(scoringData[,6]),6] = FALSE
scoringData[is.na(scoringData[,7]),7] = FALSE
scoringData[is.na(scoringData[,58]),58] = 2800













#select variables
data = data[,c(1,2,3,4,5,8,13,14,15,16,17,18,19,20,23,24,27:48,52:54,56,59)]
#processing xgdata
train = length(data$host_response_time)
price = rep(0,9210)
xgtest = cbind(scoringData[,1:58],price,scoringData[,59])
xgtest = xgtest[,c(1,2,3,4,5,8,13,14,15,16,17,18,19,20,23,24,27:48,52:54,56,59,60)]
colnames(xgtest)[44] = "distance_to_time_square"
price = data$price
n = rbind(data,xgtest)
n = n[,-c(11,12)]
xgdata = model.matrix(price~.,n)[,-43]
#running model
xgboost = xgboost(data = xgdata[1:train,], label = price, nrounds = 50)
#predict
xgtest = n[(train+1):length(n$host_response_time),]
xgtest = model.matrix(price~.,n)[,-43]
pred = predict(xgboost, newdata = xgtest)
pred = pred[36383:length(n$host_response_time)]
### Construct submision from predictions
submissionFile_simplelm = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile_simplelm, '/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/submissionFile.csv',row.names = F)




