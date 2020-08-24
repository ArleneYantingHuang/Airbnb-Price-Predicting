#data clearning
#combine data together
data = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/analysisData.csv")
scoringData = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/scoringData.csv")
price = rep(0,length(scoringData$id))
scoringData = cbind(scoringData[1:46],price,scoringData[47:90])
comboData = rbind(data,scoringData)
rm(price)
#preperation of cleaning  data
library(stringr)
library(dplyr)
id = comboData$id
write.csv(id, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/comboid.csv")
rm(id)
price = comboData$price
write.csv(price, "/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/comboprice.csv")
rm(price)
comboData = comboData[,-47]
#create cleaned vectors
name_luxury = str_detect(comboData$name, pattern = "Luxury") | str_detect(comboData$name, pattern = "luxury")
name_Manhattan = str_detect(comboData$name, pattern = "Manhattan") | str_detect(comboData$name, pattern = "manhattan")
name_Brooklyn = str_detect(comboData$name, pattern = "Brooklyn") | str_detect(comboData$name, pattern = "brooklyn")
name_Midtown = str_detect(comboData$name, pattern = "Midtown") | str_detect(comboData$name, pattern = "midtown")
name_Modern = str_detect(comboData$name, pattern = "Modern") | str_detect(comboData$name, pattern = "modern")
name_Spacious = str_detect(comboData$name, pattern = "Spacious") | str_detect(comboData$name, pattern = "spacious")
transit_subway = str_detect(comboData$name, pattern = "Subway") | str_detect(comboData$name, pattern = "subway")
host_time = as.numeric(Sys.Date() - comboData$host_since)
host_time[is.na(host_time)] = 0
host_location = str_split(comboData$host_location, pattern = ",")
hostlocation = data.frame()
for (i in 1:length(comboData$id)) {
  if (length(host_location[[i]]) == 3) {
    hostlocation[i,1] = host_location[[i]][1]
    hostlocation[i,2] = host_location[[i]][2]
    hostlocation[i,3] = host_location[[i]][3]
  }
  else if (length(host_location[[i]]) == 2){
    hostlocation[i,1] = NA
    hostlocation[i,2] = host_location[[i]][1]
    hostlocation[i,3] = host_location[[i]][2]
  }
  else if (length(host_location[[i]]) == 1){
    hostlocation[i,1] = NA
    hostlocation[i,2] = NA
    hostlocation[i,3] = host_location[[i]][1]
  }
}
rm(host_location)
colnames(hostlocation) = c("host_district","host_region","host_nation")
host_response_time = comboData$host_response_time
host_response_time[which(host_response_time == "")] = "N/A"
host_response_time[which(host_response_time == "N/A")] = NA
host_response_time = droplevels(host_response_time)
host_response_rate = str_replace_all(comboData$host_response_rate,"%","")
host_response_rate[which(host_response_rate == "N/A")] = NA
host_response_rate = as.numeric(host_response_rate)/100
superhost = as.character(comboData$host_is_superhost)
superhost[which(superhost == "f")] = FALSE
superhost[which(superhost == "t")] = TRUE
superhost[which(superhost == "")] = NA
host_listing_count = comboData$host_listings_count
host_total_listing_count = comboData$host_total_listings_count
host_neigoborhood = comboData$host_neighbourhood
host_neigoborhood[which(host_neigoborhood == "")] = NA
host_has_profile_pic = as.character(comboData$host_has_profile_pic)
host_has_profile_pic[which(host_has_profile_pic == "f")] = FALSE
host_has_profile_pic[which(host_has_profile_pic == "t")] = TRUE
host_has_profile_pic[which(host_has_profile_pic == "")] = NA
host_identity_verified = as.character(comboData$host_identity_verified)
host_identity_verified[which(host_identity_verified == "f")] = FALSE
host_identity_verified[which(host_identity_verified == "t")] = TRUE
host_identity_verified[which(host_identity_verified == "")] = NA
colnames(street) = c("district","region","nation")
neighborhood_cleansed = comboData$neighbourhood_cleansed
neighborhood_group_cleansed = comboData$neighbourhood_group_cleansed
city = comboData$city
state = comboData$state
zipcode = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/zipcode.csv", stringsAsFactors = F)
Zip = data.frame(Zip = as.numeric(as.character(comboData$zipcode)))
Zip = zipcode %>%
  select(Zip, Latitude) %>%
  distinct() %>%
  right_join(Zip, by = "Zip")
Zip = zipcode %>%
  select(Zip, Longitude) %>%
  distinct() %>%
  right_join(Zip, by = "Zip")
Zip = Zip %>%
  mutate(distance_to_time_square = sqrt((Latitude-40.7580)^2+(Longitude+73.9855)^2)*111.7)
distance_to_time_square = Zip$distance_to_time_square
rm(zipcode,Zip)
market = comboData$market
smart_location = comboData$smart_location
is_location_exact = as.character(comboData$is_location_exact)
is_location_exact[which(is_location_exact == "f")] = FALSE
is_location_exact[which(is_location_exact == "t")] = TRUE
is_location_exact[which(is_location_exact == "")] = NA
property_type = comboData$property_type
room_type = comboData$room_type
accommodates = comboData$accommodates
bathrooms = comboData$bathrooms
bedrooms = comboData$bedrooms
beds = comboData$beds



#create cleaned dataframe
cleanData = data.frame(name_Brooklyn,name_luxury,name_Manhattan,name_Midtown,name_Modern,name_Spacious,transit_subway,host_time,hostlocation,host_response_time,host_response_rate,superhost,host_listing_count,host_total_listing_count,host_neigoborhood,host_has_profile_pic,neighborhood_cleansed,neighborhood_group_cleansed,city,state,distance_to_time_square,market,smart_location,is_location_exact,property_type,room_type,accommodates,bathrooms,bedrooms,beds)









#random forest
library(randomForest)
train = cleanData[1:length(data$id),]
price = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/comboprice.csv")[1:length(data$id),2]
train = cbind(price, train)
forest = randomForest(price~bathrooms+bedrooms+beds, data = train, ntree = 100)
pred = predict(forest)
rmse_analysis = sqrt(mean((pred-price)^2))
rmse_analysis

#xgboost
train = cleanData[1:length(data$id),]
price = read.csv("/Users/arlenehuang/OneDrive - Columbia University/Fall 2019/APAN classes/APAN 5200/Kaggle/comboprice.csv")[1:length(data$id),2]
library(xgboost)
train = cbind(train, price)
xgdata = model.matrix(price~bathrooms+bedrooms+beds,train)[,-33]
price = train$price
xgboost = xgboost(data = xgdata[1:10,], label = price[10], nrounds = 5)



for (i in 1:91) {
  if (nlevels(data[,i])>10) {
    print(i)
    print(nlevels(data[,i]))
  }
}



