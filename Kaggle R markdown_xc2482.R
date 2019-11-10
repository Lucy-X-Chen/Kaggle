# clear environment and set working directory
rm(list =ls())
setwd("C:/Users/lucyc/Downloads/Kaggle")
library(base)
library(stats)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(lm.beta)
library(graphics) 
library(grDevices)
library(utils)
library(datasets)
library(methods) 
library(car)
library(leaps)
library(vtreat)
library(ranger)
library(caTools)
library(rpart)
library(rpart.plot)
library(doBy)
library(ROCR)
library(ISLR)
library(pROC)
library(e1071)
library(randomForest)
# load analysis data
data = read.csv('analysisData.csv')
dim(data)
# load scoring data
scoringData = read.csv('scoringData.csv')
#cleaning dollar sign//
head(data$price)
head(as.numeric(gsub('[$,]', '', data$price)))
data$price = as.numeric(gsub('[$,]', '', data$price))

head(data$cleaning_fee)
head(as.numeric(gsub('[$,]', '', data$cleaning_fee)))
data$cleaning_fee = as.numeric(gsub('[$,]', '', data$cleaning_fee))
head(scoringData$cleaning_fee)
head(as.numeric(gsub('[$,]', '', scoringData$cleaning_fee)))
scoringData$cleaning_fee = as.numeric(gsub('[$,]', '', scoringData$cleaning_fee))

head(data$security_deposit)
head(as.numeric(gsub('[$,]', '', data$security_deposit)))
data$security_deposit = as.numeric(gsub('[$,]', '', data$security_deposit))
head(scoringData$security_deposit)
head(as.numeric(gsub('[$,]', '', scoringData$security_deposit)))
scoringData$security_deposit = as.numeric(gsub('[$,]', '', scoringData$security_deposit))

head(data$extra_people)
head(as.numeric(gsub('[$,]', '', data$extra_people)))
data$extra_people = as.numeric(gsub('[$,]', '', data$extra_people))
head(scoringData$extra_people)
head(as.numeric(gsub('[$,]', '', scoringData$extra_people)))
scoringData$extra_people = as.numeric(gsub('[$,]', '', scoringData$extra_people))

head(data$weekly_price)
head(as.numeric(gsub('[$,]', '', data$weekly_price)))
data$weekly_price = as.numeric(gsub('[$,]', '', data$weekly_price))
head(scoringData$weekly_price)
head(as.numeric(gsub('[$,]', '', scoringData$weekly_price)))
scoringData$weekly_price = as.numeric(gsub('[$,]', '', scoringData$weekly_price))

head(data$monthly_price)
head(as.numeric(gsub('[$,]', '', data$monthly_price)))
data$monthly_price = as.numeric(gsub('[$,]', '', data$monthly_price))
head(scoringData$monthly_price)
head(as.numeric(gsub('[$,]', '', scoringData$monthly_price)))
scoringData$monthly_price = as.numeric(gsub('[$,]', '', scoringData$monthly_price))
#Mask NA with median
median_review_scores_rating = median(data$review_scores_rating, na.rm = T)
median_bathrooms= median(data$bathrooms,   na.rm = T)
median_bedrooms= median(data$bedrooms,   na.rm = T)
median_beds= median(data$beds,   na.rm = T)
median_cleaning_fee= median(data$cleaning_fee,  na.rm = T)
median_security_deposit = median(data$security_deposit,  na.rm = T)
median_weekly_price= median(data$weekly_price,  na.rm = T)
median_monthly_price = median(data$monthly_price,  na.rm = T)
median_reviews_per_month = median(data$reviews_per_month,  na.rm = T)
median_host_listings_count = median(data$host_listings_count,  na.rm = T)
median_review_scores_accuracy= median(data$review_scores_accuracy,  na.rm = T)
median_review_scores_cleanliness= median(data$review_scores_cleanliness,  na.rm = T)
median_review_scores_checkin= median(data$review_scores_checkin,  na.rm = T)
median_review_scores_communication= median(data$review_scores_communication,  na.rm = T)
median_review_scores_location= median(data$review_scores_location,  na.rm = T)
median_review_scores_value= median(data$review_scores_value,  na.rm = T)

mask_NAs = is.na(data$review_scores_rating)
data[mask_NAs, 'review_scores_rating'] = median_review_scores_rating
sum(is.na(data$review_scores_rating)) == 0
median(data$review_scores_rating, na.rm = T) == median_review_scores_rating

mask_NAs_bathrooms = is.na(data$bathrooms)
data[mask_NAs_bathrooms, 'bathrooms'] = median_bathrooms
sum(is.na(data$bathrooms)) == 0
median(data$bathrooms, na.rm = T) == median_bathrooms

mask_NAs_bedrooms = is.na(data$bedrooms)
data[mask_NAs_bedrooms, 'bedrooms'] = median_bedrooms
sum(is.na(data$bedrooms)) == 0
median(data$bedrooms, na.rm = T) == median_bedrooms

mask_NAs_beds = is.na(data$beds)
data[mask_NAs_beds, 'beds'] = median_beds
sum(is.na(data$beds)) == 0
median(data$beds, na.rm = T) == median_beds

mask_NAs_cleaning_fee = is.na(data$cleaning_fee)
data[mask_NAs_cleaning_fee, 'cleaning_fee'] = median_cleaning_fee
sum(is.na(data$cleaning_fee)) == 0
median(data$cleaning_fee, na.rm = T) == median_cleaning_fee

mask_NAs_security_deposit = is.na(data$security_deposit)
data[mask_NAs_security_deposit, 'security_deposit'] = median_security_deposit
sum(is.na(data$security_deposit)) == 0
median(data$security_deposit, na.rm = T) == median_security_deposit

mask_NAs_weekly_price = is.na(data$weekly_price)
data[mask_NAs_weekly_price, 'weekly_price'] = median_weekly_price
sum(is.na(data$weekly_price)) == 0
median(data$weekly_price, na.rm = T) == median_weekly_price

mask_NAs_monthly_price = is.na(data$monthly_price)
data[mask_NAs_monthly_price, 'monthly_price'] = median_monthly_price
sum(is.na(data$monthly_price)) == 0
median(data$monthly_price, na.rm = T) == median_monthly_price

mask_NAs_reviews_per_month = is.na(data$reviews_per_month)
data[mask_NAs_reviews_per_month, 'reviews_per_month'] = median_reviews_per_month
sum(is.na(data$reviews_per_month)) == 0
median(data$reviews_per_month, na.rm = T) == median_reviews_per_month

mask_NAs_host_listings_count = is.na(data$host_listings_count)
data[mask_NAs_host_listings_count, 'host_listings_count'] = median_host_listings_count
sum(is.na(data$host_listings_count)) == 0
median(data$host_listings_count, na.rm = T) == median_host_listings_count

mask_NAs_review_scores_accuracy = is.na(data$review_scores_accuracy)
data[mask_NAs_review_scores_accuracy, 'review_scores_accuracy'] = median_review_scores_accuracy
sum(is.na(data$review_scores_accuracy)) == 0
median(data$review_scores_accuracy, na.rm = T) == median_review_scores_accuracy

mask_NAs_review_scores_cleanliness = is.na(data$review_scores_cleanliness)
data[mask_NAs_review_scores_cleanliness, 'review_scores_cleanliness'] = median_review_scores_cleanliness
sum(is.na(data$review_scores_cleanliness)) == 0
median(data$review_scores_cleanliness, na.rm = T) == median_review_scores_cleanliness

mask_NAs_review_scores_checkin = is.na(data$review_scores_checkin)
data[mask_NAs_review_scores_checkin, 'review_scores_checkin'] = median_review_scores_checkin
sum(is.na(data$review_scores_checkin)) == 0
median(data$review_scores_checkin, na.rm = T) == median_review_scores_checkin

mask_NAs_review_scores_communication = is.na(data$review_scores_communication)
data[mask_NAs_review_scores_communication, 'review_scores_communication'] = median_review_scores_communication
sum(is.na(data$review_scores_communication)) == 0
median(data$review_scores_communication, na.rm = T) == median_review_scores_communication

mask_NAs_review_scores_location = is.na(data$review_scores_location)
data[mask_NAs_review_scores_location, 'review_scores_location'] = median_review_scores_location
sum(is.na(data$review_scores_location)) == 0
median(data$review_scores_location, na.rm = T) == median_review_scores_location

mask_NAs_review_scores_value = is.na(data$review_scores_value)
data[mask_NAs_review_scores_value, 'review_scores_value'] = median_review_scores_value
sum(is.na(data$review_scores_value)) == 0
median(data$review_scores_value, na.rm = T) == median_review_scores_value

#Mask NA in ScoringData
mask_NAs = is.na(scoringData$review_scores_rating)
scoringData[mask_NAs, 'review_scores_rating'] = median_review_scores_rating
sum(is.na(scoringData$review_scores_rating)) == 0

mask_NAs_bathrooms = is.na(scoringData$bathrooms)
scoringData[mask_NAs_bathrooms, 'bathrooms'] = median_bathrooms
sum(is.na(scoringData$bathrooms)) == 0

mask_NAs_bedrooms = is.na(scoringData$bedrooms)
scoringData[mask_NAs_bedrooms, 'bedrooms'] = median_bedrooms
sum(is.na(scoringData$bedrooms)) == 0

mask_NAs_beds = is.na(scoringData$beds)
scoringData[mask_NAs_beds, 'beds'] = median_beds
sum(is.na(scoringData$beds)) == 0

mask_NAs_cleaning_fee = is.na(scoringData$cleaning_fee)
scoringData[mask_NAs_cleaning_fee, 'cleaning_fee'] = median_cleaning_fee
sum(is.na(scoringData$cleaning_fee)) == 0

mask_NAs_security_deposit = is.na(scoringData$security_deposit)
scoringData[mask_NAs_security_deposit, 'security_deposit'] = median_security_deposit
sum(is.na(scoringData$security_deposit)) == 0

mask_NAs_weekly_price = is.na(scoringData$weekly_price)
scoringData[mask_NAs_weekly_price, 'weekly_price'] = median_weekly_price
sum(is.na(scoringData$weekly_price)) == 0

mask_NAs_monthly_price = is.na(scoringData$monthly_price)
scoringData[mask_NAs_monthly_price, 'monthly_price'] = median_monthly_price
sum(is.na(scoringData$monthly_price)) == 0

mask_NAs_reviews_per_month = is.na(scoringData$reviews_per_month)
scoringData[mask_NAs_reviews_per_month, 'reviews_per_month'] = median_reviews_per_month
sum(is.na(scoringData$reviews_per_month)) == 0

mask_NAs_host_listings_count = is.na(scoringData$host_listings_count)
scoringData[mask_NAs_host_listings_count, 'host_listings_count'] = median_host_listings_count
sum(is.na(scoringData$host_listings_count)) == 0

mask_NAs_review_scores_accuracy = is.na(scoringData$review_scores_accuracy)
scoringData[mask_NAs_review_scores_accuracy, 'review_scores_accuracy'] = median_review_scores_accuracy
sum(is.na(scoringData$review_scores_accuracy)) == 0

mask_NAs_review_scores_cleanliness = is.na(scoringData$review_scores_cleanliness)
scoringData[mask_NAs_review_scores_cleanliness, 'review_scores_cleanliness'] = median_review_scores_cleanliness
sum(is.na(scoringData$review_scores_cleanliness)) == 0

mask_NAs_review_scores_checkin = is.na(scoringData$review_scores_checkin)
scoringData[mask_NAs_review_scores_checkin, 'review_scores_checkin'] = median_review_scores_checkin
sum(is.na(scoringData$review_scores_checkin)) == 0

mask_NAs_review_scores_communication = is.na(scoringData$review_scores_communication)
scoringData[mask_NAs_review_scores_communication, 'review_scores_communication'] = median_review_scores_communication
sum(is.na(scoringData$review_scores_communication)) == 0

mask_NAs_review_scores_location = is.na(scoringData$review_scores_location)
scoringData[mask_NAs_review_scores_location, 'review_scores_location'] = median_review_scores_location
sum(is.na(scoringData$review_scores_location)) == 0

mask_NAs_review_scores_value = is.na(scoringData$review_scores_value)
scoringData[mask_NAs_review_scores_value, 'review_scores_value'] = median_review_scores_value
sum(is.na(scoringData$review_scores_value)) == 0
#Check NA
sum(is.na(data$price))
sum(is.na(data$minimum_nights))
sum(is.na(data$review_scores_rating))
sum(is.na(data$accommodates))
sum(is.na(data$guests_included))
sum(is.na(data$bathrooms))
sum(is.na(data$bedrooms))
sum(is.na(data$room_type))
sum(is.na(data$number_of_reviews))
sum(is.na(data$reviews_per_month))
sum(is.na(data$neighbourhood_group_cleansed))
sum(is.na(data$cleaning_fee))
sum(is.na(data$security_deposit))
sum(is.na(data$weekly_price))
sum(is.na(data$monthly_price))
sum(is.na(data$extra_people))
sum(is.na(data$review_scores_accuracy))
sum(is.na(data$review_scores_cleanliness))
sum(is.na(data$review_scores_checkin))
sum(is.na(data$review_scores_communication))
sum(is.na(data$review_scores_location))
sum(is.na(data$review_scores_value))
#Amenity -??? changed long description to #character
data$amenities= nchar(as.character(data$amenities))
scoringData$amenities=nchar(as.character(scoringData$amenities))
#Find Correlation
options(max.print=1000000)
cor(data[sapply(data, is.numeric)])
#Split Data
library(caTools)
set.seed(199)
split = sample.split(data$price,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

#Ranger Model on train data
model= ranger(price ~ accommodates+amenities+availability_30+availability_60+bathrooms+bedrooms+calculated_host_listings_count+cancellation_policy+cleaning_fee+extra_people+guests_included+host_listings_count+host_response_time+instant_bookable+longitude+market+minimum_nights+monthly_price+neighbourhood_cleansed+neighbourhood_group_cleansed+number_of_reviews+property_type+require_guest_phone_verification+review_scores_accuracy+review_scores_cleanliness+review_scores_location+review_scores_rating+reviews_per_month+room_type+security_deposit+weekly_price, train)

#test RMSE on test dataset

pred = predict(model, test)
sse = sum((pred$predictions - test$price)^2)
sst = sum((mean(test$price)-test$price)^2)
model_r2 = 1 - sse/sst; model_r2
rmse = sqrt(mean((pred$predictions-test$price)^2)); rmse
#Ranger Model on full dataset
model= ranger(price ~ accommodates+amenities+availability_30+availability_60+bathrooms+bedrooms+calculated_host_listings_count+cancellation_policy+cleaning_fee+extra_people+guests_included+host_listings_count+host_response_time+instant_bookable+longitude+market+minimum_nights+monthly_price+neighbourhood_cleansed+neighbourhood_group_cleansed+number_of_reviews+property_type+require_guest_phone_verification+review_scores_accuracy+review_scores_cleanliness+review_scores_location+review_scores_rating+reviews_per_month+room_type+security_deposit+weekly_price, data)
pred = predict(model, scoringData)
submission = data.frame(id = scoringData$id, price = pred$predictions) 
sum(is.na(submission)) == 0
write.csv(submission, 'sample_submission.csv',row.names = F)

#Additional Test through Feature Selection
#Hybride test on good factors
start_mod = lm(price ~ 1,data=data)
empty_mod = lm(price~1,data=data)
full_mod = lm(price ~ last_scraped + host_response_time + host_is_superhost + host_listings_count + host_has_profile_pic + host_identity_verified + neighbourhood_group_cleansed + market + latitude + longitude + is_location_exact + property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + guests_included + minimum_nights + maximum_nights + availability_30 + availability_60 + availability_90 + availability_365 + calendar_last_scraped + number_of_reviews + review_scores_rating + review_scores_accuracy + review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location + review_scores_value + instant_bookable + cancellation_policy + require_guest_profile_picture + require_guest_phone_verification + calculated_host_listings_count + reviews_per_month,data=data)
hybridStepwise = step(start_mod, scope=list(upper=full_mod, lower=empty_mod), direction='both')
summary(hybridStepwise)

model9=ranger(price ~ accommodates + neighbourhood_group_cleansed +  bathrooms + property_type + room_type + longitude + availability_30 + reviews_per_month + review_scores_location + bedrooms + cancellation_policy + beds + review_scores_cleanliness + require_guest_phone_verification + calculated_host_listings_count + host_listings_count + availability_60 +  last_scraped + review_scores_checkin + host_response_time + instant_bookable + minimum_nights + guests_included + host_has_profile_pic +    host_is_superhost + number_of_reviews + availability_365 + latitude + review_scores_accuracy + review_scores_communication + require_guest_profile_picture, data = train)
