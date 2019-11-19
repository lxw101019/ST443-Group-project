rm(list = ls())

# SET YOUR WORKING DIRECTORY TOWARDS THE ONE IN WHICH THIS SCRIPT AND THE CSV FILE ARE
setwd("~/Documents/Rworkspace/ST443")

# IF NOT INSTALLED, FIRST:
# install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

amsterdam <- read_csv('listingsamsterdam.csv')

amsterdam <- select(amsterdam, price, review_scores_rating, host_since, 
                    host_is_superhost, neighbourhood_cleansed, host_listings_count,
                    host_identity_verified, room_type,
                    bathrooms, bedrooms, minimum_nights,
                    number_of_reviews, cancellation_policy, instant_bookable, accommodates,
                    weekly_price, monthly_price, cleaning_fee)

# Data transformation
amsterdam$price <- as.numeric(gsub('\\$|,', '', amsterdam$price))
amsterdam$weekly_price <- as.numeric(gsub('\\$|,', '', amsterdam$weekly_price))
amsterdam$monthly_price <- as.numeric(gsub('\\$|,', '', amsterdam$monthly_price))
amsterdam$cleaning_fee <- as.numeric(gsub('\\$|,', '', amsterdam$cleaning_fee))

amsterdam$host_since <- as.Date(amsterdam$host_since)
amsterdam <- mutate(amsterdam, host_is_superhost = ifelse(host_is_superhost == TRUE, 1 , 0)) 
amsterdam <- mutate(amsterdam, host_identity_verified = ifelse(host_identity_verified == TRUE, 1 , 0)) 
amsterdam <- mutate(amsterdam, near_centre = 
                      ifelse(neighbourhood_cleansed == 'Centrum-West' | 
                               neighbourhood_cleansed == 'Centrum-Oost' |
                               neighbourhood_cleansed == 'Zuid', 1, 0))
amsterdam <- mutate(amsterdam, far_from_centre = 
                      ifelse(neighbourhood_cleansed == 'Bijlmer-Oost' | 
                               neighbourhood_cleansed == 'Gaasperdam - Driemond' |
                               neighbourhood_cleansed == 'Bijlmer-Centrum' | 
                               neighbourhood_cleansed == 'Osdorp' |
                               neighbourhood_cleansed == 'Geuzenveld - Slotermeer' | 
                               neighbourhood_cleansed == 'Slotervaart' |
                               neighbourhood_cleansed == 'De Aker - Nieuw Sloten' | 
                               neighbourhood_cleansed == 'Bos en Lommer' |
                               neighbourhood_cleansed == 'Noord-Oost' | 
                               neighbourhood_cleansed == 'Noord-West' |
                               neighbourhood_cleansed == 'Oostelijk Havengebied - Indische Buurt', 1, 0))
#Michael: this is sufficient, without "far_from_centre" 
amsterdam$location_3ways <- ifelse(amsterdam$neighbourhood_cleansed == 'Centrum-West' | 
                                     amsterdam$neighbourhood_cleansed == 'Centrum-Oost' |
                                     amsterdam$neighbourhood_cleansed == 'Zuid', "near_centre",
                                   ifelse(amsterdam$neighbourhood_cleansed == 'Bijlmer-Oost' | 
                                            amsterdam$neighbourhood_cleansed == 'Gaasperdam - Driemond' |
                                            amsterdam$neighbourhood_cleansed == 'Bijlmer-Centrum' | 
                                            amsterdam$neighbourhood_cleansed == 'Osdorp' |
                                            amsterdam$neighbourhood_cleansed == 'Geuzenveld - Slotermeer' | 
                                            amsterdam$neighbourhood_cleansed == 'Slotervaart' |
                                            amsterdam$neighbourhood_cleansed == 'De Aker - Nieuw Sloten' | 
                                            amsterdam$neighbourhood_cleansed == 'Bos en Lommer' |
                                            amsterdam$neighbourhood_cleansed == 'Noord-Oost' | 
                                            amsterdam$neighbourhood_cleansed == 'Noord-West' |
                                            amsterdam$neighbourhood_cleansed == 'Oostelijk Havengebied - Indische Buurt', 
                                          "far_from_centre", "Moderate"))
#amsterdam <- mutate(amsterdam, bathrooms = bathrooms / accommodates) # Michael - as discussed with Lauren, will not change the variable
#amsterdam <- mutate(amsterdam, bedrooms = bedrooms / accommodates) # Michael - as discussed with Lauren, will not change the variable
amsterdam$realprice <- rep(NA, length(amsterdam$price))

for (i in 1:length(amsterdam$realprice)){
  if (amsterdam$minimum_nights[i] > 27){
    if (!is.na(amsterdam$monthly_price[i])){
    amsterdam$realprice[i] <- amsterdam$monthly_price[i]/30
    } else {
      amsterdam$realprice[i] <- amsterdam$price[i]  
    }
  } else if (amsterdam$minimum_nights[i] > 6) {
    if (!is.na(amsterdam$weekly_price[i])){
    amsterdam$realprice[i] <- amsterdam$weekly_price[i]/7
    } else {
      amsterdam$realprice[i] <- amsterdam$price[i]
    }
  } else {
  amsterdam$realprice[i] <- amsterdam$price[i]
  }
}

# review_scores_rating has 2565 NA's, 
# cleaning_fee 3611 NA's, removing those NAs after removing r_s_r NA's leads to a drop of ~2000 observations, maybe not worth it
# amsterdam <- filter(amsterdam, !is.na(cleaning_fee))

# 5 NAs in host_since, 5 NAs in host_is_superhost, 5 NAs in host_listingscount
# Host response rate and time have 8536 NA's, removed them 
# Experiences_offered contains only none, removed it
# 33 types of property, removed it

# Did not manage to convert host_verifications, removed it
# Mininum nights has maximum of 1001 (outlier I suppose)
# 5 types of cancellation policy
# 7NAs bathrooms, 14NAs bedrooms, 8 NAs beds


#drops <- c("weekly_price","monthly_price","neighbourhood_cleansed") #Michael - did not drop "neighbourhood_cleansed
drops <- c("weekly_price", "monthly_price")
amsterdam <- amsterdam[ , !(names(amsterdam) %in% drops)]

# Michael - Add in host's "age" on AirBnb
Date_scrap <- as.Date("14/09/19","%d/%m/%y")
amsterdam$host_since_duration <- Date_scrap-amsterdam$host_since
#-----------

#Michael- keep cleaning fee as a variable as it is an important variable (ols)

amsterdam <- drop_na(amsterdam) # Michael - will drop new listings (explained below)
check <- amsterdam[amsterdam$realprice < 1,] # Michael - There is an outlier with price = 0

amsterdam <- amsterdam[amsterdam$realprice > 1,]
#write.csv(amsterdam, 'st443amsterdamdata_new')
#amsterdam <- read_csv('st443amsterdamdata')
#amsterdam <- amsterdam[,-1]

amsterdam$logprice <- log(amsterdam$realprice)

#------------------------------------------------
# Michael
# Study of outliers:
#------------------------------------------------

# Two qns:
# 1. Qn --> Do we include "new" listings, where reviews were none, 
# therefore number of reviews = NA and were removed in the above??

# 2 - Based on analysis below, do we then just use the data as it is or 
# use the boxplots/histogram to set arbitrary cut-offs? 
# My preference is to use the data as it is, but log(price)

# Analysis
# Took out one outlier, wherer price = 0, as above

# Based on boxplots, there are too many outliers at different price points to be taken out. 
# Histograms of numeric variables shows that all are skewed. There is no right way to set limits
# Using logprice transformation however, removes skewness in price. Subsequent OLS shows no outliers upon regression
# Using price as the y variable, plot residuals/leverage and identify 2 outliers, 
# they were remove and OLS re-iterated, with more outliers. It will be too long a process. 

# ------------------------------------------------------------------------------------
# Price
# Using Lauren's ggplots:
ggplot(amsterdam, aes(price)) + 
  geom_freqpoly(stat='density') + xlim(0,1000)
ggplot(amsterdam, aes(logprice)) + 
  geom_freqpoly(stat='density') + xlim(0,10)
# log transformation of price result in a less skewed variable "price" without losing any data points

#boxplot of location vs price
ggplot(amsterdam, aes(x=location_3ways, y=realprice)) + 
  geom_boxplot(outlier.colour="blue", outlier.shape=16, outlier.size=2, notch=TRUE)

ggplot(amsterdam, aes(x=location_3ways, y=logprice)) + 
  geom_boxplot(outlier.colour="dark blue", outlier.shape=16,
               outlier.size=2, notch=TRUE)
# lesser outliers detected using boxplot of logprice / location

#boxplot of room_type vs price
ggplot(amsterdam, aes(x=room_type, y=realprice)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=TRUE)

ggplot(amsterdam, aes(x=room_type, y=logprice)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=TRUE)
# lesser outliers detected using boxplot of logprice / location

#boxplot of cancellation_policy vs price
ggplot(amsterdam, aes(x=cancellation_policy, y=realprice)) + 
  geom_boxplot(outlier.colour="green", outlier.shape=16, outlier.size=2, notch=TRUE)

ggplot(amsterdam, aes(x=cancellation_policy, y=logprice)) + 
  geom_boxplot(outlier.colour="green", outlier.shape=16, outlier.size=2, notch=TRUE)
# lesser outliers detected using boxplot of logprice / location

#histograms of numeric variables
par(mfrow=c(2,2))
hist(amsterdam$review_scores_rating)
hist(amsterdam$bathrooms)
hist(amsterdam$bedrooms)
hist(amsterdam$number_of_reviews)
# As expected, all are skewed. 

# OLS Regression
Price_ols <- lm(realprice ~ review_scores_rating + host_is_superhost +  
                  host_listings_count + host_identity_verified + 
                  room_type + bathrooms + bedrooms + 
                  minimum_nights + number_of_reviews + cancellation_policy + 
                  instant_bookable + host_since_duration + location_3ways + cleaning_fee, data=amsterdam)
summary(Price_ols)

#Using log price - interpretation of coefficients will be different 
logPrice_ols <- lm(logprice ~ review_scores_rating + host_is_superhost +  
                  host_listings_count + host_identity_verified + 
                  room_type + bathrooms + bedrooms + 
                  minimum_nights + number_of_reviews + cancellation_policy + 
                  instant_bookable + host_since_duration + location_3ways + cleaning_fee, data=amsterdam)
summary(logPrice_ols)

Lauren <- lm(realprice ~ near_centre + far_from_centre, data=amsterdam)
Michael <- lm(realprice ~ location_3ways, data=amsterdam)
summary(Lauren)
summary(Michael)

par(mfrow=c(2,2))
plot(Price_ols)
plot(logPrice_ols) # no outliers
y <- amsterdam[c(92,12797),]
# 2 listings total in (y) from plot(Price_ols)
# Listing @ Index:92 -> $8000/Moderate/1 bathrooms/1bedrooms/accomodates 4/review_score:72/ no.reviews:8
# Listing @ Index:12797 -> $90/far_from_centre/3.5 bathrooms/3bedrooms/accomodates 4/review_score:98/ no.reviews:10
# Qn - to take out??

amsterdam1 <- amsterdam[-c(92,12797),]
Price_ols_removeoutliers <- lm(realprice ~ review_scores_rating + host_is_superhost +  
                  host_listings_count + host_identity_verified + 
                  room_type + bathrooms + bedrooms + 
                  minimum_nights + number_of_reviews + cancellation_policy + 
                  instant_bookable + host_since_duration + location_3ways + cleaning_fee, data=amsterdam1)
summary(Price_ols_removeoutliers)
par(mfrow=c(2,2))
plot(Price_ols_removeoutliers)

#------------------------------------------------------
# Michael - end of Michael's codes
#------------------------------------------------------

# Descriptive statistics command
lapply(amsterdam, summary)

# DATA EXPLORATION

amsterdam %>% count(neighbourhood_cleansed) %>% arrange(desc(n)) %>% print(n=30)
amsterdam %>% count(room_type) %>% arrange(desc(n)) %>% print(n=30)
amsterdam %>% count(cancellation_policy) %>% arrange(desc(n)) %>% print(n=30)

# mean review rating is 95... extremely skewed and probably uninteresting density
ggplot(amsterdam, aes(review_scores_rating)) + 
  geom_freqpoly(stat='density') + xlim(0,100)

# price interesting
ggplot(amsterdam, aes(price)) + 
  geom_freqpoly(stat='density') + xlim(0,1000)

# interesting
ggplot(amsterdam, aes(host_since)) + 
  geom_freqpoly(stat='density') 

ggplot(amsterdam, aes(bathrooms)) + 
  geom_freqpoly(stat='density') 

ggplot(amsterdam, aes(bedrooms)) + 
  geom_freqpoly(stat='density') 

ggplot(amsterdam, aes(beds)) + 
  geom_freqpoly(stat='density') 

# skewed to the right
ggplot(amsterdam, aes(number_of_reviews)) + 
  geom_freqpoly(stat='density')  + xlim(0,50)

# skewed to the right
ggplot(amsterdam, aes(number_of_reviews_ltm)) + 
  geom_freqpoly(stat='density')  + xlim(0,50)

# Useless variables, will not add much
ggplot(amsterdam, aes(host_listings_count)) + 
  geom_freqpoly(stat='density') + xlim(0,20)

# Not sure if interesting
ggplot(amsterdam, aes(minimum_nights)) + 
  geom_freqpoly(stat='density') + xlim(0,20)

# make three/four neighbourhoods in terms of price?
amsterdam %>% group_by(neighbourhood_cleansed) %>% 
  summarise(price = mean(price), avgrating = mean(review_scores_rating), n = n()) %>% arrange(desc(price)) %>%  print(n=33)


# cancellation relevant interms of price
amsterdam %>% group_by(cancellation_policy) %>% 
  summarise(price = mean(price), avgrating = mean(review_scores_rating), n = n()) %>% arrange(desc(price))

# room_type significant differences (in terms of price)
amsterdam %>% group_by(room_type) %>% 
  summarise(price = mean(price), avgrating = mean(review_scores_rating), n = n()) %>% arrange(desc(price))

# instant_bookable provides no information in terms of price and rating
amsterdam %>% group_by(instant_bookable) %>% 
  summarise(price = mean(price), avgrating = mean(review_scores_rating), n = n()) %>% arrange(desc(price)) 


reg1 <- lm(data = amsterdam, number_of_reviews ~ price + review_scores_rating +
             host_since + host_is_superhost  + host_identity_verified +
             room_type + cancellation_policy + instant_bookable)
summary(reg1)

ggplot(amsterdam, aes(review_scores_rating,number_of_reviews)) + geom_point(alpha=0.2)
ggplot(amsterdam, aes(host_since,number_of_reviews)) + geom_point(alpha=0.2)




