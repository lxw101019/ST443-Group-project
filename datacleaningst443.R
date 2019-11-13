rm(list = ls())

# SET YOUR WORKING DIRECTORY TOWARDS THE ONE IN WHICH THIS SCRIPT AND THE CSV FILE ARE
setwd("~/Documents/Rworkspace/ST443")

# IF NOT INSTALLED, FIRST:
# install.packages('tidyverse')
library(tidyverse)

amsterdam <- read_csv('listingsamsterdam.csv')


amsterdam <- select(amsterdam, price, review_scores_rating, host_since, 
         host_is_superhost, neighbourhood_cleansed, host_listings_count,
         host_identity_verified, room_type,
         bathrooms, bedrooms, minimum_nights,
         number_of_reviews, cancellation_policy, instant_bookable, accommodates,
         weekly_price, monthly_price)

# Data transformation
amsterdam$price <- as.numeric(gsub('\\$|,', '', amsterdam$price))
amsterdam$weekly_price <- as.numeric(gsub('\\$|,', '', amsterdam$weekly_price))
amsterdam$monthly_price <- as.numeric(gsub('\\$|,', '', amsterdam$monthly_price))
#amsterdam$cleaning_fee <- as.numeric(gsub('\\$|,', '', amsterdam$cleaning_fee))

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
amsterdam <- mutate(amsterdam, bathrooms = bathrooms / accommodates)
amsterdam <- mutate(amsterdam, bedrooms = bedrooms / accommodates)
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

drops <- c("weekly_price","monthly_price","neighbourhood_cleansed")
amsterdam <- amsterdam[ , !(names(amsterdam) %in% drops)]

amsterdam <- drop_na(amsterdam)
write.csv(amsterdam,'st443amsterdamdata')
amsterdam <- read_csv('st443amsterdamdata')
amsterdam <- amsterdam[,-1]

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




