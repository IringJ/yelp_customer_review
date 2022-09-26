##### APAN 5205 #####
####### Group 10 #####
##### Group project: Yelp #####
##### Section 1. raw data preparation #####

setwd("~/Desktop/5205_cleaned_dataset")

#### Part 1: Preparation
library(rjson)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cluster)


## 1. business dataset 
## Load the package required to read JSON files.
business = readLines('yelp_academic_dataset_business.json')
business = jsonlite::stream_in(textConnection(business))
business <- business %>%
  filter(state == 'FL')


head(business)
names(business)

## 2. review dataset 
## Load the package required to read JSON files.

review = readLines('yelp_academic_dataset_review.json',n=1000000)
review = jsonlite::stream_in(textConnection(review))

head(review)


# select reviews from 2019-2022
class(review$date)
review$date = as.Date(review$date)
head(review$date)


# filter out the review since 2019
review <- review %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2022-03-01"))

head(review$date)
busi_revi = merge(business, review, by = 'business_id')

write.csv(busi_revi,"busi_revi.csv",row.names = FALSE)




## 3. user dataset 
## Load the package required to read JSON files.

user = readLines('yelp_academic_dataset_user.json',n = 1000000)
user = jsonlite::stream_in(textConnection(user))
write.csv(user,"user.csv",row.names = FALSE)


## 4. checkin dataset 
## Load the package required to read JSON files.

checkin = readLines('yelp_academic_dataset_checkin.json',n = 100)
checkin = jsonlite::stream_in(textConnection(checkin))


## 5. tip dataset 
## Load the package required to read JSON files.

tip = readLines('yelp_academic_dataset_tip.json',n = 1000000)
tip = jsonlite::stream_in(textConnection(tip))

# Overview of dataset 
names(business)

names(review)

names(user)

################################################################################
#### Part 2. merge data frames and write csv
# Merge1: business & review data frame 
busi_revi = merge(business, review, by = 'business_id')



#too much NA values and irregular column of hours and attributes, drop directly 
head(busi_revi$hours)
head(busi_revi$attributes)
busi_revi=subset(busi_revi,select=-c(hours,attributes))


# Merge2: busi_revi & user date frame 
busi_revi_user = merge(busi_revi, user, by = 'user_id')


#write to csv 
#write.csv(busi_revi,"busi_revi_raw.csv",row.names = FALSE)
#write.csv(busi_revi_user,"busi_revi_user_raw.csv",row.names = FALSE)

################################################################################
####Part3 data clean and exploratory descriptive analysis, busi_revi

######read data
business=read.csv('business.csv')
busi_revi <- read.csv('busi_revi.csv')
busi_revi_user <- read.csv('busi_revi_user.csv')
head(busi_revi)
head(busi_revi_user)
str(busi_revi_user)

nrow(busi_revi)
nrow(busi_revi_user)

#3.1 check joined table column
ls(busi_revi)
str(busi_revi)

#3.2 drop city with less than 20 restaurants

busi_revi<-busi_revi %>%
  mutate(
    city = ifelse(
      city %in%  names(which(table(city) <= 20)),
      'too less',
      city
    ))

busi_revi=busi_revi[busi_revi$city!='too less',]
table(busi_revi$city)



#3.3 check star distribution
table(busi_revi$stars.x)
table(busi_revi$star.y)

ggplot(busi_revi,aes(x=stars.x))+
  geom_histogram()
ggplot(busi_revi,aes(x=stars.y))+
  geom_histogram()

#3.4 check city distribution 
ggplot(busi_revi,aes(x=city))+
  geom_bar()+
  coord_flip()
#3.5 get city with more than 5000 restaurants
names(which(table(busi_revi$city) > 5000))


#3.6 correlation between review_counts and stars
cor(busi_revi$stars.x,busi_revi$review_count)


ggplot(data=busi_revi,aes(x=stars.x,y=review_count)+
         geom_point()+
         geom_smooth(method='lm',size=1.3,color='steelblue3')+
         coord_cartesian()
       

#3.7check open status
table(busi_revi$is_open)
#3.8 leave for anomaly detection analysis
close=busi_revi[busi_revi$is_open==0,]
open=busi_revi[busi_revi$is_open==1,]


################################################################################
####Part4 data clean and exploratory descriptive analysis, busi_revi_user
#4.1 languages of text  
library(textcat)
busi_revi_user <- busi_revi_user %>%
 mutate(language = textcat(busi_revi_user$text))

table(busi_revi_user$language)

ggplot(busi_revi_user, aes(x=language)) +
 geom_bar()

#4.2 users
glimpse(busi_revi_user)

ggplot(busi_revi_user, aes(x=average_stars)) +
 geom_bar()


# #4.3 fans
# table(busi_revi_user$fans)
# 
# ggplot(busi_revi_user, aes(x=fans)) +
#   geom_bar()
# 
# #4.4 l_fans 
# l_fans <- busi_revi_user %>%
#   mutate(fans = case_when(fans == 0 ~ '= 0',
#                           1 <= fans & fans <= 50 ~ '1-50',
#                           50 < fans & fans <= 100 ~ '50-100',
#                           100 < fans & fans <= 200 ~ '100-200',
#                           200 < fans & fans <= 500 ~ '200-500',
#                           500 < fans & fans <= 1000 ~ '500-1000',
#                           1000 < fans ~ '> 1000'))
# 
# ggplot(l_fans, aes(x=fans)) +
#   geom_bar()
# 
# 
# ggplot(l_fans,aes(fans,average_stars)) +
#   geom_point() +
#   geom_smooth(method='lm')


# 4.5 friends
len_freinds <- busi_revi_user %>%
 mutate(friends0 = case_when(friends == 'None' ~ 0))

len_freinds <- len_freinds %>%
 mutate(friends1 = case_when(TRUE ~ lengths(strsplit(friends, ","))))

# combine the two columns together 
len_freinds <- len_freinds %>%
 mutate(friends = coalesce(friends0, friends1))

# delete the friends0 and friends1
busi_revi_user = subset(len_freinds,select=-c(friends0,friends1))


# 4.6 elite
len_elite <- busi_revi_user %>%
 mutate(elite1 = gsub('20,20','2020',elite))

len_elite <- len_elite %>%
 mutate(elite = case_when(TRUE ~ lengths(strsplit(elite1, ","))))

# delete the friends0 and friends1
busi_revi_user = subset(len_elite,select=-c(elite1))

###############################################################################
####Part5 Write Cleaned Data
write.csv(busi_revi,"busi_revi.csv",row.names = FALSE)
write.csv(busi_revi_user,"busi_revi_user.csv",row.names = FALSE)
