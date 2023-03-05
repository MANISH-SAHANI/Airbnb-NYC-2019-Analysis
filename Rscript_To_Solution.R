library(tidyverse)
library(glue)
library(dplyr)
library(esquisse)
library(ggthemes)
library(lubridate)
library(tinytex)
library(shiny)
library(knitr)

#Importing Data
airbnb <- read.csv("C:/Desktop/Industry_Assignment_2/Assignment2_Solution/Airbnb.csv",header = TRUE)

#Viewing the data set
head(airbnb)
View(airbnb)
summary(airbnb)
glimpse(airbnb)


### 1) PRE PROCESSING 

# 1) a.Checking For Missing Values
NAValues <- airbnb %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
NAValues

#2) b.Checking For Duplicated values
sum(duplicated(airbnb))
#There are No Duplicated Values in the data set

summary(airbnb)

# 1) c.Identify and Treat Outliers if any
# We take the quantile 1 and 3 as references
qtl1 = quantile(airbnb$price, 0.25)
qtl3 = quantile(airbnb$price, 0.75)
iqr = qtl3 - qtl1
lower = qtl1 - iqr * 1.5
upper = qtl3 + iqr * 1.5
lower
upper

airbnb %>%
  filter(price < lower | price > upper) %>%
  top_n(10, price) %>%
  select(neighbourhood_group, neighbourhood, price) %>%
  arrange(desc(price)) %>%
  kable(col.names = c("Neighbourhood Group", "Neighbourhood", "Price"))


# 1) d.Transform Data based on judgement and explain the methodology used

#Checking Up the Unique Values in neighborhood_group, room_type column(nominal qualitative)
#For extracting unique values
c(unique(airbnb["neighbourhood_group"]))
c(unique(airbnb["room_type"]))
c(unique(airbnb["neighbourhood"]))
c(unique(airbnb["host_name"]))
c(unique(airbnb["minimum_nights"]))


#Converting neighbourhood_group,neighbourhood and room_type into factors
airbnb$neighbourhood_group <- as.factor(airbnb$neighbourhood_group)
levels(airbnb$neighbourhood_group)

airbnb$room_type <-  as.factor(airbnb$room_type)
levels(airbnb$room_type)

airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
levels(airbnb$neighbourhood)


#Separating Dates
airbnb <- tidyr::separate(airbnb,last_review,c("Month","Day","Year"),sep = "/")

#Replacing NA with 0
airbnb$Year[is.na(airbnb$Year) == TRUE]=0
airbnb$Month[is.na(airbnb$Month) == TRUE]=0
airbnb$Day[is.na(airbnb$Day) == TRUE]=0


#Datatype Conversion
airbnb$Month<- as.integer(airbnb$Month)
airbnb$Year<- as.integer(airbnb$Year)
airbnb$Day<- as.integer(airbnb$Day)
View(airbnb)


#Replacing NA in review_per_month with 0
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month) == TRUE] = 0

#Checking for any NA value remaining
sapply(airbnb, function(x) sum(is.na(x)))
glimpse(airbnb)

summary(airbnb)


### 2) EXPLORATORY DATA ANALYSIS 

#2 a)What can we learn about different hosts and areas?
airbnb %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarise(mean_review = host_name) %>%
  arrange(desc(mean_review)) %>%
  head(30) %>%
  ggplot(aes(x=mean_review, y = reorder(neighbourhood,mean_review), fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Top 30 Host Prefered in Different Areas",
       x = "Host",
       y = "Neighborhood",
       fill = "Neighbourhood Group")

#The graph represents the different areas under different hosts. 
#Hosts are stated on the x axis and different areas are shown in the y axis.
#Different colors in the graph represents the neighborhood groups.
#As per the graph we could say that zsofia host has the more number of area which are queens and manhattan neighborhood groups.
#the least number of area is under zsuzsanna has the least number of area which is of queens neighborhood groups.


#2 b) Which hosts are the busiest and why?
#We can check the host is busiest ,according to number of reviews
airbnb |> group_by(host_name,neighbourhood,neighbourhood_group) |> 
  tally(number_of_reviews) |> 
  arrange(desc(n)) |> 
  head(10)
#As we can see from above the most popular host is maya 


# 2 c)Is there any noticeable difference in traffic among different areas and what could be the reason for it?
airbnb %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarise(review = number_of_reviews) %>%
  arrange(desc(review)) %>%
  head(50) %>%
  ggplot(aes(x=review, y = reorder(neighbourhood,review), fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Top 50 Neighborhood According to The Number of Reviews",
       x = "Number of Reviews",
       y = "Neighbourhood",
       fill = "Neighbourhood Group")

#The graph represents the Top 50 Neighbourhood according to the Number of reviews 
#Different colors in the graph represents the different neighborhood groups.
#Yes! There is a lot of difference in traffic among different areas as we can see in the 
#bar plot there is huge traffic in QUEENS EAST ELMHURST as compared to others neighbourhood group.
#From bar plot we can clearly see that the number of reviews of QUEEN EAST ELMHURST is highest
#,so this means the number of visits are more in QUEENS EAST ELMHURST so we could conclude that there is more traffic in QUEENS EAST ELMHURST neighbourhood group.
#So, according to number of reviews we can analyse traffic among different areas 



### 3) PLOTTING GRAPHS

#3 a) Cluster of hosts according to neighborhood group
#we have used latitude and longitude of hosts
ggplot(airbnb, aes(latitude, longitude, color = neighbourhood_group)) + 
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Cluster of Host According to the Neighbourhood Group",
       x = "Latitude",
       y = "Longitude",
       color = "Neighbourhood Group")


#3 b)Plot a graph showing room_type chosen commonly with neighborhood group.
ggplot(airbnb, aes(x = room_type, y = price, fill = room_type)) + scale_y_log10() +
  geom_boxplot() +
  theme_minimal() +
  labs (x="", y= "Price") +
  facet_wrap(~neighbourhood_group) +
  facet_grid(.~ neighbourhood_group) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "right") +
  labs(title = "Room Type Commonly Chossen With Neighborhood Groups",
       fill = "Room Type")


#3 c)Plot average price with neighborhood.
airbnb %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(desc(mean_price)) %>%
  head(50) %>%
  ggplot(aes(x=mean_price, y = reorder(neighbourhood,mean_price), fill = neighbourhood_group)) +
    geom_col() +
  theme_minimal() +
  labs(title = "Top 50 Neighborhood According to The Average Price",
       x = "Average Price",
       y = "Neighborhood",
       fill = "Neighbourhood Group")


#3 d)Plot highest, lowest, and median price of stay with neighborhood
#highest
airbnb %>%
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(mean_price = mean(price))%>%
  arrange(desc(mean_price))%>% 
  head(30)%>%
  ggplot(., aes(x = reorder(neighbourhood, -mean_price) , y = mean_price, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_price,digits=3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 30 Hightest Price of Stay With Neighbourhoods",
       x = "Neighbourhood",
       y = "Price",
       fill = "Neighbourhood Group")

#lowest
airbnb %>%
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(mean_price = mean(price))%>%
  arrange(mean_price) %>% 
  head(30)%>%
  ggplot(., aes(x = reorder(neighbourhood, mean_price) , y = mean_price, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_price,digits=3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 30 Lowest Price of Stay With Neighbourhoods",
       x = "Neighbourhood",
       y = "Price",
       fill = "Neighbourhood Group")

#median
airbnb %>%
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(min_price = median(price))%>%
  arrange(min_price) %>% 
  head(30)%>%
  ggplot(., aes(x = reorder(neighbourhood, min_price) , y = min_price, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(min_price,digits=3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 30 Median Price of Stay With Neighbourhoods",
       x = "Neighbourhood",
       y = "Price",
       fill = "Neighbourhood Group")
