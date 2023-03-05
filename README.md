<h1 align="center"> 📈Airbnb NYC 2019</h1>
 
## Importing Some Required Library and the Data Set :
<p>
 
library(tidyverse)

library(glue)

library(dplyr)

library(esquisse)

library(ggthemes)

library(lubridate)

library(tinytex)

library(shiny)

library(knitr)
 
</p>

## Importing DataSet
airbnb <- read.csv("C:/Desktop/Industry_Assignment_2/Assignment2_Solution/Airbnb.csv",header = TRUE)

## Viewing the data set
head(airbnb)

<p align="left">
  <img width="700" src="https://user-images.githubusercontent.com/91081774/222950681-51d779e7-0042-4a6f-868d-2a9138b9a5a8.png"/>
</p>


### 1) PRE PROCESSING 

#### a.Checking For Missing Values
<p>
  NAValues <- airbnb %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
  
  NAValues
</p>

#### b.Checking For Duplicated values
sum(duplicated(airbnb))
 #There are No Duplicated Values in the data set

summary(airbnb)
