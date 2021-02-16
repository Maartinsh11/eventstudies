#Thesis: Vesilind and Cerlenoks

rm(list=ls())

library(dplyr)

#Get tweets
library(readxl)
library(rtweet)
init <- read_excel("init.xlsx")
users <- init %>% filter(tweets==1)
users <- users$`CEO handle`
tmls <- get_timeline(users, n = 3200, retryonratelimit=TRUE)

#Save to an excel file
library(openxlsx)
openxlsx::write.xlsx(tmls, file = "tweet_data.xlsx", sheetName = "tweets", append = FALSE)
