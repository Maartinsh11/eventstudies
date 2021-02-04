#Bachelors thesis: DATASET MERGING & FORMATTING (2)
rm(list=ls())

library(tidyr)
library(dplyr)
library(plm)
library(zoo)
library(readxl)

setwd('C:/Users/marti/Desktop/BACHELORS')
gate <- read_excel("init.xlsx", col_types = c("text", 
                                              "text", "text", "text", "text", "numeric", 
                                              "text", "text", "date"))
gate <- gate %>% filter(gate$`CEO handle`!='NA')
gate <- gate %>% filter(gate$tweets==1)

df <- read_excel("stock_data.xlsx", 
                               col_types = c("date", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"))
df$date <- as.Date(df$date,format="%Y-%m-%d")
df <- gather(df, firm, price, ATVI:ZM, factor_key=TRUE)

twitter_datas <- read_excel("tweets.xlsx")
########################################################################
twitter_data <- twitter_datas[,c(3:5, 13:14, 91:93)]

twitter_data <- separate(data = twitter_data, col = created_at, into = c("date", "time"), sep = " ")
twitter_data$date <- as.Date(twitter_data$date , format = "%Y-%m-%d")
twitter_data <- twitter_data %>% group_by(screen_name) %>% arrange(desc(retweet_count)) %>% slice(1:100) #!!!!!!!!!!!!!!
twitter_data$firm=gate$`Security Symbol`[match(twitter_data$screen_name, gate$`CEO handle`)]

df <- full_join(df, twitter_data, by=c('date','firm'))
df <- df %>% filter(df$price!="NA")
#df <- pdata.frame(df,index=c('firm','date'))
df$text[is.na(df$text)] <- "0"
df$text<-replace(df$text, df$text!="0", "1")
df$day <- weekdays(as.Date(df$date))
df <- df %>%
  group_by(firm) %>%
  filter(any(Neu==1,Pos==1,Neg==1))
df$Pos[is.na(df$Pos)] <- "0"
df$Neu[is.na(df$Neu)] <- "0"
df$Neg[is.na(df$Neg)] <- "0"
df <- unique(df)

###

setwd('C:/Users/marti/Desktop/BACHELORS/news2')
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read_excel(file, col_names=FALSE, col_types = c("date", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text"))
    dataset <- tail(dataset,-4)
    names(dataset) <- dataset[1,]
    dataset <- dataset[-1,]
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read_excel(file, col_names=FALSE, col_types = c("date", 
                                                                   "text", "text", "text", "text", "text", 
                                                                   "text", "text", "text", "text", "text", 
                                                                   "text", "text", "text"))
    temp_dataset <- tail(temp_dataset,-4)
    names(temp_dataset) <- temp_dataset[1,]
    temp_dataset <- temp_dataset[-1,]
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

###
#library(openxlsx)
#openxlsx::write.xlsx(df, file = "final.xlsx", sheetName = "tweets", append = FALSE)
#openxlsx::write.xlsx(dataset, file = "events.xlsx", sheetName = "events", append = FALSE)
###
dataset$RIC <- gsub("\\..*","",dataset$RIC)



###
library(tidyverse)
dataset2 <- dataset %>% 
  unite(id, c("RIC", "NA"),remove=FALSE)

dataset1 <- df %>% 
  unite(id, c("firm", "date"),remove=FALSE)
dataset2 <- dataset2[,c('id','Event Type')]
df <- merge(x= dataset1, y= dataset2, by= 'id', all.x= T)
df$`Event Type`[is.na(df$`Event Type`)] <- "0"
df$`Event Type`<-replace(df$`Event Type`, df$`Event Type`!="0", "1")
###
df$price = as.numeric(df$price)
df <- df %>% group_by(firm) %>%  mutate(R=(price/dplyr::lag(price,order_by=date))-1) %>% ungroup()
df <- df %>% filter(df$R!="NA") #?

###
setwd('C:/Users/marti/Desktop/BACHELORS')
fama<-read.csv("F-F_Research_Data_Factors_daily.csv")
fama$date <- as.Date(fama$date, format="%d/%m/%Y")
df$date <- as.Date(df$date, format="%Y-%m-%d")
fama$Mkt.RF = as.numeric(fama$Mkt.RF)

df <- merge(x= df, y= fama, by= 'date', all.x= T)
df <- df %>% group_by(firm) %>%  mutate(ER=R-RF) %>% ungroup()
df <- df %>% filter(df$ER!="NA") #?

####
#Event studies
list <- df$firm
list <- unique(list)
total<-data.frame()
df$text = as.numeric(df$text)
df$Pos = as.numeric(df$Pos)
df <- unique(df)

df$`Event Type` = as.numeric(df$`Event Type`)
tweetstaken<-0
tweetspre<-0
for(ifirm in list){
  tempds <- df %>% filter(firm==ifirm) %>% filter(!is.na(ER))
  tempds[,"calc"] <- NA
  tempds[,"b0"] <- NA
  tempds[,"bmkt.rf"] <- NA
  tempds[,"bsmb"] <- NA
  tempds[,"bhml"] <- NA
  tempds[order(as.Date(tempds$date, format="%Y-%m-%d")),]
  for(irow in 1:nrow(tempds)) {
    row<-tempds[irow,]
      est <- irow-100
      est_end <- irow-10
      start <- irow+1
      apply <- irow-20
      apply_end <- irow+20
    if(row$text==1 & nrow(tempds)-irow>21 & est>0){
      tweetspre<-tweetspre+1
      regds <- tempds[est:est_end,]
      regds[order(as.Date(regds$date, format="%Y-%m-%d")),]
      #testds <- tempds[c(est:est_end,start:apply_end),]
      testds <- tempds[est:est_end,]
      if(sum(testds[,"text"])==0 & sum(testds[,"Event Type"])==0){
        tweetstaken<-tweetstaken+1
      reg <- regds %>% lm(data=., formula=ER~Mkt.RF+SMB+HML)
      tempds$b0[apply:apply_end]<-as.numeric(summary(reg)$coefficients[[1]])
      tempds$bmkt.rf[apply:apply_end]<-as.numeric(summary(reg)$coefficients[[2]])
      tempds$bsmb[apply:apply_end]<-as.numeric(summary(reg)$coefficients[[3]])
      tempds$bhml[apply:apply_end]<-as.numeric(summary(reg)$coefficients[[4]])
      ##reg <- regds %>% lm(data=., formula=R~(Mkt.RF+RF))
      #prediction <- predict(reg,newdata=tempds[apply:apply_end,c("Mkt.RF","SMB","HML")])
      #tempds$calc[apply:apply_end] <- prediction
      tempds <- tempds %>%  mutate(calc=b0+bmkt.rf*Mkt.RF+bsmb*SMB+bhml*HML)
      print(c(irow, ifirm, sum(tempds[est:est_end, c("text")])))
      }
    }
  }
  total <- rbind(tempds, total) 
}

#PREDICTION()??? ROWS? - ORDER BY DATE???? -  sum(testds[,"text"])==0 & sum(testds[,"Event Type"])==0

total <- total %>% group_by(firm) %>%  mutate(AR=R-calc) %>% ungroup()
total <- total %>% filter(AR!=0)
list <- total$firm
list <- unique(list) 

####DEBUG####################
for(ifirm in list){
  tempds <- total %>% filter(firm==ifirm)
  for(irow in 1:nrow(tempds)) {
    row<-tempds[irow,]
    est <- irow-20
    est_end <- irow-1
    apply <- irow-10
    apply_end <- irow+10
    if(row$text==1 & est>0){
      if(sum(tempds[est:est_end, c("text")])==0){
      print(c(irow, sum(tempds[est:est_end, c("text")])))}
    }
  }
}
####DEBUG############################

AR <- data.frame()
#AR[1,] <- as.numeric(0)
days <- c(-20:20)
AR[1:41,'numb'] <-  days
for(ifirm in list){
  AR[1:41,ifirm] <- as.numeric(0)
  tempds <- total %>% filter(firm==ifirm)
  twcount <- 0
  for(irow in 1:nrow(tempds)) {
    row<-tempds[irow,]
    if(row$text==1){
      for(i in days){
      AR[i+21,ifirm] <-  AR[i+21,ifirm]+abs(tempds[irow+i,"AR"])
      }
      twcount <- twcount+1
    }
  }
  for(i in days){
    AR[i+21,ifirm] <-  AR[i+21,ifirm]/twcount
  }
}
c <- length(list)

AR[is.na(AR)] <- 0
AR[2:(c+1)] <- cumsum(AR[1:41, c(2:(c+1))])
for(i in days){
  AR[i+21,'sum'] <-  sum(AR[i+21,2:(c+1)])
}
AR$sum <-AR$sum/c
AR1 <- gather(AR, firm, AR, 2:(c+2), factor_key=FALSE)
AR <- AR  %>% select(c(numb, sum))
