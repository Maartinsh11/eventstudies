#Thesis: Vesilind and Cerlenoks
#
rm(list=ls())

library(tidyr)
library(dplyr)
library(plm)
library(zoo)
library(readxl)
library(ggplot2)
library(tidyverse)

setwd('C:/Users/marti/Desktop/BACHELORS')
gate <- read_excel("init.xlsx", col_types = c("text", 
                                              "text", "text", "text", "text", "numeric", 
                                              "text", "text", "date", "text", "text"))
gate <- gate %>% filter(gate$`CEO handle`!='NA')

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
twitter_data <- read_excel("tweets.xlsx")
twitter_data <- twitter_data %>% filter(is_retweet==FALSE)
########################################################################
twitter_data <- twitter_data[,c(3:5, 13:14, 92:94)]
twitter_data$created_at <- format(twitter_data$created_at, tz="EST")
twitter_data <- separate(data = twitter_data, col = created_at, into = c("date", "time"), sep = " ")
twitter_data$day <- lubridate::wday(twitter_data$date, week_start = 1)
#twitter_data <- twitter_data %>% group_by(screen_name) %>% arrange(desc(retweet_count)) %>% slice(1:30) #%>% filter(retweet_count>10000)
twitter_data$firm=gate$`Security Symbol`[match(twitter_data$screen_name, gate$`CEO handle`)]
twitter_data <- mutate(twitter_data, after_hours=if_else(twitter_data$time>16, 1, 0))
twitter_data$day <- as.numeric(twitter_data$day)
twitter_data$after_hours <- as.numeric(twitter_data$after_hours)
twitter_data$date <- as.Date(twitter_data$date, format="%Y-%m-%d")

twitter_data <- twitter_data %>% mutate(date = if_else(day==7, date + 1, if_else(day==6, date + 2, if_else(day==5&after_hours==1, date+3, if_else(after_hours==1 & (day==1|day==2|day==3|day==4), date+1, date)))))
                                                              
#twitter_data <-twitter_data %>%  filter(firm=="TSLA")
twitter_data$date <- as.Date(twitter_data$date,format="%Y-%m-%d")
df <- full_join(df, twitter_data, by=c('date','firm'))
tweets <- gate[,c("Security Symbol", "tweets", "popular")]
names(tweets)[1]<-paste("firm")
df <- merge(x= df, y= tweets, by= 'firm', all.x= T)


df <- df %>% filter(tweets==1) #%>% filter(popular==1)


df$text[is.na(df$text)] <- "0"
df$text<-replace(df$text, df$text!="0", "1")
library(lubridate)



#####################NEWS MODULE###################################

setwd('C:/Users/marti/Desktop/BACHELORS/NEWS')
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read_excel(file, col_names=FALSE  , col_types = c("date", 
                                                                 "text", "text", "text", "text", "text", 
                                                                 "text", "text", "text", "text", "text", 
                                                                 "text", "text", "text"))
    dataset <- tail(dataset,-4)
    names(dataset) <- dataset[1,]
    dataset <- dataset[-1,]
  }

  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read_excel(file, col_names=FALSE  , col_types = c("date", 
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
setwd('C:/Users/marti/Desktop/BACHELORS/')
News_used <- read_excel("News_used.xlsx", 
                        col_names = FALSE, col_types = c("text"))
names(News_used)[1]<-paste("news")
news <- News_used$news
dataset <- dataset %>% filter(`Event Type` %in% news)

########################################################
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
df = df[order(df[,'id'],-df[,'retweet_count']),]
df = df[!duplicated(df$id),]
#####
df <- df %>% filter(df$price!="NA")
df <- df %>% group_by(firm) %>%  mutate(R=(price/dplyr::lag(price,order_by=date))-1) %>% ungroup()

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
total <- data.frame()
df$text = as.numeric(df$text)
#df <- df %>%  filter(firm== "TSLA")
RETS <- data.frame()

df$`Event Type` = as.numeric(df$`Event Type`)
tweetsphase1<-0
tweetsphase2<-0
tweetsphase3<-0
RETSA<-data.frame()
firmas<-0
for(ifirm in list){
  RETSA[1:11, ifirm] <- as.numeric(0)
  tweetsfirm <- 0
  tempds <- df %>% filter(firm==ifirm) %>% filter(!is.na(ER))
  tempds[,"calc"] <- NA
  tempds[,"b0"] <- NA
  tempds[,"bmkt.rf"] <- NA
  tempds[,"bsmb"] <- NA
  tempds[,"bhml"] <- NA
  tempds[order(as.Date(tempds$date, format="%Y-%m-%d")),]
  ceostartdate <- gate %>% filter(`Security Symbol`==ifirm) %>% select(9)
  ceostartdate <- as.Date(ceostartdate[[1,1]], format="%Y-%m-%d")
  
  for(irow in 1:nrow(tempds)) {
    row<-tempds[irow,]
      est <- irow-125 #120
      est_end <- irow-6
      start <- irow+1
      apply <- irow-5
      apply_end <- irow+5
    if(row$text==1 & nrow(tempds)-irow>10 & est>0 & apply>0 & apply_end>0 & row$date>ceostartdate){
      tweetsphase1<-tweetsphase1+1
      regds <- tempds[est:est_end,]
      regds[order(as.Date(regds$date, format="%Y-%m-%d")),]
      testds <- tempds[irow,] #gives nas
      if(sum(testds[,"Event Type"])==0){
      tweetsphase2<-tweetsphase2+1
      reg <- regds %>% lm(data=., formula=ER~Mkt.RF+SMB+HML)
      coe1<-as.numeric(summary(reg)$coefficients[[1]])
      coe2<-as.numeric(summary(reg)$coefficients[[2]])
      coe3<-as.numeric(summary(reg)$coefficients[[3]])
      coe4<-as.numeric(summary(reg)$coefficients[[4]])
      nos<-paste(ifirm, irow, sep=" ")
      abnret <- tempds[irow,"R"]-(coe1+coe2*tempds[irow,"Mkt.RF"]+coe3*tempds[irow,"SMB"]+coe4*tempds[irow,"HML"])
      summa<- sum(tempds[apply:apply_end,"text"])
      if(row$Neu==1){
      RETS[1:11, nos] <- as.numeric(0)
      tweetsphase3 <- tweetsphase3+1
      RETS[1:11,nos] <- (tempds[apply:apply_end,"R"]-(coe1+coe2*tempds[apply:apply_end,"Mkt.RF"]+coe3*tempds[apply:apply_end,"SMB"]+coe4*tempds[apply:apply_end,"HML"]))
      tweetsfirm <- tweetsfirm+1
      RETSA[1:11,ifirm] <- abs(RETSA[1:11,ifirm]+RETS[1:11,nos])
      }
      print(c(irow, ifirm, sum(tempds[est:est_end, c("text")])))
      }
    }
  }
  total <- rbind(tempds, total)
  RETSA[1:11,ifirm] <- RETSA[1:11,ifirm]/tweetsfirm
  firmas <- firmas+1
}
#RETSA[1:41,] <- RETSA[1:41,][which(RETSA[21,] > 0)]
list <- total$firm
list <- unique(list) 
##############################################################
days <- c(-5:5)
d<-(abs(days[1])+1)
z<-((abs(days[1])*2)+1)
y<-((abs(days[1])/2)+1)
ntweets<-as.numeric(ncol(RETS))
library(DescTools)
RETS[1:z,'numb'] <- days
RETSG <- gather(RETS, firm, AR, 1:ntweets, factor_key=FALSE)
#RETSG[,3]<-Winsorize(RETSG[,3], minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
#                     na.rm = FALSE, type = 7)
min(RETS[,3])
RETS<-data.frame(t(spread(RETSG, numb, AR)))
colnames(RETS) <- RETS[1,]
RETS <- RETS[-1, ]
RETS[] <- as.numeric(unlist(RETS[]))
RETSA[] <- as.numeric(unlist(RETSA[]))
RETSA <- RETSA[, colSums(is.na(RETSA)) != nrow(RETSA)]
RETSA[1:z,'sum'] <-  rowSums(RETSA[1:z,1:ncol(RETSA)])/ncol(RETSA)
RETS[1:z,'sum'] <-  rowSums(RETS[1:z,1:ncol(RETS)])/ncol(RETS)
#RETSA[1:z,'sum'] <-  rowSums(RETSA[1:z,1:firmas])/firmas
RETSA[1:z,'numb'] <- days

#RETS[1:z,] <- cumsum(RETS[1:z, ])

RETS[1:z,'numb'] <- days
RETS2<- RETS[,c("numb", "sum")]
RETS3<- RETSA[,c("numb", "sum")]
RETSSAVE<-RETS2
RETSSAVE$ABS<-RETS3$sum
###
library(openxlsx)
library(DescTools)
openxlsx::write.xlsx(RETSSAVE, file = "res.xlsx", sheetName = "AR", append = FALSE)
###


t.test(three[1,1:ntweets], mu=0)
ggplot(RETS) +
  aes(x = numb, y = sum) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()


three <- data.frame()
three[1,1:ntweets] <- colSums(RETS[1:20,1:ntweets])/20
three[2,1:ntweets] <- RETS[21,1:ntweets]
three[3,1:ntweets] <- colSums(RETS[22:41,1:ntweets])/20

wilcox.test(as.numeric(RETS[10,1:ntweets]), y = NULL,
            alternative = "two.sided",
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.90,
            tol.root = 1e-4, digits.rank = Inf)


RETSA[1:41,'sum'] <-  rowSums(RETSA[1:41,1:ncol(RETSA)])/ncol(RETSA)
RETSA[1:z,'numb'] <- days

ggplot(RETSA) +
  aes(x = numb, y = sum) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

wilcox.test(as.numeric(RETSA[37,1:(ncol(RETSA)-2)]), y = NULL,
            alternative = "two.sided",
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.90,
            tol.root = 1e-4, digits.rank = Inf)
