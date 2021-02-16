#Thesis: Vesilind and Cerlenoks
#
rm(list=ls())

library(tidyr)
library(dplyr)
library(plm)
library(zoo)
library(readxl)
library(ggplot2)

setwd('C:/Users/marti/Desktop/BACHELORS')
gate <- read_excel("init.xlsx", col_types = c("text", 
                                              "text", "text", "text", "text", "numeric", 
                                              "text", "text", "date", "text"))
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
df <- df %>% filter(!is.na(price))
twitter_data <- read_excel("tweets.xlsx")
########################################################################
twitter_data <- twitter_data[,c(3:5, 13:14)]

twitter_data <- separate(data = twitter_data, col = created_at, into = c("date", "time"), sep = " ")
twitter_data$date <- as.Date(twitter_data$date , format = "%Y-%m-%d")
twitter_data <- twitter_data %>% group_by(screen_name) %>% arrange(desc(retweet_count)) %>% slice(1:30) #%>% filter(retweet_count>10000)
twitter_data$firm=gate$`Security Symbol`[match(twitter_data$screen_name, gate$`CEO handle`)]

df <- full_join(df, twitter_data, by=c('date','firm'))
tweets <- gate[,c("Security Symbol", "tweets")]
names(tweets)[1]<-paste("firm")
df <- merge(x= df, y= tweets, by= 'firm', all.x= T)
df <- df %>% filter(tweets==1)


df$text[is.na(df$text)] <- "0"
df$text<-replace(df$text, df$text!="0", "1")
df$day <- weekdays(as.Date(df$date))

###

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

###
dataset <- dataset %>% filter(`Event Type`=="Earnings Conference Call"|
                              `Event Type`=="Earnings Release"|
                              `Event Type`=="CourtWire"|
                              `Event Type`=="Corporate Conference Presentation"|
                              `Event Type`=="Corporate Call"|
                              `Event Type`=="Annual Shareholders Meeting"|
                              `Event Type`=="Company Financials Distress"|
                              `Event Type`=="Significant M&A - Completed"|
                              `Event Type`=="Earnings Release"|
                              `Event Type`=="Significant M&A - Announced"|
                              `Event Type`=="Significant M&A - Upcoming")


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
total <- data.frame()
df$text = as.numeric(df$text)

df <- unique(df)
df = df[!duplicated(df$id),]
RETS <- data.frame()

df$`Event Type` = as.numeric(df$`Event Type`)
tweetsphase1<-0
tweetsphase2<-0
tweetsphase3<-0
for(ifirm in list){
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
      est <- irow-140 #140
      est_end <- irow-21
      start <- irow+1
      apply <- irow-20
      apply_end <- irow+20
    if(row$text==1 & nrow(tempds)-irow>20 & est>0 & row$date>ceostartdate){
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
      nos<-paste(tweetsphase3, ifirm, row$date, sep=" ")
      abnret <- tempds[irow,"R"]-(coe1+coe2*tempds[irow,"Mkt.RF"]+coe3*tempds[irow,"SMB"]+coe4*tempds[irow,"HML"])
      if(abnret>(-0.015) & abnret<(0.015)){
      RETS[1:41, nos] <- as.numeric(0)
      tweetsphase3 <- tweetsphase3+1
      RETS[1:41,nos] <- tempds[apply:apply_end,"R"]-(coe1+coe2*tempds[apply:apply_end,"Mkt.RF"]+coe3*tempds[apply:apply_end,"SMB"]+coe4*tempds[apply:apply_end,"HML"])
      }
      print(c(irow, ifirm, sum(tempds[est:est_end, c("text")])))
      }
    }
  }
  total <- rbind(tempds, total) 
}

list <- total$firm
list <- unique(list) 
##############################################################
days <- c(-20:20)
d<-(abs(days[1])+1)
z<-((abs(days[1])*2)+1)
y<-((abs(days[1])/2)+1)
ntweets<-as.numeric(ncol(RETS))
library(DescTools)
RETS[1:z,'numb'] <- days
RETSG <- gather(RETS, firm, AR, 1:ntweets, factor_key=FALSE)
RETSG[,3]<-Winsorize(RETSG[,3], minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                     na.rm = FALSE, type = 7)
min(RETS[,3])
RETS<-data.frame(t(spread(RETSG, numb, AR)))
colnames(RETS) <- RETS[1,]
RETS <- RETS[-1, ]
RETS[] <- as.numeric(unlist(RETS[]))
RETS[1:z,'sum'] <-  rowSums(RETS[1:z,1:ntweets])/ntweets

RETS[1:z,] <- cumsum(RETS[1:z, ])
RETS[1:z,'numb'] <- days

RETS2<- RETS[,c("numb", "sum")]

###
library(openxlsx)
library(DescTools)
openxlsx::write.xlsx(RETS2, file = "res.xlsx", sheetName = "AR", append = FALSE)
###


t.test(three[1,1:ntweets], mu=0)
ggplot(RETS2) +
  aes(x = numb, y = sum) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()


three <- data.frame()
three[1,1:ntweets] <- colSums(RETS[1:20,1:ntweets])/20
three[2,1:ntweets] <- RETS[21,1:ntweets]
three[3,1:ntweets] <- colSums(RETS[22:41,1:ntweets])/20
wilcox.test(as.numeric(three[3,1:ntweets]), y = NULL,
            alternative = "two.sided",
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.90,
            tol.root = 1e-4, digits.rank = Inf)
