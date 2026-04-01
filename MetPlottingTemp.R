### Script to plot up Met data, find average temp and precip 

library(ggplot2)
library(nlme)
library(dplyr)
library(knitr)
library(lubridate)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### Temperature data downloaded directly from MCM website and saved locally
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/'

Temp1<-read.csv(paste0(drive, '/mcmlter-clim-frlm_airt-daily-20230605.csv'), stringsAsFactors=FALSE)
Temp1$date<-lubridate::mdy(Temp1$date_time)
Temp1$Temp_avg<-as.numeric(Temp1$avg_airt3m)

Temp<-Temp1%>%
  filter(., date>'2011-12-01'&date<'2022-03-01')

Temp$month<-lubridate::month(Temp$date)
Temp<-Temp%>%filter(., month==11|month==12|month==1|month==2)

Temp_avg<-Temp%>%
  summarize(., Temp_mean=mean(avg_airt3m))
  
