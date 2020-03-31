setwd("/Users/gunnvantsaini/Documents/Work/Jigsaw Academy/Data Scientist Course/Data Science Redo/Telecom Case study/Final")
data=read.csv("telecomfinal.csv",na.strings=c(""," "))
str(data)
summary(data$avgmou)
summary(data$churn)
deciles<-quantile(data$avgmou,probs = seq(0,1,0.1))
data$avmou_groups<-ifelse()### bad way
data$avgmou_group<-cut(data$avgmou,deciles,include.lowest = T)
churn=aggregate(data$churn,by=list(data$avgmou_group),sum)

library(dplyr)
count=data%>%group_by(avgmou_group)%>%summarise(n())
churn$Count=count$`n()`
churn

create_bins<-function(col,data)
{
  
}