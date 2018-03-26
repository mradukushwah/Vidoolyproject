addata=read.csv("ad_org_test1.csv")
install.packages("stringr")
library(stringr)
q<-addata$duration
p<-substr(q,3,str_length(q))
k=regexpr("H",p)
l=regexpr("M",p)
m=regexpr("S",p)
k=as.numeric(k)
l=as.numeric(l)
m=as.numeric(m)
length=length(p)
hour=c(0)
min=c(0)
sec=c(0)
for(i in 1:length)
{
  
  if(k[i]==-1&&l[i]==-1&&m[i]==-1)
  {hour[i]=0
  min[i]=0
  sec[i]=c(0)}
  
  else if(k[i]==-1&&l[i]==-1&&m[i]!=-1)
  {hour[i]=0
  sec[i]=substr(p[i],1,m[i]-1)
  min[i]=0}
  
  else if(k[i]==-1&&l[i]!=-1&&m[i]==-1)
  {hour[i]=0
  min[i]=substr(p[i],1,l[i]-1)
  sec[i]=0}
  
  if(k[i]==-1&&l[i]!=-1&&m[i]!=-1)
  {hour[i]=0
  min[i]=substr(p[i],1,l[i]-1)
  sec[i]=substr(p[i],l[i]+1,m[i]-1)}
  
  if(k[i]!=-1&&l[i]==-1&&m[i]==-1)
  {hour[i]=substr(p[i],1,k[i]-1)
  min[i]=0
  sec[i]=0}
  
  if(k[i]!=-1&&l[i]==-1&&m[i]!=-1)
  {hour[i]=substr(p[i],1,k[i]-1)
  min[i]=0
  sec[i]=substr(p[i],k[i]+1,m[i]-1)}
  
  if(k[i]!=-1&&l[i]!=-1&&m[i]==-1)
  {hour[i]=substr(p[i],1,k[i]-1)
  min[i]=substr(p[i],k[i]+1,l[i]-1)
  sec[i]=0}
  
  if(k[i]!=-1&&l[i]!=-1&&m[i]!=-1)
  {hour[i]=substr(p[i],1,k[i]-1)
  min[i]=substr(p[i],k[i]+1,l[i]-1)
  sec[i]=substr(p[i],l[i]+1,m[i]-1)}
  
}

hour=as.numeric(hour)
min=as.numeric(min)
sec=as.numeric(sec)
time_in_sec=hour*60*60+min*60+sec

install.packages("dplyr")
library(dplyr)
names(addata)
adnew<-cbind(addata,time_in_sec)

install.packages("lubridate")
library(lubridate)
date<-dmy(adnew$published)
no_days_old=today()-date
no_days_old=as.numeric(no_days_old)
adnew<-cbind(adnew,no_days_old)
names(adnew)
adnew<-adnew[,c(2,3,5,9)]
write.csv(adnew,"addata_test.csv")