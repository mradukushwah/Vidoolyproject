gc()
library(data.table)
library(dplyr)
adnew<-fread("addata_train.csv")
adnew<-select(adnew,vidid:no_days_old)
adnew$views<-as.numeric(adnew$views)
adnew$likes<-as.numeric(adnew$likes)
adnew$dislikes<-as.numeric(adnew$dislikes)
adnew$comment<-as.numeric(adnew$comment)



chunk1<-adnew[sample(nrow(adnew), 1000,replace=F), ]


reg<-lm(adview~views+likes+dislikes+comment+time_in_sec+no_days_old,chunk1)
summary(reg)
#outlier detection
par(mfrow=c(3,3))
boxplot(chunk1$views)
boxplot(chunk1$likes)
boxplot(chunk1$dislikes)
boxplot(chunk1$comment)
boxplot(chunk1$time_in_sec)
boxplot(chunk1$no_days_old)
boxplot(chunk1$adview)


x <- chunk1$views
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
chunk1$views<-x
boxplot(chunk1$views)

y <- chunk1$likes
qnt <- quantile(y, probs=c(.25, .75), na.rm = T)
caps <- quantile(y, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt[1] - H)] <- caps[1]
z[z > (qnt[2] + H)] <- caps[2]
chunk1$likes<-y
boxplot(chunk1$likes)

z <- chunk1$dislikes
qnt <- quantile(z, probs=c(.25, .75), na.rm = T)
caps <- quantile(z, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(z, na.rm = T)
z[z < (qnt[1] - H)] <- caps[1]
z[z > (qnt[2] + H)] <- caps[2]
chunk1$dislikes<-z
boxplot(chunk1$dislikes)

p <- chunk1$comment
qnt <- quantile(p, probs=c(.25, .75), na.rm = T)
caps <- quantile(p, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(p, na.rm = T)
p[p < (qnt[1] - H)] <- caps[1]
p[p > (qnt[2] + H)] <- caps[2]
chunk1$comment<-p
boxplot(chunk1$comment)

q <- chunk1$no_days_old
qnt <- quantile(q, probs=c(.25, .75), na.rm = T)
caps <- quantile(q, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(q, na.rm = T)
q[q < (qnt[1] - H)] <- caps[1]
q[q > (qnt[2] + H)] <- caps[2]
chunk1$no_days_old<-q
boxplot(chunk1$no_days_old)


r <- chunk1$time_in_sec
qnt <- quantile(r, probs=c(.25, .75), na.rm = T)
caps <- quantile(r, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(r, na.rm = T)
r[r < (qnt[1] - H)] <- caps[1]
r[r > (qnt[2] + H)] <- caps[2]
chunk1$time_in_sec<-r
boxplot(chunk1$time_in_sec)

s <- chunk1$adview
qnt <- quantile(s, probs=c(.25, .75), na.rm = T)
caps <- quantile(s, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(s, na.rm = T)
s[s < (qnt[1] - H)] <- caps[1]
s[s > (qnt[2] + H)] <- caps[2]
chunk1$adview<-s
boxplot(chunk1$adview)

reg1<-lm(adview~views+likes+dislikes+comment+time_in_sec+no_days_old,chunk1)
summary(reg1)
#Multicollinearity detection
install.packages("usdm")
library(usdm)
vif(chunk1[,c(3:6,8:9)])

#Auto Correlation
install.packages("lmtest")
library(lmtest)
dwtest(adview~views+likes+dislikes+comment+time_in_sec+no_days_old, data=chunk1)
#autocorrelation present

#Checking of presence of homiscedasticity: If no specific pattern then presence of homoscedasticity
#heteroscedasticity
par(mfrow=c(1,1))
plot(reg1$fit,reg1$residuals)

#fitting the best model
reduced_model=step(reg1, direction = "backward")

final_model=lm(adview ~ views + likes + comment + time_in_sec,chunk1)
summary(final_model)
# PREDICTING 
addata_test=read.csv("addata_test.csv")
addata_test$views<-as.numeric(addata_test$views)
addata_test$likes<-as.numeric(addata_test$likes)
addata_test$comment<-as.numeric(addata_test$comment)
ad_view<-round(predict(final_model,addata_test))
ad<-read.csv("ad_org_test1.csv")
final_table<-data.frame(ad$vidid,ad_view)
write.csv(final_table,"adview.csv")
