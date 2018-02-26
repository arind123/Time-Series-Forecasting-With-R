# To replace NA by MEAN
# model Pt/P(t-1)
#AndamanNicobar$ANNUAL <- ifelse(is.na(AndamanNicobar$ANNUAL), mean(AndamanNicobar$ANNUAL, na.rm=TRUE), AndamanNicobar$ANNUAL)
library(zoo)
library(ggplot2)
library(reshape2) 
library(tseries)
library(urca)
library(astsa)
library(itsmr)
library(forecast)
library(tsDyn)
library(changepoint)
library(urca)
library(caret)

#Load the data 
rain = rainfall_area_wt_sd_1901_2015
rain$SUBDIVISION = as.factor(rain$SUBDIVISION)
summary(rain)
summary(rain$SUBDIVISION)
any(is.na(rain$ANNUAL))

#First we pick the first of the SUBDIVISIONS : a)"ANDAMAN & NICOBAR ISLANDS"
AndamanNicobar = subset(rain, SUBDIVISION == "ANDAMAN & NICOBAR ISLANDS")
str(AndamanNicobar)
#DATA 1943 1944 1945 not in the list
#DATA CLEANING::Check if there are any missing values
any(is.na(AndamanNicobar$YEAR)) #FALSE
any(is.na(AndamanNicobar$JAN))  #FALSE
any(is.na(AndamanNicobar$FEB))  #FALSE
any(is.na(AndamanNicobar$MAR))  #TRUE
AndamanNicobar$MAR <- ifelse(is.na(AndamanNicobar$MAR), mean(AndamanNicobar$MAR, na.rm=TRUE), AndamanNicobar$MAR)
any(is.na(AndamanNicobar$APR))  #TRUE
AndamanNicobar$APR <- ifelse(is.na(AndamanNicobar$APR), mean(AndamanNicobar$APR, na.rm=TRUE), AndamanNicobar$APR)
any(is.na(AndamanNicobar$MAY))  #TRUE
AndamanNicobar$MAY <- ifelse(is.na(AndamanNicobar$MAY), mean(AndamanNicobar$MAY, na.rm=TRUE), AndamanNicobar$MAY)
any(is.na(AndamanNicobar$JUN))  #TRUE
AndamanNicobar$JUN <- ifelse(is.na(AndamanNicobar$JUN), mean(AndamanNicobar$JUN, na.rm=TRUE), AndamanNicobar$JUN)
any(is.na(AndamanNicobar$JUL))  #TRUE
AndamanNicobar$JUL <- ifelse(is.na(AndamanNicobar$JUL), mean(AndamanNicobar$JUL, na.rm=TRUE), AndamanNicobar$JUL)
any(is.na(AndamanNicobar$AUG))  #TRUE
AndamanNicobar$AUG <- ifelse(is.na(AndamanNicobar$AUG), mean(AndamanNicobar$AUG, na.rm=TRUE), AndamanNicobar$AUG)
any(is.na(AndamanNicobar$SEP))  #TRUE
AndamanNicobar$SEP <- ifelse(is.na(AndamanNicobar$SEP), mean(AndamanNicobar$SEP, na.rm=TRUE), AndamanNicobar$SEP)
any(is.na(AndamanNicobar$OCT))  #TRUE
AndamanNicobar$OCT <- ifelse(is.na(AndamanNicobar$OCT), mean(AndamanNicobar$OCT, na.rm=TRUE), AndamanNicobar$OCT)
any(is.na(AndamanNicobar$NOV))  #TRUE
AndamanNicobar$NOV <- ifelse(is.na(AndamanNicobar$NOV), mean(AndamanNicobar$NOV, na.rm=TRUE), AndamanNicobar$NOV)
any(is.na(AndamanNicobar$DEC))  #TRUE
AndamanNicobar$DEC <- ifelse(is.na(AndamanNicobar$DEC), mean(AndamanNicobar$DEC, na.rm=TRUE), AndamanNicobar$DEC)

any(is.na(AndamanNicobar$ANNUAL)) #FALSE
AndamanNicobar$ANNUAL <- ifelse(is.na(AndamanNicobar$ANNUAL), mean(AndamanNicobar$ANNUAL, na.rm=TRUE), AndamanNicobar$ANNUAL)

#Read as time series data by dividing the data set in 5 parts:

N1=69
A1 =head(AndamanNicobar, -N1)
View(A1)
AndamanNicobarTS_01_42 = ts(as.vector(t(as.matrix(A1[,3:14]))),start=c(1901,1),end=c(1942,12),frequency = 12)
plot.ts(AndamanNicobarTS_01_42,type='o',main="Rainfall in AndamanNicobar per month for years from 1901-1942",ylab = "Rainfall Amount")

N2= 1
M2 = 41
A =head(AndamanNicobar, -N2)
View(A)
A2 =tail(A, -M2)
View(A2)
AndamanNicobarTS_46_14 = ts(as.vector(t(as.matrix(A2[,3:14]))),start=c(1946,1),end=c(2014,12),frequency = 12)
plot.ts(AndamanNicobarTS_46_14,type='o',main="Rainfall in AndamanNicobar per month for years from 1946-2014",ylab = "Rainfall Amount")

mvalue = cpt.mean(AndamanNicobarTS_46_14, method="BinSeg",Q =2) #mean changepoints using PELT
cpts(mvalue) ##1976(360)
plot(mvalue,type = "l")

N2= 39
M2 = 41
A =head(AndamanNicobar, -N2)
View(A)
A3 =tail(A, -M2)
View(A3)
AndamanNicobarTS_46_76 = ts(as.vector(t(as.matrix(A3[,3:14]))),start=c(1946,1),end=c(1976,12),frequency = 12)

M2 = 71 
N=1
A =head(AndamanNicobar, -N)
View(A)
A4 =tail(A, -M2)
View(A4)
AndamanNicobarTS_77_14 = ts(as.vector(t(as.matrix(A4[,3:14]))),start=c(1977,1),end=c(2014,12),frequency = 12)

par(mfrow=c(2,2))
plot.ts(AndamanNicobarTS_01_42,type='o',main="Rainfall in AndamanNicobar per month for years from 1901-1942",ylab = "Rainfall Amount")
plot.ts(AndamanNicobarTS_46_90,type='o',main="Rainfall in AndamanNicobar per month for years from 1946-1990",ylab = "Rainfall Amount")
plot.ts(AndamanNicobarTS_46_76,type='o',main="Rainfall in AndamanNicobar per month for years from 1946-1976",ylab = "Rainfall Amount")
plot.ts(AndamanNicobarTS_77_14,type='o',main="Rainfall in AndamanNicobar per month for years from 1977-2014",ylab = "Rainfall Amount")



#Test for stationarity :Augmented Dickey-Fuller Test

#All proving to be stationary
summary(ur.df(AndamanNicobarTS_01_15,type='none'))  
adf.test(AndamanNicobarTS_01_15,alternative = "stationary")
summary(ur.df(AndamanNicobarTS_01_42,type='drift'))  
adf.test(AndamanNicobarTS_01_42,alternative = "stationary")
summary(ur.df(AndamanNicobarTS_46_76,type='drift'))  
adf.test(AndamanNicobarTS_46_76,alternative = "stationary")
summary(ur.df(AndamanNicobarTS_77_14,type='drift'))  
adf.test(AndamanNicobarTS_77_14,alternative = "stationary")


#Remove Seasonality and forecast
##### 1_42 #####
plot.ts(AndamanNicobarTS_01_42,type='o',main="Rainfall in AndamanNicobar per month for years from 1901-1942",ylab = "Rainfall Amount")
plot.ts(diff(AndamanNicobarTS_01_42,12),type='l',ylab="Twelve Diff.")
#plot.ts(diff(log(AndamanNicobarTS_01_42,12)),type='l',ylab="Twelve Diff.")
summary(ur.df(diff(AndamanNicobarTS_01_42,12),type='drift'))  

#adf.test(diff(AndamanNicobarTS_01_42,12))
AndamanNicobarTS_01_42Adj = diff(AndamanNicobarTS_01_42,12)
#AndamanNicobarTS_01_42Adj=diff(diff(AndamanNicobarTS_01_42,12),1)
plot.ts(AndamanNicobarTS_01_42Adj,type='l',main="Seasonality Removed Rainfall in AndamanNicobar per month for years from 1901-1942",ylab = "Rainfall Amount")
acf2(AndamanNicobarTS_01_42,max.lag = 60) 
acf2(AndamanNicobarTS_01_42Adj,max.lag = 60) 

sarima(AndamanNicobarTS_01_42,p=0,d=0,q=0,P=0,D=1,Q=1,S=12) 
y1 =sarima.for(AndamanNicobarTS_01_42,n.ahead=36,p=0,d=0,q=0,P=0,D=1,Q=1,S=12)  # SARIMA forecast 

#### 46_76 #####
plot.ts(AndamanNicobarTS_46_76,type='o',main="Rainfall in AndamanNicobar per month for years from 1946-1976",ylab = "Rainfall Amount")
acf2(AndamanNicobarTS_46_76,max.lag = 48)
plot.ts(diff(AndamanNicobarTS_46_76,12),type='l',ylab="Twelve Diff.",main="Differenced Rainfall in AndamanNicobar per month for years from 1946-1976")
summary(ur.df(diff(AndamanNicobarTS_46_76,12),type='drift'))  

AndamanNicobarTS_46_76Adj=diff(AndamanNicobarTS_46_76,12)
plot.ts(AndamanNicobarTS_46_76Adj,type='l',main="Seasonality Removed Rainfall in AndamanNicobar per month for years from 1946-1976",ylab = "Rainfall Amount")

acf2(AndamanNicobarTS_46_76Adj,max.lag = 48) 
#AndamanNicobarTS_46_76Adj_auto = autofit(AndamanNicobarTS_46_76Adj, p = 0:8, q = 0:8)
sarima(AndamanNicobarTS_46_76,p=0,d=0,q=0,P=0,D=1,Q=1,S=12) 

y =sarima.for(AndamanNicobarTS_46_76,n.ahead=12,p=0,d=0,q=0,P=0,D=1,Q=1,S=12)  # SARIMA forecast 
a = c(y$pred)
b=c(79.7,43.2,6.1,6.9,275.3,339.3,341.1,345.500,467.1,263.6,153.3,67.1) 
t.test(a,b, var.equal=TRUE, paired=FALSE)
my_RMSE1<-caret::RMSE(a,b)
my_RMSE1
x = 1:12
avg = y$pred
sdev = y$se
plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Months",ylab = "values",
     main="Line Graph with Sarima standard error bars and Observed data for Rainfall in year 1977",type = "o",col = "red"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
lines(z2,type = "o",col="green")
legend("topleft", legend = c("forecasts", "observed values"), 
       col = c("red", "green"), lty = c(1, 1), bty = "n")





##77_14##
plot.ts(AndamanNicobarTS_77_14,type='o',main="Rainfall in AndamanNicobar per month for years from 1977-2014",ylab = "Rainfall Amount")
plot.ts(diff(AndamanNicobarTS_77_14,12),type='l',ylab="Twelve Diff.")
summary(ur.df(AndamanNicobarTS_77_14,type='drift'))  
AndamanNicobarTS_77_14Adj=diff(AndamanNicobarTS_77_14,12)
plot.ts(AndamanNicobarTS_77_14Adj,type='l',main="Seasonality Removed Rainfall in AndamanNicobar per month for years from 1977-2014",ylab = "Rainfall Amount")

acf2(AndamanNicobarTS_77_14Adj,max.lag = 60) 

sarima(AndamanNicobarTS_77_14,p=0,d=0,q=0,P=0,D=1,Q=1,S=12) 
y1 =sarima.for(AndamanNicobarTS_77_14,n.ahead=12,p=0,d=0,q=0,P=0,D=1,Q=1,S=12)  # SARIMA forecast 
z1= c(y1$pred)
z2 = c(126.8,7.6,3.1,138.20,331.9,346.4,328.9,480,523.3,252.10,236.30,129.90)
t.test(z1,z2, var.equal=TRUE, paired=FALSE)

my_RMSE<-caret::RMSE(z1,z2)
my_RMSE


x = 1:12
avg = y1$pred
sdev = y1$se
plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Months",ylab = "values",
     main="Line Graph with Sarima standard error bars and Observed data for Rainfall in year 2015",type = "o",col = "red"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
lines(z2,type = "o",col="green")
legend("topleft", legend = c("forecasts", "observed values"), 
       col = c("red", "green"), lty = c(1, 1), bty = "n")

