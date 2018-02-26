write.csv(AndamanNicobar, file = "MyData.csv")
data = MyData
N1=1
A1 =head(data, -N1)
View(A1)
AndamanNicobarTS_01_14 = ts(as.vector(t(as.matrix(A1[,3:14]))),start=c(1901,1),end=c(2014,12),frequency = 12)
plot.ts(AndamanNicobarTS_01_14,type='o',main="Rainfall in AndamanNicobar per month for years from 1901-2014",ylab = "Rainfall Amount")

adf.test(AndamanNicobarTS_01_14,alternative = "stationary")
summary(ur.df(AndamanNicobarTS_01_14,type='drift'))  ##Stationary
plot.ts(diff(AndamanNicobarTS_01_14,12),type='l',ylab="Twelve Diff.")
acf2(AndamanNicobarTS_01_14,max.lag = 48)

AndamanNicobarTS_01_14Adj=diff(AndamanNicobarTS_01_14,12)
#plot.ts(AndamanNicobarTS_46_76Adj,type='l',main="Seasonality Removed Rainfall in AndamanNicobar per month for years from 1946-1976",ylab = "Rainfall Amount")

acf2(AndamanNicobarTS_01_14Adj,max.lag = 48) 
#AndamanNicobarTS_46_76Adj_auto = autofit(AndamanNicobarTS_46_76Adj, p = 0:8, q = 0:8)
sarima(AndamanNicobarTS_01_14,p=0,d=0,q=0,P=0,D=1,Q=1,S=12) 

y =sarima.for(AndamanNicobarTS_01_14,n.ahead=12,p=0,d=0,q=0,P=0,D=1,Q=1,S=12)  # SARIMA forecast 
a1 = c(y$pred)
a2 = c(126.8,7.6,3.1,138.20,331.9,346.4,328.9,480,523.3,252.10,236.30,129.90)
t.test(a1,a2, var.equal=TRUE, paired=FALSE)
my_RMSE2<-caret::RMSE(a1,a2)
my_RMSE2

x = 1:12
avg = y$pred
sdev = y$se
plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Months",ylab = "values",
     main="Line Graph with Sarima standard error bars and Observed data for Rainfall in year 2015(Whole Data)",type = "o",col = "red"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
lines(a2,type = "o",col="green")
legend("topleft", legend = c("forecasts", "observed values"), 
       col = c("red", "green"), lty = c(1, 1), bty = "n")



