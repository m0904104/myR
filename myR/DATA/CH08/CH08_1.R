#時間序列分析 2017/09/01
#檔名CH08_1.R 資料檔CH08_1.dat
#國小男教師人數97至105學年度
#切換至工作目錄
setwd("D:/DATA/CH08/")
pe <- scan("CH08_1.dat", skip=1)
head(pe)
tail(pe)
library(forecast)
tspe <- ts(pe, start=c(2008))
print(tspe)
plot.ts(tspe)

tsped1<-diff(tspe,differences=1)
plot.ts(tsped1)
tsped2<-diff(tspe,differences=2)
plot.ts(tsped2)

acf(tsped2,lag.max=20)
acf(tsped2,lag.max=20,plot=FALSE)

pacf(tsped2,lag.max=20)
pacf(tsped2,lag.max=20,plot=FALSE)

tspearima<-arima(tspe,order=c(0,1,0))
print(tspearima)

BIC(tspearima)
predict(tspearima, 3)

tspearimaf <- forecast(tspearima, h=3, level=c(80,95))
print(tspearimaf)

plot(tspearimaf)

acf(tspearimaf$residuals,lag.max=20)
acf(tspearimaf$residuals,lag.max=20,plot=FALSE)
Box.test(tspearimaf$residuals, lag=3, type="Ljung-Box")

plot.ts(tspearimaf$residuals)

fit <- auto.arima(tspe)
print(fit)
forecast(fit,h=3)
plot(forecast(fit,h=3))
Box.test(fit$residuals, lag=3, type="Ljung-Box")

plot(tspe,xlab="TIME",ylab="NUM",main="")
grid()
points(tspe, type="o",xlab="TIME",ylab="NUM")
tspeho1 <- HoltWinters(tspe, beta=FALSE, gamma=FALSE)
print(tspeho1)
print(tspeho1$fitted)
tspeho2 <- forecast(tspeho1, h=3)
print(tspeho2)

plot(tspe, type='o', xlab="TIME", ylab="NUM")
lines(c(2009:2016), tspeho1$fitted[,1],type='o', lty=2)
legend(x="topright",legend=c("預測","實際"),lty=1:3)

plot(tspeho2, type='o', lty=2, xlab="TIME", ylab="NUM", main="")

tspeho3 <- HoltWinters(tspe, gamma=FALSE)
print(tspeho3)
tspeho4 <- forecast(tspeho3, h=3)
print(tspeho4)

plot(tspe, type='o', xlab="TIME", ylab="NUM")
lines(c(2010:2016), tspeho3$fitted[,1],type='o', lty=2)
legend(x="topright",legend=c("預測","實際"),lty=1:3)

plot(tspeho4, type='o', lty=2, xlab="TIME", ylab="NUM", main="")