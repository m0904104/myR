#共變數分析 2017/08/10
#迴歸同質性顯著
#檔名CH05_3.R 資料檔CH05_3.csv
#設定工作目錄
setwd("D:/DATA/CH05/")
library(readr)
sdata0 <- read_csv("CH05_3.csv")
head(sdata0)
tail(sdata0)
str(sdata0)
sdata0$GROUP <- factor(sdata0$GROUP)
print(sdata0$GROUP)

table(sdata0$GROUP)

pmodel2 <- aov(POST~PRE*GROUP, data=sdata0, contrasts=list(GROUP=contr.sum))

library(car)
Anova(pmodel2, type=3)

sdata0.g1 <- subset(sdata0, GROUP=="0")
sdata0.g2 <- subset(sdata0, GROUP=="1")
head(sdata0.g2)
reg.g1 <- lm(POST ~ PRE , data=sdata0.g1)
reg.g2 <- lm(POST ~ PRE , data=sdata0.g2)
anova(reg.g2)
plot(sdata0$POST ~ sdata0$PRE, type="n")
points(sdata0.g1$PRE, sdata0.g1$POST, pch=1)
points(sdata0.g2$PRE, sdata0.g2$POST, pch=2)
abline(reg.g1, lty=1)
abline(reg.g2, lty=2)
legend("bottomright",c("G1","G2"), lty=c(1,2), pch=c(1,2))