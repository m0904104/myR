#卡方考驗 2017/11/19
#檔名CH07_1.R 資料檔CH07_1.csv CH07_2.csv
#設定工作目錄
setwd("D:/DATA/CH07/")
sdata0 <- c(96,104)
print(sdata0)
chisq.test(sdata0)
#期望次數比率相同
presult0 <- chisq.test(sdata0)
presult01 <- data.frame(presult0$observed,presult0$expected,
                        presult0$observed-presult0$expected,
                        (presult0$observed-presult0$expected)^2,
                        (presult0$observed-presult0$expected)^2/presult0$expected)

names(presult01)<- c("O","E","(O-E)","(O-E)^2","Chi-Square")
print(presult01)
presult0 <- chisq.test(c(38,56,44,56,66,40))
presult01 <- data.frame(presult0$observed,presult0$expected,
                        presult0$observed-presult0$expected,
                        (presult0$observed-presult0$expected)^2,
                        (presult0$observed-presult0$expected)^2/presult0$expected)

names(presult01)<- c("O","E","(O-E)","(O-E)^2","Chi-Square")
print(presult01)

chisq.test(sdata0, p=c(0.50, 0.50))
chisq.test(sdata0, p=c(1, 1)/2)
edata0 <- c(1,1)
chisq.test(sdata0, p=edata0, rescale.p = TRUE)
#期望次數比率不同
sdata11 <- c(168,32)
sdata12 <- c(180,20)/200
presult11 <- chisq.test(sdata11, p=sdata12)
print(presult11)

presult12 <- data.frame(presult11$observed,presult11$expected,
                        presult11$observed-presult11$expected,
                        (presult11$observed-presult11$expected)^2,
                        (presult11$observed-presult11$expected)^2/presult11$expected,
                        presult11$residuals)

names(presult12)<- c("O","E","(O-E)","(O-E)^2",
                     "Chi-Square","Residuals")
print(presult12)
#適合度考驗
sdata21 <- c(192,302,318,132,38)
sdata22 <- c(0.20,0.30,0.30,0.15,0.05)
presult21 <- chisq.test(sdata21, p=sdata22)
print(presult21)
#獨立性考驗
library(readr)
sdata31 <- read_csv("D:/DATA/CH07/CH07_1.csv")
head(sdata31)
sdata32 <- xtabs(人數 ~ 狀態+支持, data=sdata31)
print(sdata32)
presult31 <- chisq.test(sdata32)
print(presult31)
presult41 <- chisq.test(sdata32, correct=FALSE)
print(presult41)
#另一種獨立性檢定
summary(sdata32)
#同質性檢定
library(readr)
sdata41 <- read_csv("D:/DATA/CH07/CH07_2.csv")
head(sdata41)
print(sdata41)
sdata42 <- xtabs(次數 ~ 態度+團體, data=sdata41)
print(sdata42)
presult51 <- chisq.test(t(sdata42))
print(presult51)

presult4 <- chisq.test(sdata42, correct=FALSE)
print(presult4)
ar_presult4 <- presult4$stdres
print(ar_presult4)
psig <- 0.05
adj_psig <- psig/((nrow(sdata42)-1)*(ncol(sdata42)-1))
print(adj_psig)

print(qnorm(adj_psig/2))
#事後比較
library(fifer)
chisq.post.hoc(sdata42, popsInRows = FALSE, 
               control=c("bonferroni"),digits=6)