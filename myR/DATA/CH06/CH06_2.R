#相關與迴歸 2017/08/04
#檔名CH06_2.R 資料檔CH06_1.csv
#設定工作目錄
setwd("D:/DATA/CH06/")
library(readr)
sdata0 <- read_csv("CH06_1.csv")
head(sdata0)
tail(sdata0)
str(sdata0)

sdata0$GENDER <- factor(sdata0$GENDER)
print(sdata0$GENDER)
sdata0$JOB <- factor(sdata0$JOB)
print(sdata0$JOB)
sdata0$JOB <- factor(sdata0$JOB, levels = c(1,2,3,4), labels = c("主任", "組長","科任","級任"))
print(head(sdata0$JOB))

sdata0$A01 <- apply(sdata0[4:9],1,sum)
sdata0$A02 <- apply(sdata0[10:15],1,sum)
sdata0$A03 <- apply(sdata0[16:19],1,sum)
sdata0$A04 <- apply(sdata0[20:22],1,sum)
sdata0$A00 <- apply(sdata0[4:22],1,sum)
sdata0$SA01 <- apply(sdata0[4:9],1,mean)
sdata0$SA02 <- apply(sdata0[10:15],1,mean)
sdata0$SA03 <- apply(sdata0[16:19],1,mean)
sdata0$SA04 <- apply(sdata0[20:22],1,mean)
sdata0$SA00 <- apply(sdata0[4:22],1,mean)

sdata0$B01 <- apply(sdata0[23:30],1,sum)
sdata0$B02 <- apply(sdata0[31:36],1,sum)
sdata0$B03 <- apply(sdata0[37:40],1,sum)
sdata0$B04 <- apply(sdata0[41:43],1,sum)
sdata0$B00 <- apply(sdata0[23:43],1,sum)
sdata0$SB01 <- apply(sdata0[23:30],1,mean)
sdata0$SB02 <- apply(sdata0[31:36],1,mean)
sdata0$SB03 <- apply(sdata0[37:40],1,mean)
sdata0$SB04 <- apply(sdata0[41:43],1,mean)
sdata0$SB00 <- apply(sdata0[23:43],1,mean)

sdata0$C01 <- apply(sdata0[44:50],1,sum)
sdata0$C02 <- apply(sdata0[51:55],1,sum)
sdata0$C03 <- apply(sdata0[56:59],1,sum)
sdata0$C04 <- apply(sdata0[60:63],1,sum)
sdata0$C00 <- apply(sdata0[44:63],1,sum)
sdata0$SC01 <- apply(sdata0[44:50],1,mean)
sdata0$SC02 <- apply(sdata0[51:55],1,mean)
sdata0$SC03 <- apply(sdata0[56:59],1,mean)
sdata0$SC04 <- apply(sdata0[60:63],1,mean)
sdata0$SC00 <- apply(sdata0[44:63],1,mean)

head(sdata0)
#迴歸
mREG1 <- lm(B00~A00, data=sdata0)
anova(mREG1)

library(lm.beta)
mREG2 <- lm.beta(mREG1)
summary(mREG2)

confint(mREG1, level=0.95)
#多元迴歸
mREG31 <- lm(B00 ~ A01 + A02 + A03+ A04, data=sdata0)
summary(mREG31) # show results
mREG32 <- lm(B00 ~ A01 + A03+ A04, data=sdata0)
summary(mREG32)

coefficients(mREG32)
confint(mREG32, level=0.95)
anova(mREG32)

mREG33 <- lm.beta(mREG32)
summary(mREG33)

library(MASS)
mREG41 <- lm(B00~A01+A02+A03+A04,data=sdata0)
step1 <- stepAIC(mREG41, direction="both")
print(step1)
summary(step1)
anova(step1)
coefficients(step1)
residuals(step1)
influence(step1)
fitted(step1)
fitted.values(step1)
confint(step1)
#標準化迴歸係數
mREG42 <- lm(B00~A03+A01+A04,data=sdata0)
step2 <- stepAIC(mREG42, direction="both")
summary(step2)
mREG43 <- lm.beta(step2)
summary(mREG43)

mREG44 <- lm(B00~A03+A01,data=sdata0)
step3 <- stepAIC(mREG44, direction="both")
summary(step3)
mREG45 <- lm.beta(step3)
summary(mREG45)

mREG46 <- lm(B00~A03,data=sdata0)
step4 <- stepAIC(mREG46, direction="both")
summary(step4)
mREG47 <- lm.beta(step4)
summary(mREG47)
#多元共線性
library(mctest)
x <- sdata0[,c("A03","A01","A04")]
y <- sdata0[,"B00"]
omcdiag (x, y)
imcdiag (x, y)
mctest (x,y)