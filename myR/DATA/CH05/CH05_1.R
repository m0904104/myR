#共變數分析 2017/08/10
#檔名CH05_1.R 資料檔CH05_1.csv
#設定工作目錄
setwd("D:/DATA/CH05/")
library(readr)
sdata0 <- read_csv("CH05_1.csv")
head(sdata0)
tail(sdata0)
str(sdata0)
sdata0$GENDER <- factor(sdata0$GENDER)
print(sdata0$GENDER)
sdata0$GROUP <- factor(sdata0$GROUP)
print(sdata0$GROUP)

table(sdata0$GENDER)
table(sdata0$GROUP)
pmodel2 <- aov(BPOST~BPRE*GROUP, data=sdata0, contrasts=list(GROUP=contr.sum))

library(car)
Anova(pmodel2, type=3)
pmodel3 <- aov(BPOST~BPRE+GROUP, data=sdata0, contrasts=list(GROUP=contr.sum))
pmodel3$contrasts
Anova(pmodel3, type=3)

library(effects)
adj_means <- effect("GROUP", pmodel3)
print(adj_means)
pmodel4 <- aov(BPOST~BPRE+GROUP, data=sdata0, contrasts=list(GROUP=contr.treatment))
summary.lm(pmodel4)

sdata0.g0 <- subset(sdata0, GROUP=="0")
sdata0.g1 <- subset(sdata0, GROUP=="1")
head(sdata0.g1)
mean(sdata0.g0$BPRE)
mean(sdata0.g1$BPRE)
mean(sdata0.g0$BPOST)
mean(sdata0.g1$BPOST)
sd(sdata0.g0$BPOST)
sd(sdata0.g1$BPOST)
reg.g0 <- lm(BPOST ~ BPRE , data=sdata0.g0)
reg.g1 <- lm(BPOST ~ BPRE , data=sdata0.g1)
anova(reg.g1)

plot(sdata0$BPOST ~ sdata0$BPRE, type="n")
points(sdata0.g0$BPRE, sdata0.g0$BPOST, pch=1)
points(sdata0.g1$BPRE, sdata0.g1$BPOST, pch=2)
abline(reg.g0, lty=1)
abline(reg.g1, lty=2)
legend("bottomright",c("G0","G1"), lty=c(1,2,3), pch=c(1,2,3))

library(multcomp)
HSD <- glht(pmodel4, infct=mcp(GROUP="Tukey"))
summary(HSD)

anova(pmodel4)
library(DescTools)
EtaSq(pmodel4)