#共變數分析 2017/08/10
#組別有差,前測有差
#檔名CH05_2.R 資料檔CH05_2.csv
#設定工作目錄
setwd("D:/DATA/CH05/")
library(readr)
sdata0 <- read_csv("CH05_2.csv")
head(sdata0)
tail(sdata0)
str(sdata0)
sdata0$GROUP <- factor(sdata0$GROUP)
print(sdata0$GROUP)
table(sdata0$GROUP)
contrasts(sdata0$GROUP)

pmodel2 <- aov(POST~PRE*GROUP, data=sdata0, contrasts=list(GROUP=contr.sum))

library(car)
Anova(pmodel2, type=3)

pmodel3 <- (aov(POST~PRE+GROUP, data=sdata0, contrasts=list(GROUP=contr.sum)))
Anova(pmodel3, type=3)

library(effects)
adj_means <- effect("GROUP", pmodel3)
print(adj_means)

pmodel4 <- aov(POST~PRE+GROUP, data=sdata0, contrasts=list(GROUP=contr.treatment))
summary.lm(pmodel4)

library(multcomp)
HSD <- glht(pmodel3, infct=mcp(GROUP="Tukey"))
summary(HSD)
print(HSD)

sdata0.g1 <- subset(sdata0, GROUP=="1")
sdata0.g2 <- subset(sdata0, GROUP=="2")
sdata0.g3 <- subset(sdata0, GROUP=="3")

mean(sdata0.g1$POST)
mean(sdata0.g2$POST)
mean(sdata0.g3$POST)
sd(sdata0.g1$POST)
sd(sdata0.g2$POST)
sd(sdata0.g3$POST)

head(sdata0.g3)
reg.g1 <- lm(POST ~ PRE , data=sdata0.g1)
reg.g2 <- lm(POST ~ PRE , data=sdata0.g2)
reg.g3 <- lm(POST ~ PRE , data=sdata0.g3)
anova(reg.g3)

plot(sdata0$POST ~ sdata0$PRE, type="n")
points(sdata0.g1$PRE, sdata0.g1$POST, pch=1)
points(sdata0.g2$PRE, sdata0.g2$POST, pch=2)
points(sdata0.g3$PRE, sdata0.g3$POST, pch=3)
abline(reg.g1, lty=1)
abline(reg.g2, lty=2)
abline(reg.g3, lty=3)
legend("bottomright",c("G1","G1","G2"), lty=c(1,2,3), pch=c(1,2,3))

anova(pmodel4)
library(DescTools)
EtaSq(pmodel4)