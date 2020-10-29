#驗證性因素分析 2017/07/30 有分量表
#檔名CH03_3.R 資料檔CH03_3.csv
#設定工作目錄
setwd("D:/DATA/CH03/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH03_3.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
#讀取資料,去除編號欄位
sdata1 <- sdata0[,-1]
head(sdata1)

#驗證式因素分析
#套用lavaan套件
library(lavaan)
#假設估計的模型
cfa.model1 <- 'f1=~B101+B102+B103+B104+B105+B106
              f2=~B401+B402+B403+B404+B405+B406
              f3=~B303+B304+B305+B306
              f4=~B201+B202+B203'
#進行驗證性因素分析的參數估計
fit1 <- cfa(cfa.model1, data=sdata1)
#summary(fit1, standardized=TRUE, rsquare=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
#檢視適配指標
fitMeasures(fit1)
#檢視修正指標
mi <- modindices(fit1)
print(head(mi))
#繪圖
library(semPlot)
semPaths(fit1, what="path", layout="tree2", whatLabels="std",style="lisrel",edge.color=c("black"),nDigits=3)

#另一個模型
#假設估計的模型
cfa.model2 <- 'f1=~B101+B102+B103
              f2=~B401+B405+B406
              f3=~B304+B305+B306
              f4=~B201+B202+B203'
#進行驗證性因素分析的參數估計
fit2 <- cfa(cfa.model2, data=sdata1)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
#檢視適配指標
fitMeasures(fit2)
#檢視修正指標
mi <- modindices(fit2)
summary(mi)
#參數指標
parameterEstimates(fit2)
#標準化參數
standardizedSolution(fit2)
fitted(fit2)
resid(fit2, type = "standardized")
#AIC and BIC
AIC(fit2)
BIC(fit2)
fitMeasures(fit2)
#繪製模型圖
semPaths(fit2, what="path", layout="tree2", whatLabels="std",style="lisrel",nDigits=2)
semPaths(fit2, what="path", layout="tree2", whatLabels="std",style="ram",nDigits=2)