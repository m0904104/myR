#���ҩʦ]�����R 2017/07/30 �����q��
#�ɦWCH03_3.R �����CH03_3.csv
#�]�w�u�@�ؿ�
setwd("D:/DATA/CH03/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH03_3.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
#Ū�����,�h���s�����
sdata1 <- sdata0[,-1]
head(sdata1)

#���Ҧ��]�����R
#�M��lavaan�M��
library(lavaan)
#���]���p���ҫ�
cfa.model1 <- 'f1=~B101+B102+B103+B104+B105+B106
              f2=~B401+B402+B403+B404+B405+B406
              f3=~B303+B304+B305+B306
              f4=~B201+B202+B203'
#�i�����ҩʦ]�����R���ѼƦ��p
fit1 <- cfa(cfa.model1, data=sdata1)
#summary(fit1, standardized=TRUE, rsquare=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
#�˵��A�t����
fitMeasures(fit1)
#�˵��ץ�����
mi <- modindices(fit1)
print(head(mi))
#ø��
library(semPlot)
semPaths(fit1, what="path", layout="tree2", whatLabels="std",style="lisrel",edge.color=c("black"),nDigits=3)

#�t�@�Ӽҫ�
#���]���p���ҫ�
cfa.model2 <- 'f1=~B101+B102+B103
              f2=~B401+B405+B406
              f3=~B304+B305+B306
              f4=~B201+B202+B203'
#�i�����ҩʦ]�����R���ѼƦ��p
fit2 <- cfa(cfa.model2, data=sdata1)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
#�˵��A�t����
fitMeasures(fit2)
#�˵��ץ�����
mi <- modindices(fit2)
summary(mi)
#�Ѽƫ���
parameterEstimates(fit2)
#�зǤưѼ�
standardizedSolution(fit2)
fitted(fit2)
resid(fit2, type = "standardized")
#AIC and BIC
AIC(fit2)
BIC(fit2)
fitMeasures(fit2)
#ø�s�ҫ���
semPaths(fit2, what="path", layout="tree2", whatLabels="std",style="lisrel",nDigits=2)
semPaths(fit2, what="path", layout="tree2", whatLabels="std",style="ram",nDigits=2)