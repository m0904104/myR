#��]�l�ܲ��Ƥ��R 2017/08/04
#�ɦWCH04_2.R �����CH04_1.csv
#�]�w�u�@�ؿ�
setwd("D:/DATA/CH04/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH04_1.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
#�n�NGENDER�H��JOB�ର�r��
sdata0$JOB <- factor(sdata0$JOB)
print(sdata0$JOB)
sdata0$JOB <- factor(sdata0$JOB, levels = c(1,2,3,4), labels = c("�D��", "�ժ�","���","�ť�"))
print(head(sdata0$JOB))
#�p����q���`�M
#1:ID
#2:GENDER
#3:JOB
#4-9:A0101-A0106    �ѻP�հȨM��
#10-15:A0201-A0206  �i�{�Ыǻ��
#16-19:A0301-A0304  �P�i�P���X�@
#20-22:A0401-A0403  ���ɱM�~����
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
head(sdata0)

library(Rmisc)
#��]�l�ܲ��Ƥ��R
#1.�C�X�˥��K�n
summarySE(data=sdata0, groupvars="JOB", measurevar="SA01")
#2.�ܲ��ƦP����˩w
library(DescTools)
LeveneTest(SA01 ~ JOB, data=sdata0, center=mean)
#3.�ܲ��Ƥ��R(�K�n��)
mjob <- aov(SA01 ~ JOB, data=sdata0)
anova(mjob)
#4-1.�]���F��ۻݭn�ƫ���
#LSD
library(agricolae)
mLSD <- LSD.test(mjob, "JOB")
print(mLSD)
#Turkey
mTUKEY <- HSD.test(mjob, "JOB")
print(mTUKEY)
#Scheffe
mSCHEFFE <- scheffe.test(mjob, "JOB")
print(mSCHEFFE)

#�ƫ������t�@�Ӥ�k
#LSD�t�@�ؤ�k
library(DescTools)
mLSD <- PostHocTest(mjob, method="lsd")
print(mLSD)
#Tukey�t�@�ؤ�k
mTUKEY <- TukeyHSD(mjob)
print(mTUKEY)
plot(mTUKEY)
#scheffe�t�@�ؤ�k
mSCHEFFE <- ScheffeTest(mjob)
#�t�@�ػy�k
mSCHEFFE <- PostHocTest(mjob, method="scheffe")
print(mSCHEFFE)
plot(mSCHEFFE)

#4-2.�ܲ��Ƥ��P�誺�ƫ�����k
#�Q��Dunnnett C
library(DTK)
mDTK <- with(sdata0, DTK.test(SA01, JOB))
print(mDTK)
DTK.plot(mDTK)
abline(v=0)

#5.�p��ĪG�q
library(DescTools)
mETA <- EtaSq(mjob, anova=TRUE)
print(mETA)