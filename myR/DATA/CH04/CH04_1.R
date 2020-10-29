#t���� 2017/08/04
#�ɦWCH04_1.R �����CH04_1.csv
#�]�w�u�@�ؿ�
setwd("D:/DATA/CH04/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH04_1.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
#�n�NGENDER�H��JOB�ର�r��
sdata0$GENDER <- factor(sdata0$GENDER)
print(sdata0$GENDER)
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
#�W�߼˥�t����
#1.���C�X�˥��K�n
library(Rmisc)
summarySE(data=sdata0, groupvars="GENDER", measurevar="SA01")
#2.�ܲ��ƦP����˩w
library(DescTools)
#�ܲ��ƦP����˩w�A���w�ȬO����ơASPSS�O�ĥΥ����ơA���F����_���A�d�ҥHmean�ܽd
LeveneTest(SA01 ~ GENDER, data=sdata0, center=mean)
#3.t����(�ܲ��Ƥ��P�誺��k)
mgender1 <- t.test(SA01 ~ GENDER, data=sdata0)
print(mgender1)
#3.t����(�ܲ��ƦP�誺��k)
mgender2 <-t.test(SA01 ~ GENDER, data=sdata0, var.equal=TRUE)
print(mgender2)