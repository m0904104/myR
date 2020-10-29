#�q����������R 2017/07/30 �����q��
#�ɦWCH03_1.R �����CH03_1.csv
#�]�w�u�@�ؿ�
setwd("D:/DATA/CH03/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH03_1.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
#�p���D�ƻP�H��
pnum <- ncol(sdata0)-1  
snum <- nrow(sdata0)
#�˵��D�ƥH�ΤH��
pnum
snum
#Ū�����,�h���s�����
sdata1 <- sdata0[,-1]
head(sdata1)
#�p��H��
library(psych)
alpha(sdata1)
alpha(sdata1)$total$raw_alpha
#���p�����q���i�H�Q��list�@���p��
subdata <- list(x0=sdata1, x1=sdata1[,1:6], x2=sdata1[,7:12], x3=sdata1[,13:18], x4=sdata1[,19:24])
#�p���`�q���Τ��q���H��
palpha <- lapply(subdata, alpha)
#�`�q���H��
palpha$x0$total$raw_alpha
#���q��1�H��
palpha$x1$total$raw_alpha
#���q��2�H��
palpha$x2$total$raw_alpha
#���q��3�H��
palpha$x3$total$raw_alpha
#���q��4�H��
palpha$x4$total$raw_alpha