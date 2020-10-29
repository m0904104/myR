#�q����������R 2017/07/30 �������]�����R
#�ɦWCH03_2.R �����CH03_1.csv
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
print(pnum)
print(snum)
#Ū�����,�h���s�����
sdata1 <- sdata0[,-1]
head(sdata1)
#�]�����R�p��ī�
library(psych)
#�������]�����R
#KMO
KMO(sdata1)
#Bartlett�y���˩w
cortest.bartlett(sdata1)
#�~�Y��
fa.parallel(sdata1, fa="pc", show.legend = FALSE)
print.psych(fa(sdata1, fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)
#�R��B302(14),301(13),204(10)
print.psych(fa(sdata1[-c(10,13,14)], fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)
#��3���A�R��205(11)
print.psych(fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)

#��e�t������(cross loading)�ĥαץ���b
print.psych(fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="promax"),cut=0.35,sort=TRUE)
#�]�����R���Gø��
fit1 <-fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="promax")
fa.diagram(fit1,cut=0.35,sort=TRUE,digits=2)

#�p��R�D��U���q�����H��
#�}�l�p��
#�R��10,11,13,14
#X1:1:6
#X2:7:9,12
#X3:15:18
#X4:19:24
alpha(sdata1[-c(10,11,13,14)])
alpha(sdata1[-c(10,11,13,14)])$total$raw_alpha
#���p�����q���i�H�Q��list�@���p��
subdata <- list(x0=sdata1[-c(10,11,13,14)], x1=sdata1[,1:6], x2=sdata1[,c(7,8,9,12)], x3=sdata1[,15:18], x4=sdata1[,19:24])

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

#�D�������R
library(psych)
fit2 <- principal(sdata1, nfactors=4, rotate="varimax")
print(fit2, digits=2, cut=0.35, sort=TRUE)

fit3 <- principal(sdata1, nfactors=4, rotate="promax")
print(fit3, digits=2, cut=0.35, sort=TRUE)
fit4<- principal(sdata1[-c(9,10,13,14)],nfactors=4,rotate="promax")
print(fit4, digits=2, cut=0.35, sort=TRUE)
fit5<- principal(sdata1[-c(9,10,11,13,14)],nfactors=4,rotate="promax")
print(fit5, digits=2, cut=0.35, sort=TRUE)
#��ܦ]�����R���G�����]���t���q
print(loadings(fit5),digits=2,cut=0.35, sort=TRUE)
#ø�s�ϧ�
plot(fit5)
print(fit5$scores,digits=2)