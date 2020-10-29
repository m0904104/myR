#�G�����D�����D���R 2017/07/29
#�ɦWCH02_1.R �����CH02_1.csv
#�]�w�u�@�ؿ�
setwd("C:/myR/DATA/CH02/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH02_1.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
str(sdata0)
#�]���Ĥ@��O���סA�ҥH�n�p��ɻݴ�1
pnum <- ncol(sdata0)-1  #�D��
snum <- nrow(sdata0)-1  #�H��
#�˵��D�ƥH�ΤH��
print(pnum)
print(snum)
#�p���A�Q�β�2�檺���רӭp��A�æs�������sdata1<- pnumxsnum
sdata1 <- matrix(0, nrow=snum, ncol=pnum)
i <- 1
while (i <= pnum){
  j <- 1
  while (j <= snum) {
    if (sdata0[1,i+1] == sdata0[j+1,i+1]) sdata1[j,i] <- 1
    j <- j+1
  }
  i <- i+1
}
#sdata1�Y�p�����G
sdata1 <- as.data.frame(as.matrix(sdata1))
colnames(sdata1)<-names(sdata0[,2:(pnum+1)])
#�˵��p�����G
head(sdata1)
#�Q��CTT��packages�Ӷi�滤���O���R
library(CTT)
#�������
data0 <- sdata0[-1,-1]
#���T����
data1 <- sdata0[1,-1]
#�����O���R
result0 <- distractor.analysis(data0,data1)
#��ܻ����O���R���G
head(result0,2)
#�i����D���y�z�ʲέp�A���O�O�����ơB�зǮt�B���A�P�p�ת��p��
#�w�qmy_stats��ơA�����ơB�зǮt�B���A�P�p��
library(moments)
my_stats <- function(x) {
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}
#�p�⥭���ơB�зǮt�B���A�H�ήp��
sdata1_desc <- apply(sdata1, 2, my_stats)
#��ܭp�⵲�G�A�Ĥ@�C�O�����ơB�ĤG�C�O�зǮt�B�ĤT�C�O���A�B�ĥ|�C�O�p��
sdata1_desc
#����K�ѧO�A�[�W�����ѧO
rownames(sdata1_desc) <- c("���׭�", "�зǮt", "���A", "�p��")
#�N��Ƥ��H��m���x�s
result1 <- as.data.frame(t(sdata1_desc))
row.names(result1) <- names(sdata0[,2:(pnum+1)])
head(round(result1,3))

#�p��������סBŲ�O�ץH��CR��
sdata1$tot <- apply(sdata1, 1, sum)
head(sdata1)
sdata1$grp <- NA
LB=0
HB=0
LB=quantile(sdata1$tot,probs=c(0.27))
HB=quantile(sdata1$tot,probs=c(0.73))
sdata1$grp[sdata1$tot <= LB] <- "L"
sdata1$grp[sdata1$tot >= HB] <- "H"
head(sdata1)
sdata1$grp <- factor(sdata1$grp)
sdata2 <- aggregate(sdata1[,1:pnum], by=list(sdata1$grp), mean)
sdata2 <- t(sdata2[,-1])
item_t <- sapply(sdata1[,1:pnum], function(x) c(t.test(x ~ sdata1$grp)$statistic,t.test(x ~ sdata1$grp)$p.value))
result2 <- data.frame(Item=rownames(sdata2),m.l=sdata2[,2], m.h=sdata2[,1], m.a=(sdata2[,2]+sdata2[,1])/2, m.dif=sdata2[,1]-sdata2[,2], t.stat=item_t[1,], t.p=item_t[2,])
head(result2)
#�N�D�ت��s���h��
result2 <- result2[,-1]
#����R�W���
names(result2) <- c('�C��������','����������','��������', 'Ų�O��','CR��','p��')
row.names(result2) <- names(sdata0[,2:(pnum+1)])
#���T��p��
head(round(result2,3))

#�Q��psychometrics�M��A�p���D�ػP�`������
library(psychometric)
itotr <- item.exam(sdata1[, 1:pnum], discrim = TRUE)
head(itotr)
#���X���R���G
result3 <- t(rbind(itotr$Item.Tot.woi))
result3 <- data.frame(result3)
names(result3) <- c('�D���`������')
row.names(result3) <- names(sdata0[,2:(pnum+1)])
head(round(result3,3))

#�p��R�D��H��
#�o�O�D�ثH��
detach("package:psychometric", unload = TRUE)

#�Q��psych�M��A�p��R�D��H��
library(psych)
itotalpha <- alpha(sdata1[, 1:pnum],check.keys=TRUE)$alpha.drop[,'raw_alpha']
result4 <- as.data.frame(t(rbind(itotalpha, itotr$Item.Rel.woi)))
names(result4) <- c('�`�q���H��(�R�D)','�D�ثH��')
row.names(result4) <- names(sdata0[,2:(pnum+1)])
head(round(result4, 3))

#�X�֧R�D��H��
resultall <- cbind(result2, result3, result4)
head(round(resultall, 3))

#�N���G�s���ɮ�
write.csv(round(resultall,3), "���D���R���G.csv")