#�q�������D���R 2017/07/30 �����q��
#�ɦWCH02_3.R �����CH02_2.csv
#�]�w�u�@�ؿ�
setwd("D:/DATA/CH02/")
#Ū�������
library(readr)
sdata0 <- read_csv("CH02_2.csv")
#�˵��e�᤻�����
head(sdata0)
tail(sdata0)
#�p���D�ƻP�H��
pnum <- ncol(sdata0)-1  #�D��
snum <- nrow(sdata0)  #�H��
#�˵��D�ƥH�ΤH��
print(pnum)
print(snum)
#�p���A�Q�β�2�檺���רӭp��A�æs�������sdata1<- pnumxsnum
sdata1 <- sdata0[,-1]

#�w�qmy_stats��ơA�����ơB�зǮt�B���A�P�p��
library(moments)
my_stats <- function(x) {
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}

#�p�⥭���ơB�зǮt�B���A�H�ήp��
sdata1_desc <- apply(sdata1, 2, my_stats)
rownames(sdata1_desc) <- c("������", "�зǮt", "���A", "�p��")
result1 <- as.data.frame(t(sdata1_desc))
row.names(result1) <- names(sdata0[,2:(pnum+1)])
round(result1,3)

#�p��������סBŲ�O�ץH��CR��
sdata1$tot <- apply(sdata1, 1, sum)
sdata1$grp <- NA
LB=0
HB=0
LB=quantile(sdata1$tot,probs=c(0.27))
HB=quantile(sdata1$tot,probs=c(0.73))
sdata1$grp[sdata1$tot <= LB] <- "L"
sdata1$grp[sdata1$tot >= HB] <- "H"
sdata1$grp <- factor(sdata1$grp)
sdata2 <- aggregate(sdata1[,1:pnum], by=list(sdata1$grp), mean)
sdata2 <- t(sdata2[,-1])
item_t <- sapply(sdata1[,1:pnum], function(x) c(t.test(x ~ sdata1$grp, var.equal=TRUE)$statistic,t.test(x ~ sdata1$grp, var.equal=TRUE)$p.value))
result2 <- data.frame(Item=rownames(sdata2),m.l=sdata2[,2], m.h=sdata2[,1], m.a=(sdata2[,2]+sdata2[,1])/2, m.dif=sdata2[,1]-sdata2[,2], t.stat=item_t[1,], t.p=item_t[2,])
result2 <- result2[,-1]
names(result2) <- c('�C���ե���','�����ե���','�t��', 'Ų�O��','CR��','p��')
row.names(result2) <- names(sdata0[,2:(pnum+1)])
head(round(result2,3))

#�Q��psychometrics�M��A�p���D�ػP�`������
library(psychometric)
itotr <- item.exam(sdata1[, 1:pnum], discrim = TRUE)
head(itotr)
#�H�U�O�ݭn�p����q���������ɤ~�ݭn(4���q��)
#B01 6 1-6
#B02 6 7-12
#B03 6 13-18
#B04 6 19-24
#�N����ഫ���]�t4�Ӹ�Ʈج[���C
ldta <- list(x1=sdata1[,1:6],x2=sdata1[,7:12],x3=sdata1[,13:18],x4=sdata1[,19:24])
#�D�ػP���q���`������
isubr <- lapply(ldta, item.exam, discrim = TRUE)
#�˵���1�Ӥ��q
head(isubr,1)
#���X���R���G
result3 <- t(rbind(itotr$Item.Tot.woi,
c(isubr$x1$Item.Tot.woi,isubr$x2$Item.Tot.woi,isubr$x3$Item.Tot.woi,
  isubr$x4$Item.Tot.woi)))
result3 <- data.frame(result3)
names(result3) <- c('�D���`������','�D�ؤ��q������')
row.names(result3) <- names(sdata0[,2:(pnum+1)])
head(round(result3,3))

#�p��R�D��H��
isubrel <- c(isubr$x1$Item.Rel.woi,isubr$x2$Item.Rel.woi,isubr$x3$Item.Rel.woi,
             isubr$x4$Item.Rel.woi)
detach("package:psychometric", unload = TRUE)

#�Q��psych�M��A�p��R�D��H��
library(psych)
itotalpha <- alpha(sdata1[, 1:pnum],check.keys=TRUE)$alpha.drop[,'raw_alpha']
#�p��R�D����q��
isubalpha <- lapply(ldta, alpha)
ialpha <- c(isubalpha$x1$alpha.drop[,'raw_alpha'],
            isubalpha$x2$alpha.drop[,'raw_alpha'],
            isubalpha$x3$alpha.drop[,'raw_alpha'],
            isubalpha$x4$alpha.drop[,'raw_alpha']
            )
result4 <- as.data.frame(t(rbind(itotalpha, ialpha, itotr$Item.Rel.woi)))
names(result4) <- c('�`�q���H��(�R�D)','���q���H��(�R�D)','�D�ثH��')
row.names(result4) <- names(sdata0[,2:(pnum+1)])
head(round(result4, 3))

#�X�֧R�D��H��
resultall <- cbind(result2, result3, result4)
round(resultall, 3)

#�N���G�s���ɮ�
write.csv(round(resultall,3), "�h�����D���R���G.csv")