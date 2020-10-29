#相關與迴歸 2017/08/04
#檔名CH06_1.R 資料檔CH06_1.csv
#設定工作目錄
setwd("D:/DATA/CH06/")
library(readr)
sdata0 <- read_csv("CH06_1.csv")
head(sdata0)
tail(sdata0)
str(sdata0)

sdata0$GENDER <- factor(sdata0$GENDER)
print(sdata0$GENDER)
sdata0$JOB <- factor(sdata0$JOB)
print(sdata0$JOB)
sdata0$JOB <- factor(sdata0$JOB, levels = c(1,2,3,4), labels = c("主任", "組長","科任","級任"))
print(head(sdata0$JOB))

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

sdata0$B01 <- apply(sdata0[23:30],1,sum)
sdata0$B02 <- apply(sdata0[31:36],1,sum)
sdata0$B03 <- apply(sdata0[37:40],1,sum)
sdata0$B04 <- apply(sdata0[41:43],1,sum)
sdata0$B00 <- apply(sdata0[23:43],1,sum)
sdata0$SB01 <- apply(sdata0[23:30],1,mean)
sdata0$SB02 <- apply(sdata0[31:36],1,mean)
sdata0$SB03 <- apply(sdata0[37:40],1,mean)
sdata0$SB04 <- apply(sdata0[41:43],1,mean)
sdata0$SB00 <- apply(sdata0[23:43],1,mean)

sdata0$C01 <- apply(sdata0[44:50],1,sum)
sdata0$C02 <- apply(sdata0[51:55],1,sum)
sdata0$C03 <- apply(sdata0[56:59],1,sum)
sdata0$C04 <- apply(sdata0[60:63],1,sum)
sdata0$C00 <- apply(sdata0[44:63],1,sum)
sdata0$SC01 <- apply(sdata0[44:50],1,mean)
sdata0$SC02 <- apply(sdata0[51:55],1,mean)
sdata0$SC03 <- apply(sdata0[56:59],1,mean)
sdata0$SC04 <- apply(sdata0[60:63],1,mean)
sdata0$SC00 <- apply(sdata0[44:63],1,mean)

head(sdata0)

round(cor(sdata0$A01,sdata0$A02),3)

sdata1A <- sdata0[,c('A01','A02','A03','A04','A00','C01','C02','C03','C04','C00')]
sdata1S <- sdata0[,c('SA01','SA02','SA03','SA04','SA00','SC01','SC02','SC03','SC04','SC00')]

round(cor(sdata1A),3)
round(cor(sdata1S),3)

cor.test(~sdata0$A01+sdata0$A02,data=sdata1A)

library(Hmisc)
mCORR <- rcorr(as.matrix(sdata1A),type="pearson")
print(mCORR)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
head(flattenCorrMatrix(mCORR$r, mCORR$P))