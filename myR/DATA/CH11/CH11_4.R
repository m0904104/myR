#�h���p��IRT���R�Q��mirt�M��
#���D�����z�פ����R 2017/08/14
#�ɦWCH011_4.R
library(mirt)
library(psych)
#�}�ҩһݸ����
data(bfi)
head(bfi)
IRTgrm.items <- bfi[,1:25]
IRTgrm3=mirt(data=IRTgrm.items, model=1, itemtype="graded")
summary(IRTgrm3)
coef(IRTgrm3, IRTpars = T)
#ø�s���D�S�x���u
itemplot(IRTgrm3,1,type="trace")
#ø�s���D�T�����u
itemplot(IRTgrm3,5,type="info")
#ø�s�Ҧ����D���S�x���u
plot(IRTgrm3, type="trace")
#ø�s����T�����u
plot(IRTgrm3, type="info")