#���D�����z�פ����R 2017/08/14
#�ɦWCH09_1.R 
#p.367
ptheta <- seq(-3,3,0.1)
pb <- 0
pa <- 1
p <- 1/(1+exp(-pa*(ptheta-pb)))
plot(ptheta, p, type="l", xlim=c(-3,3),ylim=c(0,1),
     xlab="��O��", ylab="������v")
#p.370
iccplot <- function(pa,pb){
  ptheta <- seq(-3,3,0.1)
  p <- 1/(1+exp(-pa*(ptheta-pb)))
  plot(ptheta, p, type="l",xlim=c(-3,3),ylim=c(0,1),
       xlab="��O��",ylab="������v")
}
iccplot(1,0)
iccplot(pa=1,pb=0)

#p.371�ŧi�ܼ�
bveryeasy <- -2.7
beasy <- -1.5
bmedium <- 0
bhard <- 1.5
bveryhard <- 2.7
#p.372�ŧiŲ�O��
anone <- 0
alow <- 0.4
amoderate <- 1
ahigh <- 2.3
aperfect <- 999
#ø�s�ϧ�
iccplot(amoderate, bmedium)
par(new=T)
iccplot(alow, beasy)
par(new=T)
iccplot(amoderate, beasy)
par(new=T)
iccplot(amoderate, bmedium)
par(new=T)
iccplot(amoderate, bhard)