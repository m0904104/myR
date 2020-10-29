#���D�����z�פ����R 2017/08/14
#�ɦWCH10_1.R 
#p.415
pa=1.20
pb=0.50
ptheta=1.00
pc=0.00
P <- pc+(1-pc)/(1+exp(-pa*(ptheta-pb)))
print(P)
icc2 <- function(pa, pb, pc){
  if (missing(pc)) pc<-0
  if (missing(pa)) pa<-1
  ptheta <- seq(-3,3,0.1)
  P <- pc+(1-pc)/(1+exp(-pa*(ptheta-pb)))
  plot(ptheta, P, type="l",xlim=c(-3,3), ylim=c(0,1),
       xlab="��O��", ylab="������v")
  thetai <- 1
  pthetai <- pc+(1-pc)/(1+exp(-pa*(thetai-pb)))
  vliney <- seq(0, pthetai, 0.01)
  vlinex <- 1+vliney*0
  vlinex2 <- -3+vliney
  lines(vlinex, vliney, lty=2)
  vlinex2 <- seq(-3.00, thetai, 0.01)
  vlinex3 <- rep(pthetai, length(vlinex2))
  lines(vlinex2,vlinex3, lty=2)
}
icc2(pa=0.80, pb=-1.00)
icc2(pa=1.00, pb= 0.00)
icc2(pa=0.60, pb= 0.25)
icc2(pa=1.20, pb= 0.50)

#p.417
pb <- c(-1.00, 0.00, 0.25, 0.50)
pa <- c( 0.80, 1.20, 0.80, 0.75)
ptheta <- seq(-3.00, 3.00, 0.10)
ts <- rep(0, length(ptheta))
J <- length(pb)
for (j in 1:J){
  P <- 1/(1+exp(-pa[j]*(ptheta-pb[j])))
  ts <- ts + P
}
#ø�s����S�x���u��
plot(ptheta, ts, type="l", xlim=c(-3.00,3.00), ylim=c(0.00,J),
     xlab="��O��", ylab="�������")

#p.419
tcc <- function(pa, pb, pc){
  J <- length(pb)
  if (missing(pc)) pc<- rep(0, J)
  if (missing(pa)) pa<- rep(1, J)
  ptheta <- seq(-3.00, 3.00, 0.10)
  ts <- rep(0, length(ptheta))
  for (j in 1:J){
    P <- pc[j]+(1-pc[j]) / (1+exp(-pa[j]*(ptheta-pb[j])))
    ts <- ts + P
  }
  plot(ptheta, ts, type="l", xlim=c(-3.00,3.00), ylim=c(0.00,J),
       xlab="��O��", ylab="�������",
       main="����S�x���u")
}
#���մ���S�x���u���
b <- c(-1.50, -1.00, 0.00, 1.00, 1.50)
tcc(pb=b)

b <- c(-1.50, -1.00, 0.00, 1.00, 1.50)
a <- c( 0.25,  0.50, 1.00, 0.50, 0.25)
tcc(pa=a, pb=b)

b <- c(-1.50, -1.00, 0.00, 1.00, 1.50)
a <- c( 0.25,  0.50, 1.00, 0.50, 0.25)
c <- c( 0.02,  0.02, 0.02, 0.02, 0.02)
tcc(pa=a, pb=b, pc=c)