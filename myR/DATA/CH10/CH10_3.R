#試題反應理論之分析 2017/08/14
#檔名CH10_3.R 
#p.431
pb<- 0.00
pa<- 1.80
pc<- 0.00
ptheta <- seq(-3, 3, 0.1)
J <- length(pb)
ii <- matrix(rep(0, length(ptheta)*J), nrow=length(ptheta))
ti <- rep(0, length(ptheta))
for (j in 1:J){
  P <- 1/(1+exp(-pa[j]*(ptheta-pb[j])))
  ii[,j] <- pa[j]^2 *P *(1.0-P)
  ti <- ti+ii[,j]
}
plot(ptheta, ti ,xlim=c(-3,3),ylim=c(0,J),type="l",
     xlab="能力值", ylab="訊息量",
     main="試題訊息函數")
#p.432
pb <- c(-0.5, -0.3, -0.2, -0.2, 0.1, 0.0, 0.1, 0.2, 0.4, 0.2)
pa <- c( 1.2,  2.0,  1.4,  1.5, 1.2, 1.8, 1.8, 1.6, 0.8, 1.6)
theta <- seq(-3, 3, 0.1)
J <- length(pb)
ii <- matrix(rep(0, length(ptheta)*J), nrow=length(ptheta))
ti <- rep(0, length(ptheta))
for (j in 1:J){
  P <- 1/(1+exp(-pa[j]*(ptheta-pb[j])))
  ii[,j] <- pa[j]^2 *P *(1.0-P)
  ti <- ti+ii[,j]
}
plot(theta, ti ,xlim=c(-3,3),ylim=c(0,J),type="l",
     xlab="能力值", ylab="訊息量",
     main="測驗訊息函數")

#p.436
pb <- c(-2.0, -1.0, 0.0, 1.0, 2.0)
pa <- c( 0.8,  1.8, 2.0, 1.8, 0.6)
ptheta <- seq(-3, 3, 1)
J <- length(pb)
ii <- matrix(rep(0, length(ptheta)*J), nrow=length(ptheta))
ti <- rep(0, length(ptheta))
for (j in 1:J){
  P <- 1/(1+exp(-pa[j]*(ptheta-pb[j])))
  ii[,j] <- pa[j]^2 *P *(1.0-P)
  ti <- ti+ii[,j]
}
plot(ptheta, ti ,xlim=c(-3,3),ylim=c(0,J),type="l",
     xlab="能力值", ylab="訊息量",
     main="測驗訊息函數")
print(ptheta)
print(ii)
print(ti)
data.frame(ptheta, ii, ti)

#p.438
tif <- function(pa, pb, pc){
  J <- length(pb)
  if (missing(pc)) {pc<-rep(0,J)}
  if (missing(pa)) {pa<-rep(1,J)}
  
  ptheta <- seq(-3, 3, 0.1)
  ii <- matrix(rep(0, length(ptheta)*J), nrow=length(ptheta))
  ti <- rep(0, length(ptheta))
  
  for (j in 1:J){
    Pstar <- 1/(1+exp(-pa[j]*(ptheta-pb[j])))
    P <- pc[j]+(1-pc[j])*Pstar
    ii[,j] <- pa[j]^2*P*(1.0-P)*(Pstar/P)^2
    ti <- ti+ii[,j]
  }
  plot(ptheta, ti, xlim=c(-3,3),ylim=c(0, J), type="l",
       xlab="能力值", ylab="訊息量", 
       main="測驗訊息函數")
}

b <- c(-1.0, -0.5, 0.0, 0.5, 1.0)
a <- c( 2.0,  1.5, 1.5, 1.5, 2.0)
tif(pa=a,pb=b)