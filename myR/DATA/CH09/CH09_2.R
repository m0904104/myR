#試題反應理論之分析 2017/08/14
#檔名CH09_2.R 
#p.375
ptheta <- seq(-3,3,1)
pb <- 0.00
pa <- 0.80
L <- pa*(ptheta-pb)
P <- 1/(1+exp(-L))
print(ptheta)
print(L)
print(exp(-L))
print(1+exp(-L))
print(P)
pdata <- data.frame(ptheta, L, exp(-L), 1+exp(-L), P)
print(pdata)
#p.377
ptheta <- seq(-3,3,0.1)
pb <- 0.00
pa <- 0.80
P <- 1/(1+exp(-pa*(ptheta-pb)))
plot(ptheta, P, type="l",xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率")
thetai <- pb
pthetai <- 1/(1+exp(-pa*(thetai-pb)))
vliney <- seq(0, pthetai, 0.01)
vlinex <- pb+vliney*0
lines(vlinex, vliney, lty=2)
#p.379
theta <- seq(-3,3,1)
b <- 0.00
a <- 1.00
L <- a*(theta-b)
P <- 1/(1+exp(-L))
print(theta)
print(L)
print(exp(-L))
print(1+exp(-L))
print(P)
pdata <- data.frame(theta, L, exp(-L), 1+exp(-L), P)
print(pdata)
#p.382
ptheta <- seq(-3,3,0.1)
pb <- 0.00
pa <- 1.00
P <- 1/(1+exp(-pa*(ptheta-pb)))
plot(ptheta, P, type="l",xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率")
thetai <- pb
pthetai <- 1/(1+exp(-a*(thetai-pb)))
vliney <- seq(0, pthetai, 0.01)
vlinex <- pb+vliney*0
lines(vlinex, vliney, lty=2)
#p.384
ptheta <- seq(-3,3,1)
pb <- 0.00
pa <- 0.80
pc <- 0.02
L <- pa*(ptheta-pb)
P <- pc+(1-pc)*(1/(1+exp(-L)))
print(ptheta)
print(L)
print(exp(-L))
print(1+exp(-L))
print(P)
pdata <- data.frame(ptheta, L, exp(-L), 1+exp(-L), P)
print(pdata)
#p.386
ptheta <- seq(-3,3,0.1)
pb <- 0.00
pa <- 0.80
pc <- 0.02
P <- pc+(1-pc)*(1/(1+exp(-pa*(ptheta-pb))))
plot(ptheta, P, type="l",xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="機率")
thetai <- pb
pthetai <- pc+(1-pc)*(1/(1+exp(-pa*(thetai-pb))))
vliney <- seq(0, pthetai, 0.01)
vlinex <- pb+vliney*0
lines(vlinex, vliney, lty=2)
#p.387
ptheta <- seq(-3,3,0.1)
pb <- 0.00
pa <- 0.80
pc <- 0.02
P <- pc+(1-pc)*(1/(1+exp(-pa*(ptheta-pb))))
plot(ptheta, (1-P), type="l",xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="機率")
thetai <- pb
pthetai <- 1-(pc+(1-pc)*(1/(1+exp(-pa*(thetai-pb)))))
vliney <- seq(0, pthetai, 0.01)
vlinex <- pb+vliney*0
lines(vlinex, vliney, lty=2)
#p.389
#計算試題特徵曲線的函數
icccal <- function (pa, pb, pc){
  if (missing(pc)) pc<-0
  if (missing(pa)) pa<-1
  ptheta <- seq(-3,3,1)
  L <- pa*(ptheta-pb)
  exp1 <- exp(-L)
  exp2 <- 1+exp1
  P <- pc+(1-pc)/exp2
  data.frame(ptheta, L, exp1, exp2, P)
}
#icc函數
icc <- function(pa, pb, pc){
  if (missing(pc)) pc<-0
  if (missing(pa)) pa<-1
  ptheta <- seq(-3,3,0.1)
  P <- pc+(1-pc)/(1+exp(-pa*(ptheta-pb)))
  plot(ptheta, P, type="l",xlim=c(-3,3), ylim=c(0,1),
       xlab="能力值", ylab="答對機率")
  thetai <- pb
  pthetai <- pc+(1-pc)/(1+exp(-pa*(thetai-pb)))
  vliney <- seq(0, pthetai, 0.01)
  vlinex <- pb+vliney*0
  lines(vlinex, vliney, lty=2)  
}
#進行計算及繪製圖形
icccal(pa=1.0,pb=0.0)
icc(pa=1.0, pb=0.0)