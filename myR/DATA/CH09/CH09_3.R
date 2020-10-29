#試題反應理論之分析 2017/08/14
#檔名CH09_3.R 
#p.391
#-3 to 3 0.1 總共有38個點
ptheta <- seq(-3,3,0.16)
f <- rep(20, length(ptheta))
pb <- round(runif(1,-3,3),2)
pa <- round(runif(1,0.2,2.8),2)
pc <- round(runif(1,0,0.35),2)
md1 <- 2
if (md1 == 1 | md1 == 2) {pc <- 0}
if (md1 ==1) {pa <- 1}
for (g in 1:length(ptheta)){
  P <- pc + (1-pc)/(1+exp(-pa*(ptheta-pb)))
}
p <- rbinom(length(ptheta),f,P)/f
plot(ptheta, p, xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率")
cs <- 0
for (g in 1:length(ptheta)){
  v <- f[g]*(p[g]-P[g])^2/(P[g]-P[g]^2)
  cs <- cs+v
}
cs <- round(cs,2)
if (md1 == 1){
  maintext <- paste("Chi-Square=",cs,"\n","b=", pb)
}
if (md1 == 2){
  maintext <- paste("Chi-Square=",cs,"\n","a=", pa,"b=", pb)
}
if (md1 == 3){
  maintext <- paste("Chi-Square=",cs,"\n","a=", pa,"b=", pb,"c=", pc)
}
par(new=T)
plot(ptheta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
     xlab="", ylab="", main=maintext)
#查卡方臨界值
qchisq(0.95, df=36)

#p.395 
t1l <- -3
t1u <- -1
lowerg1 <- 0
for (g in 1:length(ptheta)){
  if (ptheta[g] <= t1l){lowerg1 <- lowerg1+1}
}
upperg1 <- 0
for (g in 1:length(ptheta)){
  if (ptheta[g] <= t1u) {upperg1 <- upperg1+1}
}
ptheta1 <- ptheta[lowerg1:upperg1]
p1 <- p[lowerg1:upperg1]
if (md1 == 1) { maintext <- paste("Group 1", "\n")}
if (md1 == 2) { maintext <- paste("Group 1", "\n")}
if (md1 == 3) { maintext <- paste("Group 1", "\n")}
plot(ptheta1, p1, xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率",
     main=maintext)
P1 <- P[lowerg1:upperg1]
if (md1 == 1){
  maintext <- paste("\n", "b=", pb)
}
if (md1 == 2){
  maintext <- paste("\n", "a=", pa, "b=", pb)
}
if (md1 == 3){
  maintext <- paste("\n", "a=", pa, "b=", pb, "c=", pc)
}
par(new=T)
plot(ptheta1, P1, xlim=c(-3,3), ylim=c(0,1), type="l",
     xlab="", ylab="", main=maintext)

t2l <- 1
t2u <- 3
lowerg2 <- 0
for (g in 1:length(ptheta)){
  if (ptheta[g] <= t2l){ lowerg2 <- lowerg2+1 }
}
upperg2 <- 0
for (g in 1:length(ptheta)){
  if (ptheta[g] <= t2u) { upperg2 <- upperg2+1 }
}
ptheta2 <- ptheta[lowerg2:upperg2]
p2 <- p[lowerg2:upperg2]
if (md1 == 1) {maintext <- paste("Group2","\n")}
if (md1 == 2) {maintext <- paste("Group2","\n")}
if (md1 == 3) {maintext <- paste("Group2","\n")}
plot(ptheta2, p2, xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率",
     main=maintext)
P2 <- P[lowerg2:upperg2]
if (md1 == 1){
  maintext <- paste("\n", "b=", pb)
}
if (md1 == 2){
  maintext <- paste("\n", "a=", pa, "b=", pb)
}
if (md1 == 3){
  maintext <- paste("\n", "a=", pa, "b=", pb, "c=", pc)
}
par(new=T)
plot(ptheta2, P2, xlim=c(-3,3), ylim=c(0,1), type="l",
     xlab="", ylab="", main=maintext)

ptheta12 <- c(ptheta1, ptheta2)
p12 <- c(p1, p2)
if (md1 == 1){ maintext <- paste("Groups1&2","\n")}
if (md1 == 2){ maintext <- paste("Groups1&2","\n")}
if (md1 == 3){ maintext <- paste("Groups1&2","\n")}
plot(ptheta12, p12, xlim=c(-3,3), ylim=c(0,1),
     xlab="能力值", ylab="答對機率",
     main=maintext)
if (md1 == 1){
  maintext <- paste("\n", "b=", pb)
}
if (md1 == 2){
  maintext <- paste("\n", "a=", pa, "b=", pb)
}
if (md1 == 3){
  maintext <- paste("\n", "a=", pa, "b=", pb, "c=", pc)
}
par(new=T)
plot(ptheta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
     xlab="", ylab="", main=maintext)
#p.402
#iccfit 函數
iccfit <- function(md1){
  theta <- seq(-3, 3, 0.16)
  f <- rep(20, length(theta))
  pb <- round(runif(1, -3, 3),2)
  pa <- round(runif(1, 0.2, 2.8),2)
  pc <- round(runif(1, 0, 0.35),2)
  if (md1 == 1 | md1 == 2) { pc <- 0}
  if (md1 == 1){ pa <- 1}
  for (g in 1:length(ptheta)){
    P <- pc + (1 - pc) / (1+exp(-pa*(ptheta-pb)))
  }
  p <- rbinom(length(ptheta), f, P)/f
  par(lab=c(7,5,3))
  plot(ptheta, p, xlim=c(-3,3), ylim=c(0,1),
       xlab="能力值", ylab="答對機率")
  cs <- 0
  for (g in 1:length(ptheta)){
    v <- f[g]*(p[g]-P[g])^2/(P[g]-P[g]^2)
    cs <- cs + v
  }
  cs <- round(cs, 2)
  if (md1 == 1){
    maintext <- paste("Chi-Square=",cs,"\n","b=",pb)
  }
  if (md1 == 2){
    maintext <- paste("Chi-Square=",cs,"\n","a=",pa,"b=",pb)
  }
  if (md1 == 3){
    maintext <- paste("Chi-Square=",cs,"\n","a=",pa,"b=",pb,"c=",pc)
  }
  par(new=T)
  plot(ptheta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
       xlab="", ylab="", main=maintext)
  
}

iccfit(1)
iccfit(2)
iccfit(3)

#p.405
iccfit2 <- function(pa, pb, pc){
  theta <- seq(-3, 3, 0.16)
  f <- rep(20, length(theta))
  if (missing(pa)) pa <- 1
  if (missing(pc)) pc <- 0
  for (g in 1:length(ptheta)){
    P <- pc + (1 - pc) / (1+exp(-pa*(ptheta-pb)))
  }
  p <- rbinom(length(ptheta), f, P)/f
  par(lab=c(7,5,3))
  plot(ptheta, p, xlim=c(-3,3), ylim=c(0,1),
       xlab="能力值", ylab="答對機率")
  cs <- 0
  for (g in 1:length(ptheta)){
    v <- f[g]*(p[g]-P[g])^2/(P[g]-P[g]^2)
    cs <- cs + v
  }
  cs <- round(cs, 2)
  maintext <- paste("Chi-Square=",cs,"\n","a=",pa,"b=",pb,"c=",pc)
  par(new=T)
  plot(ptheta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
       xlab="", ylab="", main=maintext)
}

iccfit2(pb=2.76)
iccfit2(pa=1.53,pb=-2.56)
iccfit2(pa=1.65,pb=0.17,pc=0.32)

#p.408
#groupinv 函數
groupinv <- function (md1, t1l, t1u, t2l, t2u){
  if (missing(t1l)) t1l <- -3
  if (missing(t1u)) t1u <- -1
  if (missing(t2l)) t2l <- 1
  if (missing(t2u)) t2u <- 3
  ptheta <- seq(-3, 3, 0.16)
  f <- rep(20, length(ptheta))
  pb <- round(runif(1, -3, 3), 2)
  pa <- round(runif(1, 0.2, 2.8), 2)
  pc <- round(runif(1, 0, 0.35), 2)
  if (md1 == 1 | md1==2) { pc <- 0}
  if (md1 ==1) {pa <- 1}
  for (g in 1:length(ptheta)){
    P <- pc+(1-pc)/(1+exp(-pa*(ptheta-pb)))
  }
  p <- rbinom (length(ptheta), f, P)/f
  lowerg1 <- 0
  for (g in 1:length(ptheta)){
    if (ptheta[g] <= t1l){lowerg1 <- lowerg1+1 }
  }
  upperg1 <- 0
  for (g in 1:length(ptheta)){
    if (ptheta[g] <= t1u){upperg1 <- upperg1+1}
  }
  ptheta1 <- ptheta[lowerg1:upperg1]
  p1 <- p[lowerg1:upperg1]
  lowerg2 <- 0
  for (g in 1:length(ptheta)){
    if (ptheta[g] <= t2l){lowerg2 <- lowerg2+1}
  }
  upperg2 <- 0
  for (g in 1:length(ptheta)){
    if (ptheta[g] >= t2u){upperg2 <- upperg2+1}
  }
  ptheta2 <- ptheta[lowerg2:upperg2]
  p2 <- p[lowerg2:upperg2]
  
  ptheta12 <- c(ptheta1, ptheta2)
  p12 <- c(p1,p2)
  par(lab=c(7,5,3))
  plot(ptheta12, p12, xlim=c(-3,3), ylim=c(0,1),
       xlab="能力值", ylab="答對機率")
  if (md1 == 1){
    maintext <- paste("Groups1&2", "\n", "b=", pb)
  }
  if (md1 == 2){
    maintext <- paste("Groups1&2", "\n", "a=", pa, "b=", pb)
  }
  if (md1 == 3){
    maintext <- paste("Groups1&2", "\n", "a=", pa, "b=", pb, "c=", pc)
  }
  #繪圖
  par(new=T)
  plot(ptheta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
       xlab="", ylab="", main=maintext)
}

groupinv(1)
groupinv(1, -3, -1, 1, 3)
groupinv(2)
groupinv(3)

