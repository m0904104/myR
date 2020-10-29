#試題反應理論之分析 2017/08/14
#檔名CH10_2.R 
#p.425
#定義答題組型, 1:答對, 0:答錯
u <- c(1,0,1)
#所答試題的題目參數2PL
b <- c(-1.00, 0.00, 1.00)
a <- c( 1.00, 1.20, 0.80)
#能力初始值
th <- 1.00
J <- length(b)
S <- 10

ccrit <- 0.001

for (s in 1:S){
  sumnum <- 0.00
  sumdem <- 0.00
  for (j in 1:J){
    phat <- 1/(1+exp(-a[j]*(th-b[j])))
    sumnum <- sumnum+a[j]*(u[j]-phat)
    sumdem <- sumdem-a[j]**2*phat*(1.0-phat)
  }
  delta <- sumnum /sumdem
  th <- th-delta
  cat(paste("th=",th,"\n"));flush.console()
  if (abs(delta)< ccrit | s ==S){
    se <- 1/sqrt(-sumdem)
    cat(paste("se=", se, "\n"));flush.console()
    break
  }
}
th
se

#p.428
#能力估計函數
#md1:參數別  1:rasch 2:2pl 3:3pl
#u:反應組別
#b,a,c:答題的題目參數
#u,b,a,c:四個參數的長度必需要一致
ability <- function (md1, u, pa, pb, pc){
  J <- length(pb)
  if (md1 == 1 | md1 == 2 | missing(pc)){
    pc <- rep(0, J)
  }
  if (md1 == 1 | missing(pa)){
    pa <- rep(1, J)
  }
  
  x <- sum(u)
  if (x == 0){
    th <- -log(2*J)
  }
  if (x == J){
    th <- log(2*J)
  }
  if (x == 0 | x == J){
    sumdem <- 0.00
    for (j in 1:J){
      pstar <- 1/(1+exp(-pa[j]*(thp-b[j])))
      phat <- pc[j]+(1.00-pc[j])*pstar
      sumdem <- sumdem - a[j]^2 * phat * (1.00-phat) * (pstar/phat)^2
    }
    se <- 1/sqrt(-sumdem)
  }
  
  if (x != 0 & x != J){
    th <- 1.00
    S <- 10
    ccrit <- 0.001
    for (s in 1:S){
      sumnum <- 0.00
      sumdem <- 0.00
      for (j in 1:J){
        pstar <- 1/ (1+exp(-pa[j]*(th-pb[j])))
        phat <- pc[j]+(1.00-pc[j])*pstar
        sumnum <- sumnum +pa[j]*(u[j]-phat)*(pstar/phat)
        sumdem <- sumdem -pa[j]^2 *phat * (1.00-phat)*(pstar/phat)^2
      }
      delta <- sumnum /sumdem
      th <- th-delta
      cat(paste("th=",th, "\n"));flush.console()
      if (abs(delta)<ccrit | s ==S){
        se <- 1/sqrt(-sumdem)
        cat(paste("se=",se, "\n"));flush.console()        
        break
      }
    }
  }
  
  thse <- c(th, se)
  return(thse)
}
#測試能力估計函數是否正確
md1 <- 2
u <- c(    1,    0,    1)
b <- c(-1.00, 0.00, 1.00)
a <- c( 1.00, 1.20, 0.80)
ability(md1, u, pa=a, pb=b)