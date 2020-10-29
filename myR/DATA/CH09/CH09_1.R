#試題反應理論之分析 2017/08/14
#檔名CH09_1.R 
#p.367
ptheta <- seq(-3,3,0.1)
pb <- 0
pa <- 1
p <- 1/(1+exp(-pa*(ptheta-pb)))
plot(ptheta, p, type="l", xlim=c(-3,3),ylim=c(0,1),
     xlab="能力值", ylab="答對機率")
#p.370
iccplot <- function(pa,pb){
  ptheta <- seq(-3,3,0.1)
  p <- 1/(1+exp(-pa*(ptheta-pb)))
  plot(ptheta, p, type="l",xlim=c(-3,3),ylim=c(0,1),
       xlab="能力值",ylab="答對機率")
}
iccplot(1,0)
iccplot(pa=1,pb=0)

#p.371宣告變數
bveryeasy <- -2.7
beasy <- -1.5
bmedium <- 0
bhard <- 1.5
bveryhard <- 2.7
#p.372宣告鑑別度
anone <- 0
alow <- 0.4
amoderate <- 1
ahigh <- 2.3
aperfect <- 999
#繪製圖形
iccplot(amoderate, bmedium)
par(new=T)
iccplot(alow, beasy)
par(new=T)
iccplot(amoderate, beasy)
par(new=T)
iccplot(amoderate, bmedium)
par(new=T)
iccplot(amoderate, bhard)