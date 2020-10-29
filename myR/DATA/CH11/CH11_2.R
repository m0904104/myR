#試題反應理論之分析 2017/08/14
#檔名CH11_2.R 
library(difR)
data(verbal)
head(verbal)
attach(verbal)
descript(verbal)

verbal <- verbal[colnames(verbal)!="Anger"]

r <- difLord(verbal, group = "Gender", focal.name = 1, model = "1PL")
print(r)

r <- difLord(verbal, group = "Gender", focal.name = 1, model = "1PL",
             save.output = TRUE, output = c("LordResults","default"))

plot(r)
plot(r, plot = "itemCurve", item = 1)
plot(r, plot = "itemCurve", item = 6)
plot(r, save.plot = TRUE, save.options = c("plot", "default", "pdf"))