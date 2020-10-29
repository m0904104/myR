#多元計分IRT分析
#試題反應理論之分析 2017/08/14
#檔名CH11_3.R
library(ltm)
library(psych)

data(bfi)
head(bfi)

IRTgrm.items <- bfi[,1:25]

IRTgrm <- grm(IRTgrm.items, constrained = TRUE)
print(IRTgrm)

IRTgrm.agree <- grm(IRTgrm.items[,1:5], constrained = TRUE)
IRTgrm.consc <- grm(IRTgrm.items[,6:10], constrained = TRUE)
IRTgrm.extra <- grm(IRTgrm.items[,11:15], constrained = TRUE)
IRTgrm.neuro <- grm(IRTgrm.items[,16:20], constrained = TRUE)
IRTgrm.open <- grm(IRTgrm.items[,21:25], constrained = TRUE)
IRTgrm.agree
IRTgrm.consc
IRTgrm.extra
IRTgrm.neuro
IRTgrm.open

IRTgrm2.agree <- grm(IRTgrm.items[,1:5], constrained = FALSE)
IRTgrm2.consc <- grm(IRTgrm.items[,6:10], constrained = FALSE)
IRTgrm2.extra <- grm(IRTgrm.items[,11:15], constrained = FALSE)
IRTgrm2.neuro <- grm(IRTgrm.items[,16:20], constrained = FALSE)
IRTgrm2.open <- grm(IRTgrm.items[,21:25], constrained = FALSE)
IRTgrm2.agree
IRTgrm2.consc
IRTgrm2.extra
IRTgrm2.neuro
IRTgrm2.open
#模式比較
anova(IRTgrm.agree,IRTgrm2.agree)
anova(IRTgrm.consc,IRTgrm2.consc)
anova(IRTgrm.extra,IRTgrm2.extra)
anova(IRTgrm.neuro,IRTgrm2.neuro)
anova(IRTgrm.open,IRTgrm2.open)
#繪製圖形試題特徵曲線
plot(IRTgrm2.agree, lwd = 2, cex = 0.8,
     legend = TRUE, cx = "topright",
     xlab = "Agreeableness", cex.main = 1,
     cex.lab = 1, cex.axis = 1)

#繪製圖形試題訊息曲線
plot(IRTgrm2.agree, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "Agreeableness",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.consc, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "Conscientiousness",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.extra, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "Extraversion",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.neuro, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "Neuroticism",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.open, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "Openness to Experience",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
#繪製圖形測驗訊息函數
plot(IRTgrm2.agree, type = "IIC", items = 0,
     lwd = 2, xlab = "Agreeableness",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.consc, type = "IIC", items = 0,
     lwd = 2, xlab = "Conscientiousness",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.extra, type = "IIC", items = 0,
     lwd = 2, xlab = "Extraversion",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.neuro, type = "IIC", items = 0,
     lwd = 2, xlab = "Neuroticism",
     cex.main = 1, cex.lab = 1, cex.axis = 1)
plot(IRTgrm2.open, type = "IIC", items = 0,
     lwd = 2, xlab = "Openness to Experience",
     cex.main = 1, cex.lab = 1, cex.axis = 1)