getwd()
setwd("C:/Users/huhen/Dropbox/EstimatingChina/analysis")

load("result_ame.rda")
library(amen)

s.xi <- subset(angle1, Var1 == "Xi Jinping")
s.xi <- s.xi[,-1]; colnames(s.xi) <- c("IND", "Xi")
s.li <- subset(angle1, Var1 == "Li Keqiang")
s.li <- s.li[,-1]; colnames(s.li) <- c("IND", "Li")
s.zd <- subset(angle1, Var1 == "Zhang Dejiang")
s.zd <- s.zd[,-1]; colnames(s.zd) <- c("IND", "Zh.D")
s.yu <- subset(angle1, Var1 == "Yu Zhengsheng")
s.yu <- s.yu[,-1]; colnames(s.yu) <- c("IND", "Yu")
s.ly <- subset(angle1, Var1 == "Liu Yunshan")
s.ly <- s.ly[,-1]; colnames(s.ly) <- c("IND", "Liu")
s.wq <- subset(angle1, Var1 == "Wang Qishan")
s.wq <- s.wq[,-1]; colnames(s.wq) <- c("IND", "Wang")
s.zg <- subset(angle1, Var1 == "Zhang Gaoli")
s.zg <- s.zg[,-1]; colnames(s.zg) <- c("IND", "Zhang.G")

ame.s <- merge(s.xi, s.li, by = "IND", all = T)
ame.s <- merge(ame.s, s.zd, by = "IND", all = T)
ame.s <- merge(ame.s, s.yu, by = "IND", all = T)
ame.s <- merge(ame.s, s.ly, by = "IND", all = T)
ame.s <- merge(ame.s, s.wq, by = "IND", all = T)
ame.s <- merge(ame.s, s.zg, by = "IND", all = T)

n.xi <- subset(angle2, Var1 == "Xi Jinping")
n.xi <- n.xi[,-1]; colnames(n.xi) <- c("IND", "Xi")
n.li <- subset(angle2, Var1 == "Li Keqiang")
n.li <- n.li[,-1]; colnames(n.li) <- c("IND", "Li")
n.zd <- subset(angle2, Var1 == "Zhang Dejiang")
n.zd <- n.zd[,-1]; colnames(n.zd) <- c("IND", "Zh.D")
n.yu <- subset(angle2, Var1 == "Yu Zhengsheng")
n.yu <- n.yu[,-1]; colnames(n.yu) <- c("IND", "Yu")
n.ly <- subset(angle2, Var1 == "Liu Yunshan")
n.ly <- n.ly[,-1]; colnames(n.ly) <- c("IND", "Liu")
n.wq <- subset(angle2, Var1 == "Wang Qishan")
n.wq <- n.wq[,-1]; colnames(n.wq) <- c("IND", "Wang")
n.zg <- subset(angle2, Var1 == "Zhang Gaoli")
n.zg <- n.zg[,-1]; colnames(n.zg) <- c("IND", "Zhang.G")

ame.df <- merge(ame.s, n.xi, by = "IND", all = T)
ame.df <- merge(ame.df, n.li, by = "IND", all = T)
ame.df <- merge(ame.df, n.zd, by = "IND", all = T)
ame.df <- merge(ame.df, n.yu, by = "IND", all = T)
ame.df <- merge(ame.df, n.ly, by = "IND", all = T)
ame.df <- merge(ame.df, n.wq, by = "IND", all = T)
ame.df <- merge(ame.df, n.zg, by = "IND", all = T)

lsg.df <- read.csv("DV_LSG.csv", header = T)
lsg.df <- lsg.df[,-(5:6)]

test.df <- merge(lsg.df, ame.df, by = "IND", all = T)

test.df$total[is.na(test.df$total)] <- 0
test.df$central[is.na(test.df$central)] <- 0
test.df$state[is.na(test.df$state)] <- 0

save(test.df, fit1, fit2, file = "ChinaVitae_Test.rda")
rm(list = ls())

#--------------------------------------------------------------------------------------------------
load("ChinaVitae_Test.rda")
ls()

names(test.df)

library(Zelig); library(ZeligChoice)

m0 <- zelig(cbind(cn.b, st.b) ~ node.y, model = "bprobit", data = test.df)
m1 <- zelig(cbind(cn.b, st.b) ~ xi.c.y, model = "bprobit", data = test.df)
m2 <- zelig(cbind(cn.b, st.b) ~ Xi.y, model = "bprobit", data = test.df)
m3 <- zelig(cbind(cn.b, st.b) ~ node.y + Xi.y, model = "bprobit", data = test.df)


