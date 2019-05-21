# devtools::install_github("s7minhas/amen", ref = "dev")
library(amen); source("functions.R")

load("cv_matrix.rda")
# cv.mx1          simple sum weight
# cv.mx2          Newman's weight

imp <- 10000; toBurn <- 5000

fit1 <- ame(cv.mx1, 
            symmetric = T, R = 2, model = "nrm", intercept = F,
            burn = toBurn, nscan = imp, odens = 1, rvar = F,
            cvar = F, seed = 1, print = F, plot = F)
# fit1c <- ame(cv.mx1, 
#              symmetric = T, R = 2, model = "nrm", intercept = F,
#              burn = toBurn, nscan = imp, odens = 1, rvar = T, cvar = T,
#              seed = 1, print = F, plot = F)

fit2 <- ame(cv.mx2, 
            symmetric = T, R = 2, model = "nrm", intercept = F,
            burn = toBurn, nscan = imp, odens = 1, rvar = F,
            cvar = F, seed = 1, print = F, plot = F)
# fit2c <- ame(cv.mx2, 
#              symmetric = T, R = 2, model = "nrm", intercept = F,
#              burn = toBurn, nscan = imp, odens = 1, rvar = T, cvar = T,
#              seed = 1, print = F, plot = F)
# fit2c$YPM


angle1 <- getLatAngle(fit1) 
# angle1c <- getLatAngle(fit1c)

angle2 <- getLatAngle(fit2)
# angle2c <- getLatAngle(fit2c)

save(fit1, fit2, angle1, angle2, file = "result_ame.rda")


# Visualization #----------------------------------------------------------------------------------#
library(ggplot2)

# circplot(Y = cv.mx1, U = fit1$base$U, V = fit1$base$V, vscale = .5,
#          lcol = "gray", rcol = "brown", ccol = "blue")
# circplot(Y = cv.mx2, U = fit2$base$U, V = fit2$base$V, vscale = .5)

politSC18 <- c("Xi Jinping", "Li Keqiang", "Zhang Dejiang", "Yu Zhengsheng", "Liu Yunshan",
               "Wang Qishan", "Zhang Gaoli")

politSC18c <- c("Xi Jinping", "Li Keqiang", "Zhang Dejiang", "Yu Zhengsheng", "Liu Yunshan",
                "Wang Qishan", "Zhang Gaoli", "Ma Kai", "Wang Huning", "Liu Yandong", "Liu Qibao",
                "Xu Qiliang", "Sun Chunlan", "Sun Zhengcai", "Li Jianguo", "Li Yuanchao", "Wang Yang",
                "Zhang Chunxian", "Fan Changlong", "Meng Jianzhu", "Zhao Leji", "Hu Chunhua",
                "Li Zhanshu", "Guojin Long", "Han Zheng")

politSC19 <- c("Xi Jinping", "Li Keqiang", "Li Zhanshu", "Wang Yang", "Wang Huning", "Zhao Leji", "Han Zheng")

politSC19c <- c("Xi Jinping", "Li Keqiang", "Li Zhanshu", "Wang Yang", "Wang Huning", "Zhao Leji",
                "Han Zheng", "Ding Xuexiang", "Wang Chen", "Liu He", "Xu Qiliang", "Sun Chunlan",
                "Li Xi", "Li Qiang", "Li Hongzhong", "Yang Jiechi", "Yang Xiaodu", "Zhang Youxia",
                "Chen Xi", "Chen Quanguo", "Chen Miner", "Hu Chunhua", "Guo Shengkun", "Huang Kunming",
                "Cai Qi")

par(mar = c(0,0,0,0))
circplot(Y = cv.mx2, U = fit2$U)
circSlice(Y = cv.mx2, U = fit2$U, politSC18c)
circSlice(Y = cv.mx2, U = fit2$U, politSC19c)
# circplot(Y = cv.mx2, U = fit2c$base$U, V = fit2c$base$V, vscale = .5)


# By year #----------------------------------------------------------------------------------------#
load("cv_matrix_yr.rda")

imp <- 10000; toBurn <- 5000

fit1.13 <- ame(cv.mx1.13, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit2.13 <- ame(cv.mx2.13, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit1.14 <- ame(cv.mx1.14, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit2.14 <- ame(cv.mx2.14, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit1.15 <- ame(cv.mx1.15, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit2.15 <- ame(cv.mx2.15, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit1.16 <- ame(cv.mx1.16, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit2.16 <- ame(cv.mx2.16, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit1.17 <- ame(cv.mx1.17, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

fit2.17 <- ame(cv.mx2.17, 
               symmetric = T, R = 2, model = "nrm", intercept = F,
               burn = toBurn, nscan = imp, odens = 1, rvar = F,
               cvar = F, seed = 1, print = F, plot = F)

save(fit1.13, fit2.13, fit1.14, fit2.14, fit1.15, fit2.15, fit1.16, fit2.16, fit1.17, fit2.17,
     file = "result_ame_yr.rda")

# 8*8
circSlice(Y = cv.mx2.13,  U = fit2.13$U, c(politSC18, politSC19))
circSlice(Y = -cv.mx2.14,  U = -fit2.14$U, c(politSC18, politSC19))
circSlice(Y = cv.mx2.15,  U = fit2.15$U, c(politSC18, politSC19))
circSlice(Y = cv.mx2.16,  U = fit2.16$U, c(politSC18, politSC19))

circSlice(Y = cv.mx2.13,  U = fit2.13$U, politSC19)
circSlice(Y = cv.mx2.14,  U = fit2.14$U, politSC19)
circSlice(Y = cv.mx2.15,  U = fit2.15$U, politSC19)
circSlice(Y = cv.mx2.16,  U = fit2.16$U, politSC19)

# circplot(Y = cv.mx2.13, U = fit2.13$U)
# circplot(Y = cv.mx2.14, U = fit2.14$U)
# circplot(Y = cv.mx2.15, U = fit2.15$U)
# circplot(Y = cv.mx2.16, U = fit2.16$U)


# Plot Selected #----------------------------------------------------------------------------------#
library(amen); source("functions.R")
load("cv_matrix.rda"); load("result_ame.rda")

circplot(Y = cv.mx2, U = fit2$base$U, V = fit2$base$V, vscale = .5)
