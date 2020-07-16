# WORKSPACE #######################################################################
rm(list=ls())
if(Sys.info()['user'] %in% c('Owner','herme', 'S7M')){
  u = Sys.info()['user']
  pathBase = paste0('C:/Users/', u, '/')
  pathDrop = paste0(pathBase, 'Dropbox/Research/EstimatingChina/')
  pathGit = paste0(pathBase, 'Research/EstimatingChina/')
  pathGraphics = paste0(pathGit, 'figure/')
  source(paste0(pathGit, 'analysis/functions.R'))
  setwd(paste0(pathGit, 'analysis'))
}

# libraries
library(reshape2)
library(MASS)
library(ggplot2) ; theme_set(theme_bw())
library(xtable)
library(apsrtable)
library(magrittr)

# function to calculate cos sim score
getCosSim = function(x,y){
  c = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(c) }

# loop over matrix of positions and calculate cos sim for every pair
cosSimMat = function(x) {
  # org output object
  m = matrix(NA,
    nrow=ncol(x),ncol=ncol(x),
    dimnames=list(colnames(x),colnames(x)))
  cos = as.data.frame(m)

  # loop through pairs and store in cos
  for(i in 1:ncol(x)) {
    for(j in i:ncol(x)) {
      co_rate_1 = x[which(x[,i] & x[,j]),i]
      co_rate_2 = x[which(x[,i] & x[,j]),j]
      cos[i,j]= getCosSim(co_rate_1,co_rate_2)
      cos[j,i]=cos[i,j] } }

  # return cos
  return(cos) }

# seed
set.seed(6886)
########################################################################

# DATA #######################################################################
# load data and ame results
load("ChinaVitae_Test.rda") # relevant objects: test.data, fit2
########################################################################

# LATENT DISTANCE SCORES #######################################################################
# pull out positions on unit circle from ame fit2 object
U=getPosInSpace(fit2$ULUPM)
mu=sqrt( apply(U^2,1,sum) ) ; u=diag(1/mu)%*%U
rownames(u) = rownames(U)

# calculate cosine sim scores
simScores = data.matrix(cosSimMat(t(u)))
simScores = melt(simScores)

# slice by xi and li
xi = simScores[simScores$Var1=='Xi Jinping',]
li = simScores[simScores$Var1=='Li Keqiang',]

# merge in var
test.df$Xi.y2 = xi$value[match(test.df$IND, xi$Var2)]
test.df$Li.y2 = li$value[match(test.df$IND, li$Var2)]

# reverse scale
test.df$Xi.y = test.df$Xi.y2*(-1)
test.df$Li.y = test.df$Li.y2*(-1)
########################################################################

# OUT OF SAMPLE FOR XI AND NOT MODELS #######################################################################
# set up out of sample results for first outsamp table
test.data = test.df[!is.na(test.df$Xi.x),]
test.data$slice = sample(1:20, dim(test.data)[1],replace = T)

# add vars to store preds
test.data$pred.m1 = NA
test.data$pred.m2 = NA
test.data$pred.m3 = NA

# loop through mods and get osamp preds
for(i in 1:20){
  # total number of appearances
  m1 = glm.nb(total ~ node.y, data = test.data[test.data$slice != i,])
  # total app and total coapp with xi
  m2 = glm.nb(total ~ node.y +xi.c.y, data = test.data[test.data$slice != i,])
  # totalapp and lat dist with xi
  m3 = glm.nb(total ~ node.y +Xi.y, data = test.data[test.data$slice != i,])

  # get preds
  test.data$pred.m1[test.data$slice == i] = predict(m1, test.data[test.data$slice == i,], type = "response")
  test.data$pred.m2[test.data$slice == i] = predict(m2, test.data[test.data$slice == i,], type = "response")
  test.data$pred.m3[test.data$slice == i] = predict(m3, test.data[test.data$slice == i,], type = "response")
  }

# calc perf scores
poisPerf = lapply(1:3, function(f){
  perfScore(test.data$total, test.data[,paste0('pred.m',f)])
}) %>% do.call('rbind', .) %>% signif(.,3)
rownames(poisPerf) = paste0('m',1:nrow(poisPerf))
rownames(poisPerf) = c(
  "Total Appearance", "Total and Coappearance",
  "Total Appearance and Latent Distance to Xi")
colnames(poisPerf) = c('Logarithmic', 'Brier', 'Spherical', 'Dawid-Sebastiani', 'RMSE')

# save to paper directory
print.xtable(
  xtable(poisPerf,
    align='lccccc',
    caption='Out-of-sample performance on scoring rule metrics.',
    label='tab:outPerf'
  ),
  include.rownames=TRUE,
  hline.after=c(0,0,1, nrow(poisPerf), nrow(poisPerf)),
  size='normalsize',
  file=paste0(pathPaper, 'outPerfTable.tex') )
########################################################################

# OUT OF SAMPLE FOR XI vs. LI MODELS #######################################################################
# set up out of sample results for xi v. li analysis
for(i in 1:20){
  # total app and lat dist with xi
  m1 = glm.nb(total ~ node.y + Xi.y, data = test.data[test.data$slice != i,])
  # total app and lat dist with li
  m2 = glm.nb(total ~ node.y + Li.y, data = test.data[test.data$slice != i,])

  # get preds
  test.data$pred.m1[test.data$slice == i] = predict(m1, test.data[test.data$slice == i,], type = "response")
  test.data$pred.m2[test.data$slice == i] = predict(m2, test.data[test.data$slice == i,], type = "response")
}

# calc perf scores
poisPerf = lapply(1:2, function(f){
  forPerf = data.frame(total=test.data$total, pred=test.data[,paste0('pred.m',f)])
  forPerf = na.omit(forPerf)
  perfScore(forPerf$total, forPerf$pred)
}) %>% do.call('rbind', .) %>% signif(.,3)
rownames(poisPerf) = paste0('m',1:nrow(poisPerf))
rownames(poisPerf) = c(
  "Total Appearance and Latent Distance to Xi",
  "Total Appearance and Latent Distance to Li")
colnames(poisPerf) = c('Logarithmic', 'Brier', 'Spherical', 'Dawid-Sebastiani', 'RMSE')

# save to paper directory
print.xtable(
  xtable(poisPerf,
    align='lccccc',
    caption='Out-of-sample performance on scoring rule metrics.',
    label='tab:outPerf2'
  ),
  include.rownames=TRUE,
  hline.after=c(0,0,1, nrow(poisPerf), nrow(poisPerf)),
  size='normalsize',
  file=paste0(pathPaper, 'outPerfTable2.tex') )
########################################################################

# COEF SUMMARY FOR XI AND NOT MODELS #######################################################################
# build coefficient table for first set of models
m1 = glm.nb(total~node.y, data = test.data)
m2 = glm.nb(total~node.y + xi.c.y, data = test.data)
m3 = glm.nb(total~node.y + Xi.y, data = test.data)

apsrtable(m1, m2, m3,
	model.names=c('Total Appearances', 'Total \\& Coappearances', 'Total \\& Latent Distance'),
	coef.names=c('(Intercept)', 'Total Appearances', 'Theta', 'Coappearances with Xi', 'Latent Distance from Xi'),
	stars='default'
  )

# create sim based effects plot
scen = cbind(1, mean(test.df$node.y), seq(-1,1,.1))
beta = mvrnorm(1000, coef(m3), vcov(m3))
z = beta %*% t(scen)
y = exp(z)

# summarize results from sims
ggData = t(apply(y, 2,
   function(x){ mean = mean(x) ;
     qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
     qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
     rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
ggData = data.frame(ggData)
colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
ggData$x = scen[,3]

# create viz
gg=ggplot(ggData, aes(x=x, y=Fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=Lo95, ymax=Hi95), alpha=.3) +
  geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=.5) +
  geom_rug(data=test.data, inherit.aes=FALSE, aes(x=Xi.y)) +
  xlab('Latent Angle Distance to Xi') +
  ylab('Number of LSG Appointments') +
  theme(
    panel.border=element_blank(),
    axis.ticks=element_blank() )

# save to graphics dir
ggsave(gg, file=paste0(pathGraphics, 'effects.pdf'), width=8, height=4)
########################################################################

# COEF SUMMARY FOR XI vs. LI MODELS #######################################################################
# build coefficient table for xi vs li models
m1 = glm.nb(total ~ node.y + Xi.y, data = test.data)#, family = "poisson") # total app and lat dist with xi
m2 = glm.nb(total ~ node.y + Li.y, data = test.data)#, family = "poisson") # total app and lat dist with li
m3 = glm.nb(total ~ node.y + Xi.y + Li.y, data = test.data)#, family = "poisson") # robustness check

apsrtable(m1, m2,
  model.names=c('Xi Only', 'Li Only'),
  coef.names=c('(Intercept)', 'Total Appearances', 'Latent Distance from Xi', 'Theta', 'Latent Distance from Li'),
  stars='default'
  )
########################################################################

# BIVARIATE PROBIT ANALYSIS #######################################################################
# run bivariate probit models
library(Zelig); library(ZeligChoice)

m0 = zelig(cbind(cn.b, st.b) ~ node.y, model = "bprobit", data = test.df)
m1 = zelig(cbind(cn.b, st.b) ~ xi.c.y, model = "bprobit", data = test.df)

m2 = zelig(cbind(cn.b, st.b) ~ Xi.y, model = "bprobit", data = test.df)
m3 = zelig(cbind(cn.b, st.b) ~ node.y + Xi.y, model = "bprobit", data = test.df)

m4 = zelig(cbind(cn.b, st.b) ~ Li.y, model = "bprobit", data = test.df)
m5 = zelig(cbind(cn.b, st.b) ~ node.y + Li.y, model = "bprobit", data = test.df)

summary(m0)
summary(m2)
summary(m4)
summary(m3)
summary(m5)
########################################################################
