#######################################################
if(Sys.info()['user'] %in% c('Owner','herme', 'S7M')){
  u = Sys.info()['user']
  pathBase = paste0('C:/Users/', u, '/')
  pathDrop = paste0(pathBase, 'Dropbox/Research/EstimatingChina/')
  pathGit = paste0(pathBase, 'Research/EstimatingChina/')
  pathGraphics = paste0(pathGit, 'figure/')
  source(paste0(pathGit, 'analysis/functions.R'))
  setwd(paste0(pathGit, 'analysis'))
}

# libs
library(amen)
library(igraph)

# helpers
summStats = function(x,label){
	mu = mean(x)
	qts = quantile(x, probs=c(0.025,0.05,0.95,0.975))
	names(qts) = c('lo90','lo95','hi90','hi95')
	out = data.frame(t(c(mu=mu,qts)))
	out$model = label
	return( out ) }
#######################################################

#######################################################
# load original network
load("cv_matrix.rda")
#######################################################

#######################################################
# make graph pretty
rescale = function(x,new_max,new_min){
 xResc = (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

set.seed(6886)
g = graph_from_adjacency_matrix(cv.mx2, mode='undirected', weighted = TRUE)
vSize = log(degree(g))
eWidth = .1*(E(g)$weight)^.5
vColor = rep('grey40', nrow(cv.mx2))
vColor[rownames(cv.mx2)=='Xi Jinping'] = 'blue'
vColor[rownames(cv.mx2)=='Li Keqiang'] = 'red'
par(bg='transparent')
# pdf(file=paste0(pathGraphics, 'dvViz.pdf'), width=12, height=12)
pdf(file=paste0(pathGraphics, 'dvViz_names.pdf'), width=12, height=12)
plot(g,
	layout=layout_with_fr(g),
	vertex.label.cex=.0001,
	vertex.color='grey40',
	vertex.size=vSize,
	edge.width=eWidth,
	edge.color='grey80',
	asp=FALSE,
	bg='transparent'
	)
dev.off()
system(paste0('pdfcrop ', pathGraphics, 'dvViz.pdf ',  pathGraphics, 'dvViz.pdf') )

set.seed(6886)
pdf(file=paste0(pathGraphics, 'dvViz_names.pdf'), width=12, height=12)
plot(g,
	layout=layout_with_fr(g),
	vertex.label.cex=rescale(degree(g), .8, .5),
	vertex.color='grey40',
	vertex.size=.0001,
	edge.width=eWidth,
	edge.color='grey90',
	asp=FALSE,
	bg='transparent',
	vertex.label=V(g)$name,
	vertex.label.color='black'
	)
dev.off()
system(paste0('pdfcrop ', pathGraphics, 'dvViz_names.pdf ',  pathGraphics, 'dvViz_names.pdf') )
#######################################################

#######################################################
# compare a simple bayesian logit in its ability to predict the level
# of triadic dependencies in the data
if(!file.exists(paste0(pathAnalysis, 'base.rda'))){
	base = ame(Y=cv.mx2,
		rvar=FALSE, cvar=FALSE, dcor=FALSE, R=0,
		symmetric = TRUE, model='nrm',
		plot=FALSE, print=FALSE, gof=TRUE
		)
	save(base, file=paste0(pathAnalysis, 'base.rda'))
}
load(paste0(pathAnalysis, 'base.rda'))

## compare to ame spec
load("ChinaVitae_Test.rda")
probitGOF = base$GOF ; ameGOF = fit2$GOF
actVal = probitGOF[1,'triad.dep']
probitGOF = probitGOF[-1,'triad.dep'] ; ameGOF = ameGOF[-1,'triad.dep']

ggData = rbind(
	summStats(probitGOF, 'Probit'), summStats(ameGOF, 'LFM')
	)
ggData$actVal = actVal

ggplot(ggData, aes(x=model, y=mu)) +
	geom_point() +
	geom_linerange(aes(ymin=lo90, ymax=hi90)) +
	geom_hline(aes(yintercept=actVal)) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)

tmp = data.frame(value=ameGOF) ; tmp$model = 'AME'
tmp2 = data.frame(value=probitGOF) ; tmp2$model = 'Probit'
ggData = data.frame(rbind(tmp, tmp2))
ggData$actVal = actVal

ggplot(ggData, aes(x=model, y=value)) +
	geom_jitter(alpha=.3) +
	# geom_boxplot(outlier.alpha=.001) +
	geom_hline(aes(yintercept=actVal)) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
#######################################################
