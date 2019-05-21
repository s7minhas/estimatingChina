#######################################################
if(Sys.info()['user'] %in% c('s7m','janus829')){
  setwd('~/Dropbox/Research/EstimatingChina/analysis')
  source('~/Dropbox/Research/EstimatingChina/analysis/functions.R')
  pathGraphics = '~/Dropbox/Research/EstimatingChina/figure/'
}
#######################################################

#######################################################
# load ame results and original network
load("ChinaVitae_Test.rda")
load("cv_matrix.rda")
#######################################################

#######################################################
# latent factor plot
uViz = ggCirc(Y=cv.mx2, U=fit2$U, 
  geomLabel=FALSE, geomText=TRUE,
  prange=c(2,5),
  showActLinks=FALSE, lsize=.0001, lcol='gray85'
  ) +
theme_bw() +
theme(
    legend.position='none',
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    panel.border=element_blank()    
    )
ggsave(uViz, 
	file=paste0(pathGraphics, 'uViz.pdf'), 
	width=8, height=8)
#######################################################

#######################################################
## uv subplots
# Politburo Standing Committee of the 18th Central Committee
politSC18 = c(
  'Xi Jinping', 'Li Keqiang', 'Zhang Dejiang', 
  'Yu Zhengsheng', 'Liu Yunshan', 'Wang Qishan', 'Zhang Gaoli'
  )
politSC18Circ = circSlice(Y=cv.mx2, U=fit2$U, politSC18) +
  ggtitle('Politburo Standing Committee: 18th Central Committee')
politSC18Circ
ggsave(politSC18Circ, file=paste0(pathGraphics, 'politSC18_circPlot.pdf'), width=6, height=6)

# Rest of Politburo for 18th 
polit18 = c(
  'Ma Kai', 'Wang Huning', 'Liu Yandong', 'Liu Qibao', 'Xu Qiliang', 
  'Sun Chunlan', 'Sun Zhengcai', 'Li Jianguo', 'Li Yuanchao', 
  'Wang Yang', 'Zhang Chunxian', 'Fan Changlong', 'Meng Jianzhu', 
  'Zhao Leji', 'Hu Chunhua', 'Li Zhanshu', 'Guo Jinlong', 'Han Zheng'
  )
polit18Circ = circSlice(Y=cv.mx2, U=fit2$U, polit18) +
  ggtitle('Politburo: 18th Central Committee')
ggsave(polit18Circ, file=paste0(pathGraphics, 'polit18_circPlot.pdf'), width=6, height=6)

# Politburo Standing Committee of the 19th Central Committee
politSC19 = c(
  'Xi Jinping', 'Li Keqiang', 'Li Zhanshu', 'Wang Yang', 
  'Wang Huning', 'Zhao Leji', 'Han Zheng'
  )
politSC19Circ = circSlice(Y=cv.mx2, U=fit2$U, politSC19) +
  ggtitle('Politburo Standing Committee: 19th Central Committee')
ggsave(politSC19Circ, file=paste0(pathGraphics, 'politSC19_circPlot.pdf'), width=6, height=6)

# Rest of Politburo for 19th 
polit19 = c(
  'Ding Xuexiang', 'Wang Chen', 'Liu He', 'Xu Qiliang', 'Sun Chunlan', 
  'Li Xi', 'Li Qiang', 'Li Hongzhong', 'Yang Jiechi', 'Yang Xiaodu', 
  'Zhang Youxia', 'Chen Xi', 'Chen Quanguo', 'Chen Miner', 'Hu Chunhua', 
  'Guo Shengkun', 'Huang Kunming', 'Cai Qi'
  )
polit19Circ = circSlice(Y=cv.mx2, U=fit2$U, polit19) +
  ggtitle('Politburo: 19th Central Committee')
ggsave(polit19Circ, file=paste0(pathGraphics, 'polit19_circPlot.pdf'), width=6, height=6)
#######################################################