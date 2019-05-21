#######################################################
if(Sys.info()['user'] %in% c('s7m','janus829')){
  setwd('~/Dropbox/Research/EstimatingChina/analysis')
  source('~/Dropbox/Research/EstimatingChina/analysis/functions.R')
  pathGraphics = '~/Dropbox/Research/EstimatingChina/figure/'
}

# libs
library(ggplot2)
library(ggthemes)
library(lubridate)
library(magrittr)
#######################################################

#######################################################
# load co-appearance data
load('appearance_freq.rda')

# add month info
app.df$month = month(app.df$mo)
#######################################################

#######################################################
# fig 2
pasteMult = function(x,y,sepZ){
	apply(expand.grid(x,y), 1, paste, collapse=sepZ) }
lowActivityDates = pasteMult(c(2013:2016),c('02','08'), '-') %>%
	paste0(.,'-01') %>% as.Date() %>% data.frame(x=.)
highActivityDates = pasteMult(c(2013:2016),c('03','09'), '-') %>%
	paste0(.,'-01') %>% as.Date() %>% data.frame(x=.)	

g=ggplot(app.df) +
	geom_vline(data=lowActivityDates, aes(xintercept=x), linetype='dashed', color='grey70') +
	geom_vline(data=highActivityDates, aes(xintercept=x), linetype='dotted', color='grey70') +
	geom_line(aes(x = mo, y = total), color = "darkgrey") +
	geom_point(aes(x=mo,y=total), color='darkgrey',size=.7) +
	geom_line(aes(x = mo, y = xi), color = "red") +
	geom_point(aes(x = mo, y = xi), color = "red", size = .7, shape = 16) +
	geom_line(aes(x = mo, y = li), color = "blue", lty = "dashed") +
	geom_point(aes(x = mo, y = li), color = "blue", size = .7, shape = 15) +
	annotate('text', 
		x=lowActivityDates[c(1,5),1]-5, y=225, label=c('Feb.','Aug.'),
		color='grey60', size=2.5, hjust=1
		) +
	annotate('text', 
		x=highActivityDates[c(4,8),1]+5, y=225, label=c('Mar.','Sept.'),
		color='grey60', size=2.5, hjust=0
		) +
	labs(
		x='',
		y='Number of Events'
		) +
	theme(
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)
ggsave(g, file=paste0(pathGraphics, 'freq.pdf'), width=7, height=3)


# fig 3a
app.df$xi_li_prop = with(app.df, xi_li/xi)
app.df$li_xi_prop = with(app.df, xi_li/li)
g = ggplot(app.df) +
	geom_line(aes(x = mo, y = xi_li_prop), color = "red") +
	geom_point(aes(x = mo, y = xi_li_prop), color = "red", size = 1.5, shape = 16) +
	geom_line(aes(x = mo, y = li_xi_prop), color = "blue", lty = "dashed") +
	geom_point(aes(x = mo, y = li_xi_prop), color = "blue", size = 1.5, shape = 15) +
	labs(
		x='',
		y='Share of co-appearance: \nXi (red) and Li (blue)'
		) +
	theme(
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)
ggsave(g, file=paste0(pathGraphics, 'Xi_Li.pdf'), width=7, height=3)


# fig 3b
app.df$xi_zhang_prop = with(app.df, xi_zhang/xi)
app.df$zhang_xi_prop = with(app.df, xi_zhang/zhang)
g = ggplot(app.df) +
	geom_line(aes(x = mo, y = xi_zhang_prop), color = "red") +
	geom_point(aes(x = mo, y = xi_zhang_prop), color = "red", size = 1.5, shape = 16) +
	geom_line(aes(x = mo, y = zhang_xi_prop), color = "blue", lty = "dashed") +
	geom_point(aes(x = mo, y = zhang_xi_prop), color = "blue", size = 1.5, shape = 15) +
	labs(
		x='',
		y='Share of co-appearance: \nXi (red) and Zhang G. (blue)'
		) +
	theme(
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)
ggsave(g, file=paste0(pathGraphics, 'Xi_ZG.pdf'), width=7, height=3)	
#######################################################