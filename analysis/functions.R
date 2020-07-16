library(extrafont)

# calc CI ranges, beta df must have mean and sd cols
getCIVecs = function(beta){
  beta$lo95 = beta$mean - qnorm(.975)*beta$sd
  beta$hi95 = beta$mean + qnorm(.975)*beta$sd
  beta$lo90 = beta$mean - qnorm(.95)*beta$sd
  beta$hi90 = beta$mean + qnorm(.95)*beta$sd
  return(beta)
}

# colors for sig
coefp_colors = c(
  "Positive"=rgb(54, 144, 192, maxColorValue=255),
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insignificant" = rgb(150, 150, 150, maxColorValue=255)
  )

# add sig col to beta df gen from getCIVecs
getSigVec = function(beta){
  beta$sig = NA
  beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
  beta$sig[beta$lo95 > 0] = "Positive"
  beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
  beta$sig[beta$hi95 < 0] = "Negative"
  beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
  return(beta)
}

getPosInSpace = function(latObject, symmetric = TRUE){
  if(symmetric){
    ULUPM = latObject
    eULU = eigen(ULUPM)
    eR<- which( rank(-abs(eULU$val),ties.method="first") <= 2 )
    U<-eULU$vec[,seq(1,2,length=2),drop=FALSE] %*% sqrt(diag(eULU$val[1:2]))
    L<-eULU$val[eR]
    rownames(U)<-dimnames(ULUPM)[[1]]
    return(U)
  }
  if(!symmetric){
    UVPM = latObject
    R = 2
    UDV<-svd(UVPM)
    U<-UDV$u[,seq(1,R,length=R)]%*%diag(sqrt(UDV$d)[seq(1,R,length=R)],nrow=R)
    V<-UDV$v[,seq(1,R,length=R)]%*%diag(sqrt(UDV$d)[seq(1,R,length=R)],nrow=R)
    rownames(U)<-rownames(V)<-dimnames(UVPM)[[1]]
    return(list(U = U, V = V))
  }
}

getLatAngle = function(x){
  U=getPosInSpace(x$ULUPM)
  V<-U ; vscale<-1
  mu<-sqrt( apply(U^2,1,sum) ) ; mv<-sqrt( apply(V^2,1,sum) )
  u<-diag(1/mu)%*%U ; v<-diag(1/mv)%*%V*vscale
  rownames(u) = rownames(U)
  angles = matrix(acos(u[,1]), nrow(u), 1)
  rownames(angles) = rownames(U)

  dists = expand.grid(rownames(angles), rownames(angles))
  dists = dists[dists$Var1 != dists$Var2,]
  dists$ang1 = angles[dists$Var1] ; dists$ang2 = angles[dists$Var2]
  dists$value = abs( dists$ang1 - dists$ang2 )
  return(dists[,c('Var1','Var2','value')])
}


################
# lat fac plot

################
library(ggrepel)
library(ggplot2)
theme_set(theme_bw())

getDataForCirc = function(
  Y, U=NULL, V=NULL, row.names=rownames(Y),
  col.names=colnames(Y), vscale=.8, removeIsolates=TRUE,
  uLabel='U', vLabel='V'
  ){

  #
  vLogic = is.null(V) ; uLogic = is.null(U)

  if (uLogic) {
      a <- rowMeans(Y, na.rm = TRUE)
      b <- colMeans(Y, na.rm = TRUE)
      Y0 <- Y
      Y0[is.na(Y)] <- (outer(a, b, "+"))[is.na(Y)]
      Y0 <- Y0 - mean(Y0)
      if (!all(Y == t(Y), na.rm = TRUE)) {
          sY <- svd(Y0)
          u <- sY$u[, 1:2]
          v <- sY$v[, 1:2]
          mu <- sqrt(apply(u^2, 1, sum))
          mv <- sqrt(apply(v^2, 1, sum))
          u <- diag(1/mu) %*% u
          v <- diag(1/mv) %*% v * vscale
      }
      if (all(Y == t(Y), na.rm = TRUE)) {
          eY <- eigen(Y0)
          bv <- which(abs(eY$val) >= sort(abs(eY$val), decreasing = TRUE)[2])[1:2]
          u <- eY$vec[, bv]
          mu <- sqrt(apply(u^2, 1, sum))
          u <- diag(1/mu) %*% u
          mv <- mu
          v <- u
      }
  }
  if (!uLogic) {
      if (vLogic) {
          V <- U
          vscale <- 1
      }
      mu <- sqrt(apply(U^2, 1, sum))
      mv <- sqrt(apply(V^2, 1, sum))
      u <- diag(1/mu) %*% U
      v <- diag(1/mv) %*% V * vscale
  }

  rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
  csum <- apply(abs(Y), 2, sum, na.rm = TRUE)
  links <- which(Y != 0, arr.ind = TRUE)

  # org df for gg
  uG = data.frame(u*1.2)
  uG$actor = rownames(Y)
  uG$tPch = 0 ; uG$tPch[rsum>0] = (mu[rsum>0])^3
  if(removeIsolates){ uG = uG[uG$tPch>0,] }
  uG$tPch = uG$tPch

  # add v if supplied
  if(!vLogic){
    vG = data.frame(v*1.2)
    vG$actor = rownames(Y)
    vG$tPch = 0 ; vG$tPch[csum>0] = (mv[csum>0])^3
    if(removeIsolates){ vG = vG[vG$tPch>0,] }
    vG$tPch = vG$tPch

    uG$eff = uLabel ; vG$eff = vLabel
    uG = rbind(uG, vG)
    uG$eff = factor(uG$eff, levels=c(uLabel,vLabel)) }

  #
  out = list(uG=uG, U=U, V=V, links=links, u=u, v=v)
  return(out)
}

circSlice = function(
  Y, U, sliceLabel,
  ptColor='gray48', labelSizeRange=c(2,6)){
  ggU = getDataForCirc(Y=Y, U=U)$uG
  ggU$lab = ggU$actor
  ggU$lab[!ggU$lab %in% sliceLabel] = ''
  ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0
  circSlice = ggplot(ggU, aes(x=X1, y=X2, size=tPch)) +
    geom_point(alpha=.9, color=ptColor) +
    scale_size(range=labelSizeRange) +
    ylab("") + xlab("") +
    geom_label_repel(
      aes(label=lab, size=lPch)) +
    theme(
      legend.position = 'none',
      panel.border = element_blank(), panel.grid=element_blank(),
      axis.ticks = element_blank(), axis.line=element_blank(),
      axis.text = element_blank()
      ) + theme(panel.grid = element_line())
  return(circSlice)
}

ggCirc = function(
  Y, U=NULL, V=NULL, row.names=rownames(Y), col.names=colnames(Y),
  vscale=.8, prange=c(2,5), lcol='gray85', ltype='dotted', lsize=.5,
  force=1, maxIter = 3e3, removeIsolates=TRUE, uLabel='U', vLabel='V',
  ptColor='gray48', ptAlpha=.9,
  showActLinks=TRUE, geomLabel=TRUE, geomText=FALSE, geomPoint=TRUE, ...
  ){

  #
  vLogic = is.null(V) ; uLogic = is.null(U)

  if (uLogic) {
      a <- rowMeans(Y, na.rm = TRUE)
      b <- colMeans(Y, na.rm = TRUE)
      Y0 <- Y
      Y0[is.na(Y)] <- (outer(a, b, "+"))[is.na(Y)]
      Y0 <- Y0 - mean(Y0)
      if (!all(Y == t(Y), na.rm = TRUE)) {
          sY <- svd(Y0)
          u <- sY$u[, 1:2]
          v <- sY$v[, 1:2]
          mu <- sqrt(apply(u^2, 1, sum))
          mv <- sqrt(apply(v^2, 1, sum))
          u <- diag(1/mu) %*% u
          v <- diag(1/mv) %*% v * vscale
      }
      if (all(Y == t(Y), na.rm = TRUE)) {
          eY <- eigen(Y0)
          bv <- which(abs(eY$val) >= sort(abs(eY$val), decreasing = TRUE)[2])[1:2]
          u <- eY$vec[, bv]
          mu <- sqrt(apply(u^2, 1, sum))
          u <- diag(1/mu) %*% u
          mv <- mu
          v <- u
      }
  }
  if (!uLogic) {
      if (vLogic) {
          V <- U
          vscale <- 1
      }
      mu <- sqrt(apply(U^2, 1, sum))
      mv <- sqrt(apply(V^2, 1, sum))
      u <- diag(1/mu) %*% U
      v <- diag(1/mv) %*% V * vscale
  }

  rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
  csum <- apply(abs(Y), 2, sum, na.rm = TRUE)
  links <- which(Y != 0, arr.ind = TRUE)

  # org df for gg
  uG = data.frame(u*1.2)
  uG$actor = rownames(Y)
  uG$tPch = 0 ; uG$tPch[rsum>0] = (mu[rsum>0])^3
  if(removeIsolates){ uG = uG[uG$tPch>0,] }
  uG$tPch = uG$tPch

  # add v if supplied
  if(!vLogic){
    vG = data.frame(v*1.2)
    vG$actor = rownames(Y)
    vG$tPch = 0 ; vG$tPch[csum>0] = (mv[csum>0])^3
    if(removeIsolates){ vG = vG[vG$tPch>0,] }
    vG$tPch = vG$tPch

    uG$eff = uLabel ; vG$eff = vLabel
    uG = rbind(uG, vG)
    uG$eff = factor(uG$eff, levels=c(uLabel,vLabel))
    ggCirc = ggplot(uG, aes(x=X1, y=X2,color=eff))
  }
  if(vLogic){
    ggCirc = ggplot(uG, aes(x=X1, y=X2))
  }

  # add segments
  if(showActLinks){
    for(i in 1:nrow(links)){
      ggCirc = ggCirc + geom_segment(
        x=u[links[i,1],1]*1.2, y=u[links[i,1],2]*1.2,
        xend=v[links[i,2],1]*1.2, yend=v[links[i,2],2]*1.2,
        color=lcol, linetype=ltype, size=lsize ) }
  }
  if(geomPoint){ ggCirc = ggCirc + geom_point(alpha=ptAlpha, color=ptColor) }
  if(geomLabel){ ggCirc = ggCirc + geom_label_repel(aes(label=actor, size=tPch, ...),
    force=force, max.iter=maxIter) }
  if(geomText){ ggCirc = ggCirc + geom_text_repel(aes(label=actor, size=tPch, ...),
    force=force, max.iter=maxIter) }
  ggCirc = ggCirc + scale_size(range=prange) +
    theme(
      legend.position='none',
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      axis.text=element_blank(),
      panel.border=element_blank(),
      panel.grid=element_blank()
      )
  return(ggCirc)
}
################

################
# perf metric for count models
perfScore = function(act, pred){
  logarith = -log(dpois(act, lambda=pred))
  brier = -2*dpois(act,lambda=pred) + sapply(pred,
    function(act){ sum(dpois(1:1000,lambda=act)^2) })
  spherical= - dpois(act,pred) / sqrt(sapply(pred,
    function(act){ sum(dpois(1:1000,lambda=act)^2) }))
  resid = act-pred
  dawid=(resid)^2/pred + log(pred)
  rmse = sqrt(mean( (resid)^2 )) # using median due to outliers

  return(c(
    logarith=mean(logarith[logarith!=Inf]),
    brier=mean(brier),
    spherical=mean(spherical[spherical!=-Inf]),
    dawidSebastiani=mean(dawid),rmse=rmse
    ))
}
################
