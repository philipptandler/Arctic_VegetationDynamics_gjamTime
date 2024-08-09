# redirect .rhoPrior to ignore warning
.rhoPriorMod <- function(lprior, w, x, tindex, xnames, 
                         snames, other, notOther, timeLast = NULL){
  
  loRho <- lprior$lo
  hiRho <- lprior$hi
  rho   <- (loRho + hiRho)/2
  
  if(length(other) > 0)loRho[,other] <- hiRho[,other] <- NA
  
  lkeep    <- which(!is.na(loRho))
  
  M  <- nrow(rho)
  rownames(rho)[1] <- 'intercept'
  S  <- ncol(rho)
  SS <- length(notOther)
  n  <- nrow(x)
  wz   <- w
  
  gindex <- kronecker(diag(S),rep(1,M)) 
  gindex <- gindex[lkeep,]
  
  wg     <- which(gindex == 1,arr.ind=T)
  wc     <- matrix(rep(1:M,S*S),S*M,S)[lkeep,]
  rowG   <- wc[wg]
  gindex <- cbind(rowG,wg)
  tmp    <- as.vector( t(outer(colnames(rho)[notOther],
                               rownames(rho),paste,sep='_') ) )
  rownames(gindex) <- tmp[lkeep]
  
  colX <- match(rownames(rho),colnames(x))
  colX <- colX[rowG]
  gindex <- cbind(colX, gindex)
  colnames(gindex)[3:4] <- c('rowL','colW')
  nV <- nrow(gindex)
  
  # Vmat is w[t-1,]*x[t,]
  Vmat <- matrix(0,n,nV)
  wz[wz < 0] <- 0
  Vmat[tindex[,1],] <- wz[tindex[,1], gindex[,'colW']]*x[tindex[,1], gindex[,'colX']]
  Vmat[timeLast,]   <- wz[timeLast, gindex[,'colW']]*x[timeLast, gindex[,'colX']]
  
  Rmat <- matrix(NA,nV,S)
  rownames(Rmat) <- rownames(gindex)
  loRmat <- hiRmat <- Rmat[,notOther]
  
  Rmat[ gindex[,c('rowL','colW')] ] <- rho[ gindex[,c('rowG','colW')] ]
  
  lo <- hi <- Rmat*0
  lo[ gindex[,c('rowL','colW')] ] <- loRho[ gindex[,c('rowG','colW')] ]
  hi[ gindex[,c('rowL','colW')] ] <- hiRho[ gindex[,c('rowG','colW')] ]
  Rmat[ is.nan(Rmat) ] <- 0
  
  wL <- which(!is.na(Rmat[,notOther]),arr.ind=T)
  # lo[is.na(lo)] <- 0
  # hi[is.na(hi)] <- 0
  
  list(Rmat = Rmat, loRmat = lo[,notOther], hiRmat = hi[,notOther], wL = wL, 
       gindex = gindex, Vmat = Vmat)
}
