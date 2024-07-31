
## To track errors
.gjamMod <- function(formula, xdata, ydata, modelList, verbose = FALSE){
  
  holdoutN      <-  0
  holdoutIndex  <- numeric(0)
  breakList <- modelSummary <- reductList  <- traitList <- NULL
  specByTrait <- traitTypes <- notStandard <- NULL
  censor <- censorCA <- censorDA <- CCgroups <- FCgroups <- intMat <- NULL
  N  <- r <- otherpar <- pg <- NULL
  bFacGibbs <- fSensGibbs <- sensTable <- NULL
  facNames  <- character(0)
  groupRandEff <- NULL
  x <- y <- y0 <- effort <- NULL
  xnames <- xlnames <- NULL
  ng     <- 2000
  burnin <- 500
  BPRIOR <- LPRIOR <- REDUCT <- TRAITS <- FULL <- FALSE
  termB <- termR <- termA <- FALSE
  PREDICTX <- TRUE
  rhoPrior <- betaPrior <- alphaPrior <- NULL
  
  RANDOM <- FALSE              # random group intercepts
  
  TIME <- FALSE
  timeList <- timeZero <- timeLast <- timeIndex <- groupIndex <- 
    rowInserts <- Rmat <- Amat <- beta <- NULL
  formulaBeta <- NULL
  xl <- NULL
  
  ematAlpha <- .5
  
  alpha.DP <- ncol( ydata )          # large values give more variation
  
  xdata <- as.data.frame( xdata )
  
  colnames(ydata) <- .cleanNames( colnames(ydata) )
  colnames(xdata) <- .cleanNames( colnames(xdata) )
  
  wf <- which(sapply(xdata, is.factor))
  if(length(wf) > 0){
    for(j in wf){
      jt <- as.character(xdata[,j])
      jt <- .cleanNames( jt )
      xdata[,j] <- as.factor(jt)
    }
  }
  
  if(alpha.DP == 1)
    stop('this is a multivariate model: at least 2 columns needed in ydata')
  
  for(k in 1:length(modelList))assign( names(modelList)[k], modelList[[k]] )
  
  if('CCgroups' %in% names(modelList))attr(typeNames,'CCgroups')   <- CCgroups
  if('FCgroups' %in% names(modelList))attr(typeNames,'FCgroups')   <- FCgroups
  if('CATgroups' %in% names(modelList))attr(typeNames,'CATgroups') <- CATgroups
  
  if(missing(xdata)) xdata <- environment(formula)
  
  formTerms <- unlist( strsplit( as.character(formula), '+', fixed=T) )
  formTerms <- .replaceString( formTerms[ !formTerms == '~' ], ' ', '')
  
  ft <- c( grep('_', formTerms), grep('-', formTerms) )
  if(length(ft) > 0)stop( " reserved characters '_' or '-' in formula variables, rename" )
  
  if( is.null(timeList) ){
    
    termB <- TRUE
    if( !is.null(betaPrior) ) BPRIOR <- TRUE
    
  }else{
    
    toConsole( 'Note: Fitted as a time series model', verbose = verbose )
    
    formTerms <- wterms <- character(0)
    
    ww <- which( sapply( timeList, is.null ) )
    if(length(ww) > 0)timeList <- timeList[ -ww ]
    
    for(k in 1:length(timeList))assign( names(timeList)[k], timeList[[k]] )
    
    if( "betaPrior" %in% names(timeList) ){
      
      tb <- .cleanTimePriors(formulaBeta, timeList$betaPrior, xdata)
      timeList$betaPrior <- tb$prior
      formTerms <- tb$formTerms
      xnames    <- tb$xnames
      
      wterms <- 'beta'
      termB  <- TRUE
      
      if( all(timeList$betaPrior$lo == -Inf) &
          all(timeList$betaPrior$hi == Inf) ){
        betaPrior <- NULL
        BPRIOR    <- FALSE
      }else{
        BPRIOR <- TRUE
      }
    }
    
    if( "rhoPrior" %in% names(timeList) ){
      
      formulaRho <- timeList$formulaRho
      
      tb <- .cleanTimePriors(formulaRho, timeList$rhoPrior, xdata)
      timeList$rhoPrior <- tb$prior
      formTerms <- unique( c(formTerms, tb$formTerms) )
      xlnames   <- tb$xnames
      wterms    <- c( wterms, 'rho')
      
      termR  <- TRUE
      LPRIOR <- TRUE
    }
    
    if( "alphaPrior" %in% names(timeList) ){
      timeList$alphaPrior$lo <- .cleanDims(timeList$alphaPrior$lo)
      timeList$alphaPrior$hi <- .cleanDims(timeList$alphaPrior$hi)
      wterms <- c( wterms, 'alpha')
      termA  <- TRUE
      APRIOR <- TRUE
    }
    
    toConsole( 'Note: Time series terms', wterms, verbose = verbose )
    
    if( length(formTerms) > 1 & '1' %in% formTerms )
      formTerms <- formTerms[ formTerms != '1' ]
    formTerms <- paste( '~', paste0(formTerms, collapse = '+') )
    formula   <- as.formula( formTerms  )
    
    TIME     <- T
    holdoutN <-  0
    holdoutIndex  <- numeric(0)
  }
  
  if( is.character(formula) )formula <- as.formula(formula)
  
  if( !is.null(traitList) ){
    TRAITS <- T
    for(k in 1:length(traitList))assign( names(traitList)[k], traitList[[k]] )
    
    stt <- .replaceString(colnames(specByTrait),'_','')
    colnames(specByTrait) <- stt
    colnames(plotByTrait) <- stt
    colnames(traitList$specByTrait) <- stt
    colnames(traitList$plotByTrait) <- stt
    modelList$traitList <- traitList
    
    toConsole( 'Fitted as a trait model', verbose = verbose )
  }
  
  if(burnin >= ng) stop( 'burnin must be < no. MCMC steps, ng' )
  
  if('censor' %in% names(modelList)){
    for(k in 1:length(censor)){
      if( nrow(censor[[k]]$partition) != 3 )
        stop('censor matrix: 3 rows for value, lo, hi')
      rownames(censor[[k]]$partition) <- c('value','lo','hi')
    }
  }
  
  S <- ncol(ydata)
  if(length(typeNames) == 1)typeNames <- rep(typeNames,S)
  if(length(typeNames) != S) 
    stop('typeNames must be one value or no. columns in y')
  
  ############### factors in y
  
  tmp   <- .checkYfactor(ydata, typeNames)
  ydata <- tmp$ydata; yordNames <- tmp$yordNames
  
  if(TRAITS){
    if(!all( typeNames %in% c('CC','FC') ) )
      stop('trait prediction requires composition data (CC or FC)')
    if(nrow(plotByTrait) != nrow(ydata))
      stop('nrow(plotByTrait) must equal nrow(ydata)')
    if(ncol(plotByTrait) != length(traitTypes))
      stop('ncol(plotByTrait) must equal length(traitTypes)')
    if(ncol(plotByTrait) != length(traitTypes))
      stop('ncol(plotByTrait) must equal length(traitTypes)')
    ii <- identical(rownames(specByTrait),colnames(ydata))
    if(!ii){
      ww <- match(colnames(ydata),rownames(specByTrait) )
      if( is.finite(min(ww)) ){
        specByTrait <- specByTrait[ww,]
      } else {
        stop( 'rownames(specByTrait) must match colnames(ydata)' )
      }
    }
    if(typeNames[1] == 'CC'){
      ytmp <- round(ydata,0)
      ytmp[ytmp == 0 & ydata > 0] <- 1
      ydata <- ytmp
      rm(ytmp)
    }
  }
  cat("Reached1")
  tmp <- .buildYdata(ydata, typeNames)
  y   <- as.matrix( tmp$y )
  ydataNames <- tmp$ydataNames
  typeNames  <- tmp$typeNames
  CCgroups   <- tmp$CCgroups
  FCgroups   <- tmp$FCgroups
  CATgroups  <- tmp$CATgroups
  if(TRAITS) rownames(specByTrait) <- colnames(y)
  
  S <- ncol(y)
  n <- nrow(y)
  
  
  toConsole( 'Observations and responses', c(n, S), verbose = verbose )
  
  tmp    <- .buildEffort(y, effort, typeNames, verbose)
  effort <- tmp
  effMat <- effort$values
  modelList$effort <- effort
  
  w <- y/effMat
  
  # efactor <- 1
  # efactor <- round( apply( w, 2, mean ) )
  # if( termA ) efactor[ 1:length(efactor) ] <- round( mean(efactor) )
  
  # if( mean(efactor) > 100 ){
  #   effMat <- t(t(effMat)*efactor)
  #   effort$values <- effMat
  #   w <- y/effMat
  # }
  
  tmp      <- .gjamGetTypes(typeNames)
  typeCols <- tmp$typeCols
  typeFull <- tmp$typeFull
  typeCode <- tmp$TYPES[typeCols]
  allTypes <- sort(unique(typeCols))
  
  #  if( UNSTAND )notStandard <- colnames(xdata)
  #  standard <- colnames(xdata)[!colnames(xdata) %in% notStandard]
  #  standard <- standard[ standard != 'intercept' ]
  
  tmp <- .gjamXY(formula, xdata, y, typeNames, notStandard, verbose = verbose) # all terms
  x      <- tmp$x; y <- tmp$y; snames <- tmp$snames
  xnames <- tmp$xnames
  interBeta   <- tmp$interaction 
  factorBeta  <- tmp$factorAll
  designTable <- tmp$designTable
  xscale      <- tmp$xscale
  predXcols   <- tmp$predXcols
  standMatSd  <- tmp$standMatSd
  standMatMu  <- tmp$standMatMu  
  xdataNames  <- tmp$xdataNames
  standRows   <- tmp$standRows
  
  factorRho <- interRho <- NULL
  xlnames   <- character(0)
  
  if( termB & TIME ){
    
    tmp    <- .gjamXY(formulaBeta, xdata, y, typeNames, notStandard)
    xnames <- tmp$xnames
    interBeta    <- tmp$interaction
    factorBeta   <- tmp$factorAll
    designTable  <- list(beta = tmp$designTable)
    standMatSdB  <- tmp$standMatSd
    standMatMuB  <- tmp$standMatMu
    standRowsB   <- tmp$standRows 
    notStandardB <- tmp$notStandard
  }
  
  if( termR ){
    
    tmp <- .gjamXY(formulaRho, xdata, y, typeNames, notStandard)
    xl  <- tmp$x
    xlnames      <- tmp$xnames
    interRho     <- tmp$interaction
    factorRho    <- tmp$factorAll
    designTable  <- append( designTable, list(rho = tmp$designTable) )
    standMatSdL  <- tmp$standMatSd
    standMatMuL  <- tmp$standMatMu
    standRowsL   <- tmp$standRows
    notStandardL <- tmp$notStandard[tmp$notStandard %in% xlnames]
    
    rho <- matrix(0, ncol(xl), ncol(y))
    rownames(rho) <- colnames(xl)
    colnames(rho) <- colnames(y)
  }
  
  modelList$formula      <- formula
  modelList$notStandard  <- notStandard
  
  Q <- ncol(x)
  
  tmp <- .gjamMissingValues(x, y, factorBeta$factorList, typeNames, verbose)
  xmiss  <- tmp$xmiss;   xbound <- tmp$xbound; 
  ymiss  <- tmp$ymiss;   missY <- tmp$missY
  xprior <- tmp$xprior;  yprior <- tmp$yprior
  nmiss  <- nrow(xmiss); mmiss  <- nrow(ymiss)
  x  <- tmp$x; y <- tmp$y
  
  if( termR ){
    tmp <- .gjamMissingValues(xl, y, factorRho$factorList, typeNames)
    xlmiss  <- tmp$xmiss;   xlbound <- tmp$xbound; 
    xlprior <- tmp$xprior
    nlmiss  <- nrow(xmiss)
    xl <- tmp$x
  }
  cat("Reached2")
  
  tmp <- .gjamHoldoutSetup(holdoutIndex, holdoutN, n)
  holdoutIndex <- tmp$holdoutIndex; holdoutN <- tmp$holdoutN
  inSamples    <- tmp$inSamples;         nIn <- tmp$nIn
  
  tmp <- .gjamSetup(typeNames, x, y, breakList, holdoutN, holdoutIndex,
                    censor=censor, effort=effort) 
  w <- tmp$w; z <- tmp$z; y <- tmp$y; other <- tmp$other; cuts <- tmp$cuts
  cutLo       <- tmp$cutLo; cutHi <- tmp$cutHi; plo <- tmp$plo; phi <- tmp$phi
  ordCols     <- tmp$ordCols; disCols <- tmp$disCols; compCols <- tmp$compCols 
  conCols     <- which(typeNames == 'CON')
  classBySpec <- tmp$classBySpec; breakMat <- tmp$breakMat
  minOrd      <- tmp$minOrd; maxOrd <- tmp$maxOrd; censorCA <- tmp$censorCA
  censorDA    <- tmp$censorDA; censorCON <- tmp$censorCON; 
  ncut <- ncol(cuts);  corCols <- tmp$corCols
  catCols     <- which(attr(typeNames,'CATgroups') > 0)
  sampleW     <- tmp$sampleW
  ordShift    <- tmp$ordShift
  
  sampleW[censorCA] <- 1
  sampleW[censorDA] <- 1
  sampleW[censorCON] <- 1
  sampleWhold <- tgHold <- NULL
  wHold <- NULL
  wmax  <- 1.5*apply(y/effMat,2,max,na.rm=T)
  pmin  <- -abs(wmax)
  
  if(mmiss > 0){
    phi[ ymiss ] <- wmax[ ymiss[,2] ]
    plo[ ymiss ] <- pmin[ ymiss[,2] ]
    sampleW[ ymiss ] <- 1
  }
  
  ploHold <- phiHold <- NULL
  
  if( holdoutN > 0 ){
    
    sampleWhold <- sampleW[holdoutIndex,]  #to predict X
    sampleW[holdoutIndex,] <- 1
    tgHold  <- cuts
    wHold   <- w[drop=F,holdoutIndex,]
    ploHold <- plo[drop=F,holdoutIndex,]   # if LOHI: updated to current yp
    phiHold <- phi[drop=F,holdoutIndex,]
  }
  
  byCol <- byRow <- F
  if(attr(sampleW,'type') == 'cols')byCol <- T
  if(attr(sampleW,'type') == 'rows')byRow <- T
  indexW <- attr(sampleW,'index')
  
  notCorCols <- c(1:S)
  if(length(corCols) > 0)notCorCols <- notCorCols[-corCols]
  
  ############ 'other' columns
  Q  <- ncol(x)
  sigmaDf  <- nIn - Q + S - 1   
  sg <- diag(.1,S)
  SO <- S
  cat("Reached3")
  
  notOther <- c(1:S)
  sgOther  <- NULL
  if(length(other) > 0){                     
    notOther    <- notOther[!notOther %in% other]
    SO          <- length(notOther)
    sg[other,]  <- sg[,other] <- 0
    sgOther     <- matrix( cbind(other,other),ncol=2 )
    sg[sgOther] <- .1
  }
  if(length(corCols) > 0){
    sg[corCols,corCols] <- 1
  }
  
  ############## prior on beta
  loB <- hiB <- NULL
  beta <- bg <- matrix(0, Q, S)
  rownames(beta) <- colnames(x)
  
  wB <- which(bg == 0, arr.ind=T)
  
  if( BPRIOR ){
    
    loB <- betaPrior$lo
    hiB <- betaPrior$hi
    
    if( ncol(loB) != ncol(y) |
        ncol(hiB) != ncol(y))stop('betaPrior$lo or betaPrior$hi do not match ydata columns')
    
    xrange <- max( abs(range(x)) )              # for intercept
    loB['intercept', loB['intercept',] < -10*xrange] <- -10*xrange
    hiB['intercept', hiB['intercept',] > 10*xrange]  <- 10*xrange
    
    if(length(standRows) > 0){
      sr   <- names(standRows)[ names(standRows) %in% rownames(loB) ]
      btmp <- loB[sr,]
      btmp[ which(btmp < -5) ] <- -5  # standard deviations, x is standardized
      loB[sr, ] <- btmp
      
      sr   <- names(standRows)[ names(standRows) %in% rownames(hiB) ]
      btmp <- hiB[sr,]
      btmp[ btmp > 5 ] <- 5  
      hiB[sr, ] <- btmp
    }
    
    bg <- (loB + hiB)/2
    bg[is.nan(bg)] <- 0
    
    tmp <- .betaPrior(bg, notOther, loB, hiB)
    bg <- tmp$beta; loB <- tmp$loB; hiB <- tmp$hiB
    wB <- tmp$wB;    zB <- tmp$zB
    bg[is.nan(bg) | bg == Inf | bg == -Inf] <- 0
    
    tmp <- .getPattern(bg[,notOther, drop=F], wB)
    Brows <- tmp$rows
    Bpattern <- tmp$pattern
    bg[!is.finite(bg)] <- 0
    zeroBeta <- .factorCoeffs2Zero(factorBeta, snames, betaPrior)  # max zero is missing factor level
  }
  
  zeroRho <- uindex <- NULL
  
  ############### time 
  if( TIME ){
    
    if( !all(typeNames == 'DA') )stop( "gjamTime only implemented for DA (count) data" )
    
    wB <- wL <- wA <- numeric(0)
    mua  <- mub <- mug <- muw <- w*0
    Umat <- Vmat <- Rmat <- Amat <- NULL
    Brows <- Rrows <- Arows <- Bpattern <- Rpattern <- Apattern <- NULL
    
    tmp <- .getTimeIndex(timeList, other, notOther, xdata, x, xl, y, w,
                         termB, termR, termA)
    if(termA){
      Amat   <- tmp$Amat; Apattern <- tmp$Apattern; wA <- tmp$wA; zA = tmp$zA;
      Umat   <- tmp$Umat;   uindex <- tmp$uindex; Arows <- tmp$Arows
      loAmat <- tmp$loAmat; hiAmat <- tmp$hiAmat; aindex <- tmp$aindex
      Unew <- Umat
      if( !is.finite(mean(loAmat[wA])) )stop( 'values in loAmat not finite: check .getTimeIndex' )
    }
    if( termR & LPRIOR ){
      Rmat   <- tmp$Rmat; Rpattern <- tmp$Rpattern;  wL <- tmp$wL; zR = tmp$zR;      
      Vmat   <- tmp$Vmat;    Rrows <- tmp$Rrows;  gindex <- tmp$gindex
      loRmat <- tmp$loRmat; hiRmat <- tmp$hiRmat
      zeroRho <- .factorCoeffs2Zero(factorRho, snames, rhoPrior)
      timeList$rhoPrior$hi[zeroRho] <- rhoPrior$hi[zeroRho] <- 0
      Vnew <- Vmat
      
      standMatSdRmat <- Rmat*0
      notStandardRmat <- numeric(0)
      
      if(length(standRowsL) > 0){
        csl <- paste('_',names(standRowsL),sep='')
        for(j in 1:length(csl)){
          wj <- grep(csl[j],rownames(Rmat))
          standMatSdRmat[wj,] <- standMatSdL[standRowsL[j],]
          notStandardRmat <- c(notStandardRmat,wj)
        }
      }
    }
    if(termB){
      Brows  <- tmp$Brows; bg <- tmp$bg; Bpattern <- tmp$Bpattern
      wB     <- tmp$wB; zB <- tmp$zB; loB <- tmp$loB; hiB <- tmp$hiB
      if(BPRIOR)timeList$betaPrior$hi[zeroBeta] <- betaPrior$hi[zeroBeta] <- 0     
    }
    
    timeZero <- tmp$timeZero; timeLast <- tmp$timeLast
    maxTime  <- tmp$maxTime; inSamples <- tmp$inSamples 
    tindex   <- tmp$tindex; sindex <- tmp$sindex; i1 <- tmp$i1; i2 <- tmp$i2
    
    if(!REDUCT ){
      if(length(wA) > 300)modelList$reductList <-  list(N = 8, r = 5)
    }
    
    bigx <- numeric(0)
    if(termB){
      bigx <- x[drop=F, tindex[,2], xnames]
      nb <- nrow(bg)
    }
    if(termR){
      bigx <- cbind(bigx, Vmat[tindex[,2],])
      nr <- nrow(Rmat)
    }
    if(termA){
      bigx <- cbind(bigx, Umat[tindex[,2],])
      na <- nrow(Amat)
    }
    
    bigc <- crossprod(bigx)
    diag(bigc) <- diag(bigc)*1.00001
    bigi <- try( solveRcpp(bigc), silent = TRUE )
    
    if(inherits(bigi,'try-error'))bigi <- ginv(bigc)
    
    Y <- w[tindex[,2],notOther] - w[tindex[,1],notOther]
    init <- bigi%*%crossprod(bigx, Y)
    
    if( termB ){
      binit <- init[1:nb,]
      init  <- init[-c(1:nb),]
      binit[binit < loB] <- loB[binit < loB]
      binit[binit > hiB] <- hiB[binit > hiB]
      
      ones <- binit*0 
      ones[ wB ] <- 1
      bg[ 1:length(bg) ] <- as.vector( binit*ones )
    }
    if(termR){
      rinit <- init[1:nr,]
      init  <- init[-c(1:nr),]
      loR   <- loRmat
      hiR   <- hiRmat
      loR[ !is.finite(loR) ] <- 0
      hiR[ !is.finite(hiR) ] <- 0
      
      rinit[rinit < loR] <- ( loR[rinit < loR] + hiR[rinit < loR] )/2
      rinit[rinit > hiR] <- ( loR[rinit > hiR] + hiR[rinit > hiR] )/2
      
      ones <- rinit*0 
      ones[ wL ] <- 1
      Rmat[1:length(Rmat)] <- as.vector( rinit*ones )
      colnames(Rmat) <- snames
    }
    if(termA){
      ainit <- init
      loA   <- loAmat
      loA[ !is.finite(loA) ] <- 0
      hiA <- hiAmat
      hiA[ !is.finite(hiA) ] <- 0
      
      
      ainit[ainit < loA] <- (loA[ainit < loA] + hiA[ainit < loA])/2
      ainit[ainit > hiA] <- (loA[ainit > hiA] + hiA[ainit > hiA])/2
      
      ones <- ainit*0 
      ones[ wA ] <- 1
      Amat[1:length(Amat)] <- as.vector( ainit*ones )
      colnames(Amat) <- snames
    }
    
    rm(bigx, bigc, bigi, init)
  } 
  cat("Reached4")
  
  reductList <- .setupReduct(modelList, S, Q, n) 
  
  if( is.null(reductList) ){
    REDUCT <- FALSE
  }else{
    N <- reductList$N; r <- reductList$r
    REDUCT <- T
  }
  # if( TIME )REDUCT <- FALSE
  
  if(byCol){
    inw <- intersect( colnames(y)[indexW], colnames(y)[notOther] )
    indexW <- match(inw,colnames(y)[notOther])
  }
  
  updateBeta <- .betaWrapper(REDUCT, TIME, notOther, betaLim=max(wmax)/2)
  
  ############ dimension reduction
  cat("Reached5.0")
  
  inSamp <- inSamples
  
  .param.fn <- .paramWrapper(REDUCT, inSamp, SS=length(notOther))
  sigmaerror <- .1
  otherpar   <- list(S = S, Q = Q, sigmaerror = sigmaerror, 
                     Z = NA, K =rep(1,S), sigmaDf = sigmaDf)
  sigErrGibbs <- rndEff <- NULL
  
  yp <- y
  wmax <- ymax <- apply(y,2,max)
  wmax <- wmax/effMat
  
  if(REDUCT){
    cat( paste('\nNote: Dimension reduced from',S,'X',S,'to',N,'X',r,'in Sigma\n') )
    otherpar$N <- N; otherpar$r <- r; otherpar$sigmaerror <- 0.1
    
    otherpar$Z <- rmvnormRcpp(N, rep(0,r), 1/S*diag(r))
    otherpar$D <- .riwish(df = (2 + r + N), 
                          S = (crossprod(otherpar$Z) +
                                 2*2*diag(rgamma(r,shape=1,rate=0.001))))
    otherpar$K <- sample(1:N,length(notOther),replace=T)
    
    otherpar$alpha.DP <- alpha.DP
    otherpar$pvec     <- .sampleP(N=N, avec=rep(alpha.DP/N,(N-1)),
                                  bvec=((N-1):1)*alpha.DP/N, K=otherpar$K)
    kgibbs <- matrix(1,ng,S)
    sgibbs <- matrix(0,ng, N*r)
    nnames <- paste('N',1:N,sep='-')
    rnames <- paste('r',1:r,sep='-')
    colnames(sgibbs) <- .multivarChainNames(nnames,rnames)
    sigErrGibbs <- rep(0,ng)   
    
    rndEff <- w*0
    
  } else {
    Kindex <- which(as.vector(lower.tri(diag(S),diag=T)))
    nK     <- length(Kindex)
    sgibbs <- matrix(0,ng,nK)
    colnames(sgibbs) <- .multivarChainNames(snames,snames)[Kindex] # half matrix
    
    rndEff <- 0
  }
  cat("Reached5.1")
  
  out <- .param.fn( x[,xnames,drop=F], beta = bg[,notOther,drop=F], 
                    Y = w[,notOther], otherpar )  
  sg[notOther,notOther]    <- out$sg
  sinv <- solveRcpp(sg[notOther,notOther])
  otherpar      <- out$otherpar
  
  muw <- w
  
  if( !TIME ){
    
    Y   <- w[inSamp,notOther]
    sig <- sg[notOther,notOther]
    
    if(REDUCT){
      Y   <- Y - rndEff[inSamp,notOther]
      sig <- sigmaerror
    }
    
    bg[,notOther] <- updateBeta(X = x[inSamp,], Y, sig, 
                                beta = bg[,notOther], BPRIOR, loB, hiB)
    muw <- x%*%bg
    sg[other,] <- sg[,other] <- 0
    diag(sg)[other]          <- .1
    
  }else{
    
    mub <- mua <- mug <- 0
    
    xr <- rndEff
    if(termR){
      mug <- Vmat%*%Rmat
      xr  <- xr + mug
    }
    if(termA){
      mua <- Umat%*%Amat
      xr  <- xr + mua
    }
    
    Y <- w
    Y[tindex[,1],] <- Y[tindex[,2],] - w[tindex[,1],] - xr[tindex[,1],]
    ss <- crossprod(Y)/n 
    sg[notOther,notOther] <- .cov2Cor(ss + diag(diag(ss)/2) )
    
    if( REDUCT ){
      sig <- sigmaerror
    }else{ 
      sig <- sg[notOther,notOther] 
    }
    if( termB ){
      
      bg[,notOther] <- updateBeta(X = x[drop=F,tindex[,1],xnames], Y = Y[tindex[,1],notOther], 
                                  sig = sig, beta = bg[,notOther], 
                                  PRIOR = BPRIOR,
                                  lo = loB[,notOther], hi = hiB[,notOther], 
                                  rows=Brows, pattern=Bpattern, sinv = sinv, wF = wB)
      mub <- x[,xnames]%*%bg
    }
    colnames(bg) <- snames
    muw <- mub + mug + mua 
    wpropTime <- .001 + .1*abs(w)
  }
  
  rownames(sg) <- colnames(sg) <- snames
  rownames(bg) <- xnames
  
  ############ ordinal data
  cat("Reached5.2")
  
  cutg <- tg <- numeric(0)
  
  if('OC' %in% typeCode){
    
    tg       <- cutg <- cuts
    cnames   <- paste('C',1:ncut,sep='-')
    nor      <- length(ordCols)
    cgibbs   <- matrix(0,ng,(ncut-3)*nor)
    colnames(cgibbs) <- as.vector( outer(snames[ordCols],
                                         cnames[-c(1,2,ncut)],paste,sep='_') )
    tmp   <- .gjamGetCuts(y+1,ordCols)
    cutLo <- tmp$cutLo
    cutHi <- tmp$cutHi
    plo[,ordCols] <- tg[cutLo]                                        
    phi[,ordCols] <- tg[cutHi]
    lastOrd <- ncol(tg)
  }
  
  ############ setup w
  tmp <- .gjamGetTypes(typeNames)
  typeFull <- tmp$typeFull
  typeCols <- tmp$typeCols
  allTypes <- unique(typeCols)
  Y <- w
  
  LOHI <- F
  if(!LOHI & holdoutN > 0){
    minlo <- apply(plo,2,min)
    minlo[minlo > 0] <- 0
    maxhi <- apply(phi,2,max)
  }
  
  if( 'random' %in% names(modelList)) RANDOM <- TRUE
  
  if(!TIME){
    
    .updateW <- .wWrapper(REDUCT, RANDOM, S, effMat, corCols, notCorCols, typeNames, 
                          typeFull, typeCols, 
                          allTypes, holdoutN, holdoutIndex, censor, 
                          censorCA, censorDA, censorCON, notOther, sampleW, 
                          byRow, byCol,
                          indexW, ploHold, phiHold, sampleWhold, inSamp)
  }else{
    
    .updateW <- .wWrapperTime(sampleW, y, timeZero, timeLast, i1, i2, tindex, gindex,
                              uindex, notOther, n, S, REDUCT, RANDOM, TIME,
                              termB, termR, termA, corCols)
    Y <- w #other needed in .xpredSetup
    Y[ tindex[,1],] <- Y[ tindex[,2],] - w[ tindex[,1],]
    
    if(termA) Y <- Y - mua
    if(termR) Y <- Y -  mug
    if(RANDOM)Y <- Y - rndEff
  }
  
  ycount <- rowSums(y)
  if('CC' %in% typeCode)ycount <- rowSums(y[,compCols])
  
  ############ X prediction
  cat("Reached5.3")
  
  bgg <- bg
  if( TIME & termR){
    tmp <- colnames( model.matrix(formula, xdata) )
    bgg <- matrix( 0, length(tmp), S)
    rownames(bgg) <- tmp
    colnames(bgg) <- snames
    rownames(bgg)[1] <- 'intercept'
  }
  
  tmp <- .xpredSetup(Y, x, bgg, isNonLinX = interBeta$isNonLinX, 
                     factorObject = factorBeta, 
                     intMat = factorBeta$intMat, 
                     standMatSd = standMatSd, standMatMu = standMatMu, 
                     notOther, notStandard ) 
  factorBeta$linFactor <- tmp$linFactor; xpred <- tmp$xpred; px <- tmp$px
  lox <- tmp$lox; hix <- tmp$hix
  propx <- tmp$propx
  
  priorXIV  <- diag(1e-5,ncol(x))
  priorX    <- colMeans(x)
  priorX[abs(priorX) < 1e-10] <- 0
  
  linFactor <- NULL
  
  ################## random groups
  
  if( RANDOM ){
    
    rname  <- modelList$random
    randGroupTab <- table( as.character(xdata[,rname]) )
    
    wss <- names(randGroupTab[randGroupTab <= 2])
    if(length(wss) > 0){
      toConsole( 'Note: one or more random groups with one observations', verbose = verbose )
    }
    
    randGroups <- names( randGroupTab )
    G <- length(randGroups)
    
    groupIndex  <- match(as.character(xdata[,rname]),randGroups)
    rmm <- matrix(groupIndex,length(groupIndex), S)
    smm <- matrix(1:S, length(groupIndex), S, byrow=T)
    
    randGroupIndex <- cbind( as.vector(smm), as.vector(rmm) )
    colnames(randGroupIndex) <- c('species','group')
    xdata[,rname] <- as.factor(xdata[,rname])
    alphaRandGroup <- matrix(0, S, G)
    rownames(alphaRandGroup) <- snames
    colnames(alphaRandGroup) <- randGroups
    Cmat <- var(w[,notOther]/2)
    Cmat <- Cmat + diag(.1*diag(Cmat))
    Cprior <- Cmat
    CImat <- solveRcpp(Cprior)
    Ckeep <- diag(S)
    
    alphaRanSums <- alphaRanSums2 <- alphaRandGroup*0
    groupRandEff <- w*0
    
    Aindex <- which(as.vector(lower.tri(diag(S),diag=T)))
    nK     <- length(Aindex)
    alphaVarGibbs <- matrix(0,ng,nK)
    colnames(alphaVarGibbs) <- .multivarChainNames(snames,snames)[Aindex] # half matrix
    
    toConsole( 'Note: Random groups', paste0( randGroups, collapse = ', '), verbose = verbose )
  }
  
  ################################## XL prediction: variables in both beta and rho
  cat("Reached5.4")
  
  Qall <- Q - 1
  
  # all terms
  linFactor <- numeric(0)
  lf <- factorBeta$linFactor
  xnAll <- unique( c(xnames, xlnames) )
  if( length(lf) > 0 ){
    for(k in 1:length(lf)){
      kf <- match(xnAll[lf[[k]]],colnames(xpred))
      linFactor <- append(linFactor,list(kf))
      names(linFactor)[length(linFactor)] <- names(factorBeta$linFactor)[k]
    }
  }
  
  if( termB & TIME ){
    
    tmp <- .xpredSetup(Y, x, bg, isNonLinX = interBeta$isNonLinX, 
                       factorObject = factorBeta, 
                       intMat = factorBeta$intMat, 
                       standMatSd = standMatSdB, standMatMu = standMatMuB, 
                       notOther, notStandardB ) 
    factorBeta$linFactor <- tmp$linFactor
    
    linFactorBeta <- numeric(0)
    lf <- factorBeta$linFactor
    if( length(lf) > 0 ){
      for(k in 1:length(lf)){
        kf <- match(xnames[lf[[k]]],colnames(xpred))
        linFactorBeta <- append(linFactorBeta,list(kf))
        names(linFactorBeta)[length(linFactorBeta)] <- names(factorBeta$linFactor)[k]
      }
    }
  }
  
  if( termR ){ 
    
    rho[ rho %in% c(-Inf, Inf) ] <- 0
    
    tmp <- .xpredSetup(Y, xx = xl, bgg = rho, 
                       isNonLinX = interRho$isNonLinX, 
                       factorObject = factorRho, 
                       intMat = interRho$intMat, 
                       standMatSd = standMatSdL, 
                       standMatMu = standMatMuL, 
                       notOther, notStandard = notStandardL ) 
    factorRho$linFactor <- tmp$linFactor
    
    linFactorRho <- numeric(0)
    lf <- factorRho$linFactor
    if( length(lf) > 0 ){
      for(k in 1:length(lf)){
        kf <- match(xlnames[lf[[k]]],colnames(xpred))
        linFactorRho <- append(linFactorRho,list(kf))
        names(linFactorRho)[length(linFactorRho)] <- names(factorRho$linFactor)[k]
      }
    }
  }
  
  ############  contrasts, predict F matrix
  cat("Reached6")
  
  if( termB ){
    tmp <- .setupFactors(xdata, xnames, factorBeta)
    ff  <- factorBeta[names(factorBeta) != 'factorList']
    factorBeta <- append(ff,tmp)
  }
  if( termR ){
    tmp <- .setupFactors(xdata, xlnames, factorRho)
    ff  <- factorRho[names(factorRho) != 'factorList']
    factorRho <- append(ff,tmp)
  }
  
  ############ sp richness
  richness <- richFull <- NULL
  RICHNESS <- F
  
  inRichness <- which(!typeNames %in% c('CON','CAT','OC'))
  inRichness <- inRichness[!inRichness %in% other]
  if(length(inRichness) > 2)RICHNESS  <- T
  
  wrich <- y*0 
  wrich[,inRichness] <- 1
  wrich[ymiss] <- 0
  
  ############ E matrix
  ess <- matrix(0,S,S)
  colnames(ess) <- rownames(ess) <- snames
  esens1 <- esens2 <- rep(0, S)
  
  fmat <- factorBeta$fmat
  fnames  <- rownames( factorBeta$lCont )                # variable names for centering
  
  if( termB & !TIME ){   # factors in all
    
    facNames <- names(factorBeta$factorList)
    fl <- character(0)
    for(k in 1:length(facNames)){
      kk <- which( startsWith( rownames(factorBeta$lCont), facNames[k] ) )
      fc <- rownames( factorBeta$lCont )[kk]
      fl <- c(fl, fc)
    }
  }
  
  if( termB & TIME ){   # factors in all
    
    fmat <- factorBeta$fmat
    fnames  <- rownames( factorBeta$lCont )  
    
    facNames <- names(factorBeta$factorList)
    if(!is.null(facNames)){
      fl <- character(0)
      for(k in 1:length(facNames)){
        
        kk <- which( startsWith( rownames(factorBeta$lCont), facNames[k] ) )
        fc <- rownames( factorBeta$lCont )[kk]
        fl <- c(fl, fc)
      }
      attr(bg, 'factors') <- facNames
      attr(bg, 'factorLevels') <- fl
    }
  }
  if( termR ){   # factors in all
    
    facNames <- names(factorRho$factorList)
    if(!is.null(facNames)){
      fl <- character(0)
      for(k in 1:length(facNames)){
        
        kk <- which( startsWith( rownames(factorRho$lCont), facNames[k] ) )
        fc <- rownames( factorRho$lCont )[kk]
        fl <- c(fl, fc)
      }
      attr(Rmat, 'factors') <- facNames
      attr(Rmat, 'factorLevels') <- fl
      attr(Rmat, 'formula') <- formulaRho
      factorRho$LCONT <- rep(TRUE, factorRho$nfact)
      flnames <- rownames( factorRho$lCont )
    }
    
    essL <- ess*0
    lsens1 <- lsens2 <- rep(0, S)
    RmatU <- Rmat
  }
  if( termA ){
    essA <- ess*0
    asens1 <- asens2 <- rep(0, S)
  }
  
  presence <- w*0
  
  covx <- cov(x)
  
  ############ sums
  xpred[,1] <- 1
  predx  <- predx2 <- xpred*0
  yerror <- ypred  <- ypred2 <- wpred  <- wpred2 <- ymissPred <- ymissPred2 <- y*0
  sumDev <- 0   #for DIC
  sMean  <- sg*0
  ntot   <- 0
  
  if(nmiss > 0){
    xmissSum <- xmissSum2 <- rep(0,nmiss)
  }
  
  predxl <- numeric(0)
  if(TIME & termR)predxl <- predxl2 <- xl*0
  
  ############################## gibbs chains
  
  q2 <- length(fnames)
  
  if(TRAITS){
    
    specTrait <- specByTrait[colnames(y),]
    tnames    <- colnames(specTrait)
    M         <- ncol(specTrait)
    specTrait <- t(specTrait)
    
    tpred  <- tpred2 <- matrix(0,n,M)
    
    missTrait <- which(is.na(specTrait),arr.ind=T)
    if(length(missTrait) > 0){
      traitMeans <- rowMeans(specTrait,na.rm=T)
      specTrait[missTrait] <- traitMeans[missTrait[,2]]
      warning( paste('no. missing trait values:',nrow(missTrait)) )
    }
    
    bTraitGibbs <- matrix(0,ng,M*Q)
    colnames(bTraitGibbs) <- .multivarChainNames(xnames,tnames)
    bTraitUnstGibbs <- bTraitGibbs
    
    bTraitFacGibbs <- matrix(0,ng,q2*M)
    colnames(bTraitFacGibbs) <- .multivarChainNames(fnames,tnames)
    
    mgibbs <- matrix(0,ng,M*M)
    colnames(mgibbs) <- .multivarChainNames(tnames,tnames)
  }
  
  if( termB ){
    
    bgibbsUn <- NULL
    
    bf <- .multivarChainNames(xnames, snames, keep = wB)
    
    bgibbs <- matrix(0, ng, length(bf) )
    colnames(bgibbs) <- bf
    if( length(standRows) > 0 ){
      bgibbsUn <- bgibbs                   # unstandardized
    }
    
    fbnames <- colnames( factorBeta$dCont )
    if(length(fbnames) > 0){
      bf <- .multivarChainNames( fbnames, snames[notOther] )
      bFacGibbs <- matrix(0, ng, length(bf))
      colnames(bFacGibbs) <- bf
      fSensGibbs <- matrix( 0, ng, length(fbnames) )
      colnames(fSensGibbs) <- fbnames
      covE <- cov( x[,xnames,drop=F]%*%factorBeta$dCont[xnames,fbnames] )  # note: x is standardized
    }
  }
  
  if( TIME ){
    
    yy <- y*0
    yy[rowInserts,] <- 1
    ymiss <- which(yy == 1, arr.ind=T)
    rm(yy)
    mmiss <- length(ymiss)
    
    if(termR){
      
      covL <- cov( xl%*%factorRho$dCont )  # note x is standardized
      nL  <- nrow(wL)
      
      lgibbs <- matrix(0, ng, nL)
      colnames(lgibbs) <- rownames(wL)
      lgibbsUn <- lgibbs                 # unstandardized
      spL <- rep(.01, nL)
    }
    
    if(termA){
      
      nA     <- nrow(wA)
      wnames <- apply(wA, 1, paste0, collapse='-')  #locations in Amat, not alpha
      alphaGibbs <- matrix(0, ng, nA)
      colnames(alphaGibbs) <- wnames
      spA <- rep(.001, nA)
    }
    
    ni <- length(i1)
    g1 <- 1
    gcheck <- c(50, 100, 200, 400, 800)
    tinyg <- 1e-6
  }
  
  pbar <- txtProgressBar(min=1,max=ng,style=1)
  
  form <- formula
  if( !is.null(formulaBeta) )form <- formulaBeta
  tmp <- .getUnstandX(form, x[,xnames, drop=F], xdata, standRows )
  S2U      <- tmp$S2U                    # S2U%*%*bg
  xUnstand <- tmp$xu
  
  
  if( Q == 1 )PREDICTX <- FALSE
  
  if(TIME & termB){
    tmp <- .getUnstandX(formulaBeta, x[,xnames, drop=F], xdata, standRowsB )
    S2U <- tmp$S2U
  }
  
  if(termR){
    facNamesRho <- attr(Rmat, 'factors') 
    #  xf  <- NULL
    #  if(length(facNames) > 0){
    #    xf <- xdata[, facNamesRho, drop=F]
    #  }
    
    tmp <- .getUnstandX( formulaRho, xl, xdata, standRowsL )
    S2UL      <- tmp$S2U
    xlUnstand <- tmp$xu
    missR <- which( is.na(xlUnstand), arr.ind=T )
    missXR <- unique(missR[,1])
    if(length(missR) > 0){
      xlmu <- colMeans(xlUnstand, na.rm=T)
      xlUnstand[missR] <- xlmu[ missR[,2] ]
    }
  }
  
  if(REDUCT)rndTot <- rndTot2 <- w*0 
  notPA <- which(!typeNames == 'PA' & !typeNames == 'CON')
  
  if(length(y) < 10000 | FULL) FULL <- T
  
  if(FULL){
    ygibbs <- matrix(0,ng,length(y))
  }
  if(RICHNESS){
    ypredPres <- ypredPres2 <- ypredPresN <- y*0
    shannon   <- rep(0,n)
  }
  
  varExp <- varTot <- varExpMean <- varExpRand <- rep( 0, length(notOther) )
  
  sdg <- .1
  
  
  
  for(g in 1:ng){ ########################################################
    
    if( REDUCT ){
      
      Y  <- w[,notOther]
      
      if(TIME){
        Y[tindex[,1],] <- w[tindex[,2],notOther] - w[tindex[,1],notOther]   # delta w here
        Y <- Y - mua[,notOther] - mug[,notOther] 
      }
      if(RANDOM)Y <- Y - groupRandEff[,notOther] 
      
      tmp <- .param.fn(X = x[drop=F,,xnames], beta = bg[drop=F,,notOther], Y = Y, otherpar) 
      sg[notOther,notOther] <- tmp$sg
      otherpar              <- tmp$otherpar
      rndEff[inSamples,notOther] <- tmp$rndEff
      sigmaerror          <- otherpar$sigmaerror
      kgibbs[g,notOther]  <- otherpar$K
      sgibbs[g,]          <- as.vector(otherpar$Z)
      sigErrGibbs[g]      <- sigmaerror
      sg[sgOther]         <- .1*sigmaerror
      
      sinv <- .invertSigma(sg[notOther,notOther],sigmaerror,otherpar,REDUCT)
      sdg  <- sqrt(sigmaerror)
      
      if( !TIME ){
        
        Y <- w[inSamp,notOther] - rndEff[inSamp,notOther]
        if(RANDOM)Y <- Y - groupRandEff[inSamp,notOther]
        bg[,notOther] <- updateBeta(X = x[inSamp,xnames], Y, 
                                    sig = sigmaerror, beta = bg[,notOther],
                                    PRIOR = BPRIOR, lo=loB[,notOther], hi=hiB[,notOther])
        muw[inSamp,] <- x[inSamp,]%*%bg
        
      } else {  # REDUCT and TIME
        
        mua <- mug <- w*0
        if(termA)mua  <- Umat%*%Amat 
        if(termR)mug  <- Vmat%*%Rmat
        
        Y[tindex[,1],] <- w[tindex[,2],notOther] - w[tindex[,1],notOther]   # delta w here
        
        if( termB ){
          
          if( termR )Y <- Y - mug[,notOther] 
          if( termA )Y <- Y - mua[,notOther] 
          if(RANDOM) Y <- Y - groupRandEff[,notOther]
          Y <- Y - rndEff[,notOther]
          
          bg[,notOther] <- updateBeta( X = x[drop=F, tindex[,1],xnames], Y = Y[tindex[,1],], 
                                       sig = sigmaerror, beta = bg[,notOther],
                                       PRIOR = BPRIOR,
                                       rows = Brows, pattern = Bpattern,
                                       lo=loB[,notOther], hi=hiB[,notOther],
                                       wF = wB )
          mub <- x[,xnames]%*%bg
        }else{
          mub <- 0
        }
        
        if( termR ){
          
          Y[tindex[,1],] <- w[tindex[,2],notOther] - w[tindex[,1],notOther]   # delta w here
          if( termB )Y <- Y - mub[,notOther] 
          if( termA )Y <- Y - mua[,notOther] 
          if(RANDOM) Y <- Y - groupRandEff[,notOther]
          Y <- Y - rndEff[,notOther]
          
          Rmat[,notOther] <- updateBeta( X = Vmat[tindex[,1],], 
                                         Y = Y[tindex[,1],notOther], sig=sigmaerror, 
                                         beta = Rmat[,notOther], PRIOR = LPRIOR,
                                         rows = Rrows, pattern = Rpattern, 
                                         lo = loRmat, hi = hiRmat, wF = wL )
          mug  <- Vmat%*%Rmat
        }else{
          mug <- 0
        }
        
        if( termA ){
          
          Y[tindex[,1],] <- w[tindex[,2],notOther] - w[tindex[,1],notOther]   # delta w here
          if( termB )Y <- Y - mub[,notOther] 
          if( termR )Y <- Y - mug[,notOther] 
          if(RANDOM) Y <- Y - groupRandEff[,notOther]
          Y <- Y - rndEff[,notOther]         
          
          Amat[,notOther] <- updateBeta(X = Umat[tindex[,1],], Y = Y[tindex[,1],], sig = sigmaerror, 
                                        rows = Arows, pattern = Apattern, 
                                        beta = Amat[,notOther], PRIOR = TRUE, 
                                        lo = loAmat[,notOther], hi = hiAmat[,notOther], wF = wA )
          mua <- Umat%*%Amat
        }else{
          mua <- 0
        }
        muw <- mub + mug + mua + rndEff
      }
      
    } else { 
      
      # !REDUCT 
      
      if( !TIME ){
        
        Y <- w[inSamp,notOther]
        if(RANDOM)Y <- Y - groupRandEff[inSamp,notOther]
        bg[,notOther] <- updateBeta(X = x[inSamp,], Y, sig = sg[notOther,notOther], 
                                    beta = bg[,notOther], BPRIOR, lo=loB, hi=hiB)
        muw[inSamp,] <- x[inSamp,]%*%bg
        
      }else{ 
        
        # !REDUCT & TIME
        
        muw <- mub <- mua <- mug <- w*0
        
        if( termR )mug  <- Vmat%*%Rmat
        if( termA )mua  <- Umat%*%Amat 
        
        if( termB ){
          
          Y   <- w[,notOther] 
          Y[tindex[,1],] <- w[tindex[,2],] - w[tindex[,1],notOther]
          if(termA) Y <- Y - mua[,notOther] 
          if(termR) Y <- Y - mug[,notOther]
          if(RANDOM)Y <- Y - groupRandEff[,notOther]
          
          bg[,notOther] <- updateBeta(X = x[drop=F, tindex[,1],xnames], Y = Y[tindex[,1],notOther], 
                                      sig = sg[notOther,notOther], beta = bg[,notOther],
                                      PRIOR = BPRIOR,
                                      rows = Brows, pattern = Bpattern,
                                      lo=loB[,notOther], hi=hiB[,notOther], sinv = sinv, wF = wB)
          mub[tindex[,1],] <- x[tindex[,1],xnames]%*%bg
          mub[timeLast,]   <- x[drop=F, timeLast,xnames]%*%bg
          muw <- muw + mub
        }
        
        if( termR ){
          
          Y   <- w[,notOther] 
          Y[tindex[,1],] <- Y[tindex[,2],] - w[tindex[,1],notOther]
          if(termA) Y <- Y - mua[,notOther] 
          if(termB) Y <- Y - mub[,notOther]
          if(RANDOM)Y <- Y - groupRandEff[,notOther]
          
          Rmat[,notOther] <- updateBeta(X = Vmat[tindex[,1],], Y = Y[tindex[,1],], 
                                        sig=sg[notOther,notOther], 
                                        beta = Rmat[,notOther],
                                        PRIOR = LPRIOR,
                                        rows = Rrows, pattern = Rpattern, 
                                        lo = loRmat, hi = hiRmat, sinv = sinv, wF = wL)
          
          #############   diag(Rmat) <- diag(rho)
          
          mug[tindex[,1],] <- Vmat[tindex[,1],]%*%Rmat
          mug[timeLast,]   <- Vmat[drop=F, timeLast,]%*%Rmat
          muw <- muw + mug
        }
        
        
        
        if(termA){
          
          Y   <- w[,notOther] 
          Y[tindex[,1],] <- Y[tindex[,2],] - w[tindex[,1],notOther]
          if(termR) Y <- Y - mug[,notOther] 
          if(termB) Y <- Y - mub[,notOther]
          if(RANDOM)Y <- Y - groupRandEff[,notOther]
          
          Amat[,notOther] <- updateBeta(X = Umat[tindex[,1],], Y = Y[tindex[,1],], 
                                        sig=sg[notOther,notOther], 
                                        beta = Amat[,notOther], PRIOR = TRUE,
                                        lo=loAmat[,notOther], hi=hiAmat[,notOther], 
                                        rows = Arows, pattern = Apattern, sinv = sinv, wF = wA)
          
          #     Amat[wA] <- alpha[aindex]
          
          
          
          mua[tindex[,1],] <- Umat[tindex[,1],]%*%Amat
          mua[timeLast,] <- Umat[drop=F, timeLast,]%*%Amat
          muw[,notOther] <- muw[,notOther] + mua[,notOther]
        }
        
        Y    <- w
        Y[tindex[,1],] <- w[tindex[,2],] - w[tindex[,1],]
        SS   <- crossprod(Y[tindex[,1],notOther] - muw[tindex[,1],notOther])
        SI   <- solveRcpp(SS)
      }
      
      
      if( !TIME ){
        
        # !REDUCT & !TIME, marginalize parameter matrix
        
        Y <- w[inSamp,notOther]
        if(RANDOM)Y <- Y - groupRandEff[inSamp,notOther]
        XIXXX <- x[drop=F, inSamp,]%*%solveRcpp( crossprod(x[drop=F, inSamp,]) )%*%t(x[drop=F, inSamp,] )
        XUV   <- t(Y)%*%XIXXX%*%Y
        YX    <- crossprod(Y) - XUV
        SI    <- solve( YX ) 
      }
      sinv <- .rwish(sigmaDf, SI)
      sg[notOther,notOther] <- solveRcpp(sinv)
      sgibbs[g,] <- sg[Kindex]
    }
    
    if(termB)alphaB <- .sqrtMatrix(bg, sg, DIVIDE=T)
    
    if( 'OC' %in% typeCode ){
      tg <- .updateTheta(w, tg, cutLo, cutHi, ordCols,
                         holdoutN, holdoutIndex, minOrd, maxOrd) # var scale
      if(TIME){
        cutg <- tg
      }else{
        cutg <- .gjamCuts2theta(tg, ss = sg[ordCols,ordCols]) # corr scale
      }
      breakMat[ordCols,1:lastOrd] <- cutg
      cgibbs[g,] <- as.vector( cutg[,-c(1,2,ncut)] )
      
      plo[,ordCols] <- cutg[cutLo]
      phi[,ordCols] <- cutg[cutHi]
    }
    
    if(RANDOM){
      
      cw <- w - muw
      
      if( TIME ){
        cw[tindex[,1],] <- w[tindex[,2],] - w[tindex[,1],] - muw[tindex[,2],]
      }
      
      if(REDUCT){
        cw <- cw - rndEff
        v  <- 1/sigmaerror*.byGJAM(as.vector(cw), randGroupIndex[,1], 
                                   randGroupIndex[,2], alphaRandGroup*0, 
                                   fun='sum')[notOther,]
        sinv <- diag(1/sigmaerror, SO)
      }else{
        v <- .byGJAM(as.vector(cw), randGroupIndex[,1], 
                     randGroupIndex[,2], alphaRandGroup*0, fun='sum')[notOther,]
        v <- sinv%*%v
      }
      
      alphaRandGroup[notOther,] <- randEffRcpp(v, randGroupTab, sinv, CImat)
      if(length(other) > 0)alphaRandGroup[other,] <- 0
      if(g < 10){
        alphaRandGroup[notOther,] <- 
          sweep( alphaRandGroup[notOther,], 2, 
                 colMeans(alphaRandGroup[notOther,]), '-')
      }
      SS  <- crossprod(t(alphaRandGroup[notOther,]))
      SS  <- S*SS + Cmat
      
      testv <- try( chol(SS) ,T)
      if( inherits(testv,'try-error') ){
        tiny  <- .01*diag(SS)
        SS  <- SS + diag(diag(SS + tiny))
      }
      
      Ckeep[notOther,notOther] <- .riwish( df = S*G + 1, SS )
      CImat <- solveRcpp(Ckeep[notOther,notOther])
      
      alphaVarGibbs[g,] <- Ckeep[Aindex]
      groupRandEff <- t(alphaRandGroup)[groupIndex,]
    }
    
    if( TIME ){
      
      # muw does not include rndEff or groupRandEff
      tmp <- .updateW(w, plo, phi, wpropTime, xl, yp, Rmat, Amat, rndEff, groupRandEff,
                      sdg, muw, mub, Umat, Vmat, sinv)
      w <- tmp$w; muw <- tmp$muw; yp <- tmp$yp; Umat <- tmp$Umat; Vmat <- tmp$Vmat
      
      groups <- NULL
      
      for(k in allTypes){
        
        wk <- which(typeCols == k)
        nk <- length(wk)
        wo <- which(wk %in% notOther)
        wu <- which(typeCols[notOther] == k)
        wp <- w[, wk, drop=F]
        yp <- yp[, wk, drop=F]
        yy <- y[,wk,drop=F]
        
        if(typeFull[wk[1]] == 'countComp')groups <- CCgroups
        if(typeFull[wk[1]] == 'fracComp')groups  <- FCgroups
        if(typeFull[wk[1]] == 'categorical')groups <- CATgroups
        
        glist <- list(wo = wo, type = typeFull[wk[1]], yy = yy, 
                      wq = wp, yq = yp, cutg = cutg, 
                      censor = censor, censorCA = censorCA, 
                      censorDA = censorDA, censorCON  = censorCON, 
                      eff = effMat[,wk,drop=F], groups = groups, 
                      k = k, typeCols = typeCols, notOther = notOther, 
                      wk = wk, sampW = sampleW[,wk])
        tmp <- .gjamWLoopTypes( glist )
        w[,wk]  <- tmp[[1]]
        yp[,wk] <- tmp[[2]]
      }
      
      # predict X: this changes x and Vmat
      
      if( ncol(x) > 1 ){ # do not predict intercept
        
        xtmp <- xpred
        xtmp[,-1] <- .tnorm(n*Qall, -3, 3, xpred[,-1], .1)
        
        # factors
        if( length(linFactor) > 0 ){
          for(k in 1:length(linFactor)){
            mm  <- linFactor[[k]]
            wcol <- sample(mm,n,replace=T)
            xtmp[,mm[-1]] <- 0
            xtmp[ cbind(1:n, wcol) ] <- 1
          }
        }
        
        if(length(intMat) > 0){     #  interactions
          xtmp[,intMat[,1]] <- xtmp[,intMat[,2]]*xtmp[,intMat[,3]]
        }
        
        muNow <- muNew <- w*0 + rndEff
        
        if(termB){
          muNow[,notOther]  <- muNow[,notOther] + xpred[drop=F, , xnames]%*%bg[,notOther,drop=F]
          muNew[,notOther]  <- muNew[,notOther] + xtmp[drop=F, , xnames]%*%bg[,notOther,drop=F]
        }
        if(termA){
          mua   <- Umat%*%Amat
          muNow[,notOther] <- muNow[,notOther] + mua[,notOther]
          muNew[,notOther] <- muNew[,notOther] + mua[,notOther]
        }
        if(termR){  # note: Vmat holds w[t-1,]*x[t,]
          ww <- w
          ww[ww < 0] <- 0
          Vnow <- Vnew <- Vmat
          Vnow[tindex[,1],]  <- ww[drop = FALSE,tindex[,1],gindex[,'colW']]*
            xpred[drop = FALSE,tindex[,1],xlnames][drop = FALSE,,gindex[,'rowG']]
          mugNow <- Vnow%*%Rmat
          muNow[,notOther]  <- muNow[,notOther] + mugNow[,notOther]
          
          Vnew[tindex[,1],] <- ww[drop = FALSE,tindex[,1],gindex[,'colW']]*
            xtmp[drop = FALSE,tindex[,1],xlnames][drop = FALSE,,gindex[,'rowG']]
          mugNew <- Vnew%*%Rmat
          muNew[,notOther]  <- muNew[,notOther] + mugNew[,notOther]
        }
        
        ww <- w
        ww[tindex[,1],] <- ww[tindex[,2],] - ww[tindex[,1],]
        ww[timeZero,] <- ww[timeZero+1,]
        ww[timeLast,] <- ww[timeLast-1,]
        
        if(REDUCT){
          pnow <- dnorm(ww[,notOther],muNow[,notOther],sdg,log=T)
          pnew <- dnorm(ww[,notOther],muNew[,notOther],sdg,log=T)
          a1   <- exp( rowSums(pnew - pnow) )
        }else{
          pnow <- .dMVN(ww[,notOther],muNow[,notOther],smat=sg,log=T) 
          pnew <- .dMVN(ww[,notOther],muNew[,notOther],smat=sg,log=T) 
          a1   <- exp(pnew - pnow)
        }
        z    <- runif( length(a1), 0, 1 )
        za   <- which(z < a1)
        if( length(za) > 0 ){
          xpred[za,] <- xtmp[za,]
        }
        if( termR ){
          if(nlmiss > 0)xl[xlmiss] <- xpred[,colnames(xl)][xlmiss]
        }
        if( nmiss > 0 ){
          
          x[xmiss] <- xpred[xmiss]
          
          #     xf  <- NULL
          #     if( length(facNames) > 0 ){
          #       xf <- xdata[drop=F,, facNames]
          #     }
          #     tmp    <- .getUnstandX(formula, x[drop=F,,xnames], xdata, standRows )            
          #     S2U    <- tmp$S2U
          #     XX     <- crossprod(x)
          #     IXX    <- solveRcpp(XX)
        }
      }
      
    }else{ ############# not TIME
      
      # ww <- x%*%bg + rmvnormRcpp(nrow(w), rep(0, S), sg)
      
      tmp   <- .updateW( rows=1:nrow(x), x, w, y, bg, sg, alpha=alphaB, 
                         cutg, plo, phi, rndEff, groupRandEff, 
                         sigmaerror, wHold )
      w     <- tmp$w
      yp    <- tmp$yp
      #   plo   <- tmp$plo
      #   phi   <- tmp$phi
      wHold <- tmp$wHold    #values for w if not held out
      
      Y <- w[,notOther]
      if(holdoutN > 0) Y[holdoutIndex,] <- wHold[,notOther]  # if w not held out
      if(RANDOM)Y <- Y - groupRandEff[,notOther]
      
      if(nmiss > 0){
        
        x[xmiss] <- .imputX_MVN(x,Y,bg[,notOther],xmiss,sinv,xprior=xprior,
                                xbound=xbound)[xmiss]
        if( length(standRows) > 0 ){
          
          #     xf  <- NULL
          #     if(length(facNames) > 0){
          #       xf <- xdata[, facNames, drop=F]
          #     }
          #     tmp    <- .getUnstandX( formula, x, xdata, standRows )            
          #     S2U    <- tmp$S2U
          #     XX     <- crossprod(x)
          #     IXX    <- solveRcpp(XX)
        }
      }
      
      if( PREDICTX & length(predXcols) > 0 ){
        
        if( length(interBeta$isNonLinX) > 0 ){
          
          xpred <- .predictY2X_nonLinear(xpred, yy=Y, bb=bg[,notOther],
                                         ss=sg[notOther,notOther],
                                         priorIV = priorXIV, priorX=priorX,
                                         factorObject = factorBeta, interObject = interBeta,
                                         lox, hix, propx)$x   
        }
        
        if( length(px) > 0 ){
          
          wn <- which(!is.finite(xpred),arr.ind=T)
          if(length(wn) > 0){
            tmp <- matrix(priorX,Q,nrow(wn))
            xpred[wn[,1],] <- t(tmp)
          }
          xpred[,px] <- .predictY2X_linear(xpred, yy=Y, bb=bg[,notOther],
                                           ss=sg[notOther,notOther], sinv = sinv,
                                           priorIV = priorXIV, 
                                           priorX=priorX, predCols=px, 
                                           REDUCT=REDUCT, lox, hix, propx)[,px]
          wn <- which(!is.finite(xpred),arr.ind=T)
          if(length(wn) > 0){
            tmp <- matrix(priorX,Q,nrow(wn))
            xpred[wn[,1],] <- t(tmp)
          }
        }
        
        if( length(factorBeta$linFactor) > 0 ){
          
          # predict all factors
          xtmp <- xpred
          xtmp[,factorBeta$findex] <- 
            .predictY2X_linear(xpred, yy=Y, 
                               bb=bg[,notOther],
                               ss=sg[notOther,notOther], sinv = sinv,
                               priorIV = priorXIV, 
                               priorX=priorX,predCols=factorBeta$findex, 
                               REDUCT=REDUCT, lox, hix, propx)[,factorBeta$findex]
          for(k in 1:length(factorBeta$linFactor)){
            
            mm  <- factorBeta$linFactor[[k]]
            tmp <- xtmp[,mm]
            
            tmp[,1] <- 0
            ix  <- apply(tmp,1,which.max)   
            
            tmp <- tmp*0
            tmp[ cbind(1:n,ix) ] <- 1
            tmp <- tmp[,-1,drop=F]
            xpred[,mm[-1]] <- tmp
          }
        }
        xpred[,1] <- 1
      }
    }
    
    
    setTxtProgressBar(pbar,g)
    
    if(termR){
      #     rho[ gindex[,c('rowG','colW')] ] <- Rmat[wL]
      lgibbs[g,] <- Rmat[wL]       # standardized
    }
    
    if(termA)alphaGibbs[g,] <- Amat[wA]
    
    if(termB)bgibbs[g,] <- bg[wB] # standardized, except nonStandard columns, 
    
    # unstandardize if there are standardized columns; otherwise bgibbs is unstandardized
    
    if( length(standRows) > 0 ){           # for xUnstand
      if( termB ){
        bgU <- S2U%*%bg
        bgibbsUn[g,] <- bgU[wB]
      }
    }
    
    if( TIME ){
      
      if( termR & length(standRowsL) > 0 ){
        
        if( ncol(xl) > 1 ){
          cat("Reached6.1")
          
          Vunst <- Vmat
          wz    <- w
          wz[wz < 0] <- 0
          Vunst[tindex[,1],]  <- wz[drop = FALSE,tindex[,1],gindex[,'colW']]*
            xlUnstand[drop = FALSE,tindex[,1],xlnames][drop = FALSE,,gindex[,'rowG']]
          cat("Reached6.2")
          
          Y   <- w[,notOther] 
          Y[tindex[,1],] <- Y[tindex[,2],] - w[tindex[,1],]
          if(termA) Y <- Y - mua[,notOther] 
          if(termB) Y <- Y - mub[,notOther]
          if(REDUCT)Y <- Y - rndEff[,notOther]
          if(RANDOM)Y <- Y - groupRandEff[,notOther]
          
          sig <- sigmaerror
          if( !REDUCT ) sig <- sg[notOther,notOther]
          cat("Reached6.2.1")
          
          
          RmatU[,notOther] <- updateBeta( X = Vunst[tindex[,1],], 
                                          Y = Y[tindex[,1],notOther], sig = sig, 
                                          beta = RmatU[,notOther],
                                          PRIOR = LPRIOR,
                                          rows = Rrows, pattern = Rpattern, 
                                          lo = loRmat, hi = hiRmat, sinv = sinv, wF = wL )
          cat("Reached6.2.2")
          
          lgibbsUn[g,] <- RmatU[wL]          # unstandardized
        }
      }
    }
    cat("Reached 6.3\n")
    if( TRAITS ){
      Atrait <- bgU%*%t(specTrait[,colnames(yp)])  # unstandardized
      bTraitUnstGibbs[g,] <- Atrait
      
      Atrait <- bg%*%t(specTrait[,colnames(yp)])  # standardized
      bTraitGibbs[g,] <- Atrait
      
      Strait <- specTrait[,colnames(yp)]%*%sg%*%t(specTrait[,colnames(yp)])
      mgibbs[g,] <- Strait
      
      minv <- ginv(Strait)
      
      tmp <- .contrastCoeff(beta=Atrait, 
                            notStand = notStandard[notStandard %in% xnames], 
                            sigma = Strait, sinv = minv,
                            stand = standMatSd, factorObject=factorBeta )
      tagg   <- tmp$ag
      bTraitFacGibbs[g,] <- tagg # stand for X and W, centered for factors
    }
    
    if( termB & !is.null(fSensGibbs) ){
      
      # Fmatrix centered for factors, 
      # bg is standardized by x, bgu is unstandardized
      
      nst <- notStandard[notStandard %in% xnames]
      stm <- standMatSd
      if(TIME){
        nst <- notStandardB[notStandard %in% xnames]
        stm <- standMatSdB
      }
      
      tmp <- .contrastCoeff(beta=bg[,notOther, drop=F], 
                            notStand = nst, 
                            sigma = sg[notOther,notOther], sinv = sinv,
                            stand = stm, factorObject=factorBeta )
      agg   <- tmp$ag  
      beg   <- tmp$eg
      fsens <- tmp$sens
      fsens[ fsens < 1e-12 ] <- 0
      
      fSensGibbs[g,]  <- sqrt(diag(fsens))[fbnames]
      bFacGibbs[g,] <- agg                 # stand for X and W, centered for factors
    }
    
    if(FULL)ygibbs[g,] <- as.vector(yp)
    
    if(g > burnin){
      
      ntot   <- ntot + 1
      ypred  <- ypred + yp
      ypred2 <- ypred2 + yp^2
      
      wpr  <- matrix( colMeans( w[inSamp, notOther] ), length(inSamp), length(notOther),
                      byrow = T )
      tss  <- diag( crossprod( w[inSamp,notOther] - wpr ) ) # total
      vtot <- diag( var(w[inSamp,notOther]) ) 
      
      if( termB ){
        mpr  <- x[inSamp,xnames]%*%bg[,notOther, drop=F]
        mtot <- diag( var(mpr))
        varExpMean <- varExpMean + mtot/vtot
        rss    <- diag( crossprod( w[inSamp,notOther] - mpr ) ) # residual
        vexp   <- 1 - rss/tss
        varExp <- varExp + vexp
      }
      
      varTot <- varTot + vtot
      
      if(RANDOM){
        itot <- diag(var(groupRandEff[inSamp,notOther])) 
        varExpRand <- varExpRand + itot/vtot
      }
      
      tmp <- .dMVN(w[,notOther], muw[,notOther], sg[notOther,notOther], log=T)
      sumDev <- sumDev - 2*sum(tmp) 
      yerror <- yerror + (yp - y)^2
      
      if( termB & !is.null(fSensGibbs) )fmat <- fmat + fsens
      
      sMean  <- sMean + sg
      
      wpred  <- wpred + w
      wpred2 <- wpred2 + w^2
      
      if(RICHNESS){
        
        yy <- yp
        
        if('PA' %in% typeNames){
          wpa <- which(typeNames[inRichness] == 'PA')
          yy[,inRichness[wpa]] <- round(yp[,inRichness[wpa]]) #######
        }
        
        if(length(notPA) > 0){
          w0 <- which(yy[,notPA] <= 0)
          w1 <- which(yy[,notPA] > 0)
          yy[,notPA][w0] <- 0
          yy[,notPA][w1] <- 1
        }
        
        shan <- sweep(yp[,inRichness], 1, rowSums(yp[,inRichness]), '/')
        shan[shan == 0] <- NA
        shan <- -rowSums(shan*log(shan),na.rm=T)
        shannon <- shannon + shan
        
        wpp <- which(yy > 0)
        ypredPres[wpp]  <- ypredPres[wpp] + yp[wpp]
        ypredPres2[wpp] <- ypredPres2[wpp] + yp[wpp]^2
        ypredPresN[wpp] <- ypredPresN[wpp] + 1
        
        presence[,inRichness] <- presence[,inRichness] + yy[,inRichness]
        ones <- round(rowSums(yy[,inRichness]))
        more <- round(rowSums(yy[,inRichness]*wrich[,inRichness,drop=F]))
        richFull <- .add2matrix(ones,richFull)
        richness <- .add2matrix(more,richness)  # only for non-missing
      }
      
      if(RANDOM){
        alphaRanSums  <- alphaRanSums + alphaRandGroup
        alphaRanSums2 <- alphaRanSums2 + alphaRandGroup^2
      }
      
      if(mmiss > 0){
        ymissPred[ymiss]  <- ymissPred[ymiss] + y[ymiss]
        ymissPred2[ymiss] <- ymissPred2[ymiss] + y[ymiss]^2
      }
      if(nmiss > 0){
        xmissSum  <- xmissSum + x[xmiss]
        xmissSum2 <- xmissSum2 + x[xmiss]^2
      }
      
      if(PREDICTX & length(predXcols) > 0){
        predx  <- predx + xpred
        predx2 <- predx2 + xpred^2
      }
      
      wa0 <- numeric(0)
      
      if( !is.null(fSensGibbs) ){
        if( !TIME | (TIME & termB) ){
          
          ag <- agg
          cx <- t(ag)%*%covE%*%ag 
          ess[notOther,notOther]  <- ess[notOther,notOther] + cx
          
          esd <- sqrt(diag(cx))
          esens1[notOther] <- esens1[notOther] + esd
          esens2[notOther] <- esens2[notOther] + esd^2
        }
      }
      
      if( termR){
        
        if(ncol(xl) > 1 ){
          
          # variables in Vmat are standardized
          covL <- cov(Vmat)
          cp   <- t(Rmat[,notOther,drop=F])%*%covL%*%Rmat[,notOther,drop=F]
          essL[notOther,notOther]  <- essL[notOther,notOther] + cp
          
          lsd <- sqrt(diag(cp))
          lsens1[notOther] <- lsens1[notOther] + lsd
          lsens2[notOther] <- lsens2[notOther] + lsd^2
        }
      }
      
      if(termA){
        
        covw <- cov(Umat)
        ca   <- t(Amat[,notOther])%*%covw%*%Amat[,notOther]
        essA[notOther,notOther]  <- essA[notOther,notOther] + ca
        
        asd <- sqrt(diag(ca))
        asens1[notOther] <- asens1[notOther] + asd
        asens2[notOther] <- asens2[notOther] + asd^2
      }
      
      
      if(REDUCT){
        rndTot  <- rndTot + rndEff
        rndTot2 <- rndTot2 + rndEff^2
      }
      
      if(TRAITS){
        yw     <- sweep(yp,1,rowSums(yp),'/')
        yw[yw <= 0]   <- 0
        yw[is.na(yw)] <- 0
        Ttrait <- .gjamPredictTraits(yw,specTrait[,colnames(yp)], traitTypes)
        tpred  <- tpred + Ttrait
        tpred2 <- tpred2 + Ttrait^2
      }
    } 
    
    
  }
  ################# end gibbs loop ####################
  
  
  # default: all columns standardized for analysis
  #          reported on input scale in betaMu, bgibbsUn
  #          reported on standardized scale in betaStandXmu, bgibbs
  #          reported on standardized X, correlation Y in betaStandXWmu
  #    S2U:  unstandardized beta is S2U%*%bg 
  # if( length(standRows) > 0 ) then there are standardized variables to be 
  #                             unstandardized in bgibbsUn
  
  # notStandard: columns that are are not standardized for the analysis in x
  
  otherpar$S <- S 
  otherpar$Q <- Q
  otherpar$snames <- snames
  otherpar$xnames <- xnames
  cat("Reached7")
  
  presence <- presence/ntot
  
  if(RICHNESS){
    missRows <- sort(unique(ymiss[,1]))
    richNonMiss <- richness/ntot            #only non-missing plots
    yr  <- as.matrix(ydata[,inRichness]) 
    yr[yr > 0] <- 1
    yr <- rowSums(yr,na.rm=T)
    vv  <- matrix(as.numeric(colnames(richNonMiss)),n,
                  ncol(richNonMiss),byrow=T)
    rmu <- rowSums( vv * richNonMiss )/rowSums(richNonMiss)
    
    rsd <- sqrt( rowSums( vv^2 * richNonMiss )/rowSums(richNonMiss) - rmu^2)
    
    vv  <- matrix(as.numeric(colnames(richFull)),n,ncol(richFull),byrow=T)
    rfull <- rowSums( vv * richFull )/rowSums(richFull)
    rfull[missRows] <- NA
    rmu <- rowSums(presence)
    
    shan <- sweep(y[,inRichness], 1, rowSums(y[,inRichness]), '/')
    shan[shan == 0] <- NA
    shanObs <- -rowSums(shan*log(shan),na.rm=T)
    
    richness <- cbind(yr, rmu, rsd, rfull, shanObs, shannon/ntot )
    colnames(richness) <- c('obs','predMu','predSd','predNotMissing',
                            'H_obs', 'H_pred')
    if(TIME)richness[timeZero,] <- NA
    
    ms <- sums2meanSd( ypredPres, ypredPres2, ypredPresN )  # predictive mean and se given presence
    ypredPresMu <- ms$mean
    ypredPresSe <- ms$sd
  }
  
  if('OC' %in% typeNames){
    ordMatShift <- matrix(ordShift,n,length(ordCols),byrow=T)
    onames <- snames[ordCols]
    wb <- match(paste(onames,'intercept',sep='_'), colnames(bgibbs))
    bgibbs[,wb]   <- bgibbs[,wb] + matrix(ordShift,ng,length(ordCols),byrow=T)
    if( !is.null(bgibbsUn)){
      bgibbsUn[,wb] <- bgibbsUn[,wb] + matrix(ordShift,ng,length(ordCols),byrow=T)
    }
    y[,ordCols] <- y[,ordCols] + ordMatShift
  }
  
  if(mmiss > 0){
    ymissPred[ymiss]  <- ymissPred[ymiss]/ntot
    yd <- ymissPred2[ymiss]/ntot - ymissPred[ymiss]^2
    yd[!is.finite(yd)| yd < 0] <- 0
    ymissPred2[ymiss] <- sqrt(yd)
    
    if('OC' %in% typeNames){
      ymissPred[,ordCols] <- ymissPred[,ordCols] + ordMatShift
    }
  }
  
  rmspeBySpec <- sqrt( colSums(yerror)/ntot/n )
  rmspeAll    <- sqrt( sum(yerror)/ntot/n/S )
  
  sMean <- sMean/ntot
  
  varMu <- 0
  if( termB ){
    varExp <- varExp/ntot
    varMu  <- varExpMean/ntot
  }
  varTot <- varTot/ntot
  varRn  <- varExpRand/ntot
  varMod <- varMu + varRn
  
  betaStandXWmu <-  betaStandXWse <- betaStandXWTable <- NULL
  
  if(termB){
    
    betaStandXmu  <- betaStandXse  <- betaStandXTable <- NULL
    betaStandXWmu <- betaStandXWse <- betaStandXWTable <- NULL
    
    tmp <- .chain2tab(bgibbs[burnin:ng,], snames, xnames, wF = wB)
    betaStandXmu <- tmp$mu
    betaStandXse <- tmp$se
    betaStandXTable <- tmp$tab
    
    if( !is.null(bFacGibbs) ){
      tmp <- .chain2tab(bFacGibbs[burnin:ng,], snames[notOther], rownames(agg))
      betaStandXWmu    <- tmp$mu
      betaStandXWse    <- tmp$se
      betaStandXWTable <- tmp$tab
    }
    
    if(!is.null(loB)){
      blo <- as.vector( t(loB) )
      bhi <- as.vector( t(hiB) )
      #   names(blo) <- names(bhi) <- bf
      bprior <- cbind(blo[rownames(betaStandXTable)],
                      bhi[rownames(betaStandXTable)])
      colnames(bprior) <- c('priorLo','priorHi')
      betaStandXTable <- cbind(betaStandXTable[,1:4], bprior) 
    }
    
    if( length(standRows) > 0 ){
      tmp <- .chain2tab(bgibbsUn[burnin:ng,], snames, xnames, wF = wB)
      betaMu    <- tmp$mu
      betaSe    <- tmp$se
      betaTable <- tmp$tab
      
    }else{
      betaMu    <- betaStandXmu
      betaSe    <- betaStandXse
      betaTable <- betaStandXTable
      
      betaWMu    <- betaStandXWmu
      betaWSe    <- betaStandXWse
      betaWTable <- betaStandXWTable
    }
    if( !is.null(bFacGibbs) ){
      tmp <- .chain2tab(bFacGibbs[burnin:ng,,drop=F])
      sensTable <- tmp$tab[,1:4]
    }
  }
  
  if(TIME){
    
    if(termR){
      
      RmatStandXmu <- RmatStandXse <- Rmat*0
      rhoMu <- rhoSe <- rhoTable <- NULL
      loL <- loRmat[wL]
      hiL <- hiRmat[wL]
      
      tmp <- .chain2tab(lgibbs[drop = FALSE, burnin:ng,], 
                        snames[notOther], xlnames, sigfig = 4)
      rhoStandXmu <- tmp$mu
      rhoStandXse <- tmp$se
      rhoStandXTable <- data.frame( rownames(tmp$tab), tmp$tab[,1:4],
                                    stringsAsFactors = F)
      rlo <- loRmat[wL]
      rhi <- hiRmat[wL]
      
      rprior <- cbind(rlo, rhi)
      rownames(rprior) <- rownames(wL)
      colnames(rprior) <- c('priorLo','priorHi')
      
      rhoStandXTable <- cbind( rhoStandXTable[,1:5], 
                               rprior[ rownames(rhoStandXTable), ])
      
      
      rhoStandXTable[,1] <- .replaceString( rhoStandXTable[,1], '_', ', ')
      ss <- 'rho_{to, from}'
      colnames(rhoStandXTable)[1] <- ss
      rhoStandXTable <- rhoStandXTable[!rhoStandXmu == 0 & !rhoStandXse == 0, ]
      rownames(rhoStandXTable) <- NULL
      
      
      tmp <- .chain2tab(lgibbs[drop = FALSE, burnin:ng,], 
                        colnames(Rmat), rownames(Rmat), sigfig = 4)
      RmatStandXmu[ wL ] <- tmp$tab[,'Estimate']
      RmatStandXse[ wL ] <- tmp$tab[,'SE']
      
      if( ncol(xl) > 1 ){
        Rmu <- Rmat*0
        tmp <- .chain2tab(lgibbsUn[drop = FALSE, burnin:ng,], 
                          snames[notOther], xlnames, sigfig = 4)
        rhoMu <- tmp$mu
        rhoSe <- tmp$se
        rhoTable <- data.frame( rownames(tmp$tab), tmp$tab[,1:4],
                                stringsAsFactors = F)
        rhoTable[,1] <- .replaceString( rhoTable[,1], '_', ', ')
        ss <- 'rho_{to, from}'
        colnames(rhoTable)[1] <- ss
        rownames(rhoTable) <- NULL
        rhoTable <- rhoTable[!rhoMu == 0 & !rhoSe == 0, ]
      }else{
        lgibbsUn <- lgibbs
        rhoMu <- rhoStandXmu
        rhoSe <- rhoStandXse
        rhoTable <- rhoStandXTable
      }
    }
    if(termA){
      
      ss <- matrix( as.numeric( columnSplit( colnames(alphaGibbs), '-' )), ncol = 2) 
      ss <- matrix( snames[aindex[,c('toW','fromW')]], ncol=2)
      st <- columnPaste( ss[,1], ss[,2], ', ' )
      
      
      tmp <- .chain2tab(alphaGibbs[drop = FALSE, burnin:ng,], 
                        ss[,1], ss[,2], sigfig = 4)
      alphaTable <- data.frame( st, tmp$tab[, 1:4],
                                stringsAsFactors = F )
      
      alo <- loAmat[wA]
      ahi <- hiAmat[wA]
      
      aprior <- cbind(alo, ahi)
      colnames(aprior) <- c('priorLo','priorHi')
      
      alphaTable <- cbind( alphaTable[,1:5], aprior)
      
      colnames(alphaTable)[1] <- 'alpha_{to, from}'
      
      loA <-  matrix(0,S,S)
      rownames(loA) <- colnames(loA) <- snames
      hiA <- alphaMu <- alphaSe <- loA
      
      tmp1 <- colMeans(alphaGibbs[burnin:ng,])    
      tmp2 <- apply(alphaGibbs[burnin:ng,],2,sd)
      alphaMu[ aindex[,c('toW','fromW')] ] <- tmp1
      alphaSe[ aindex[,c('toW','fromW')] ] <- tmp2
      loA[ aindex[,c('toW','fromW')] ] <- loAmat[wA]
      hiA[ aindex[,c('toW','fromW')] ] <- hiAmat[wA]
      
      Amu <- Ase <- Amat*0
      Amu[wA] <- tmp1
      Ase[wA] <- tmp2
    }
  }
  cat("Reached8")
  
  yMu <- ypred/ntot
  if('CA' %in% typeNames){
    ytmp <- yMu[,'CA' %in% typeNames]
    ytmp[ ytmp < 0 ] <- 0
    yMu[,'CA' %in% typeNames] <- ytmp
  }
  y22 <- ypred2/ntot - yMu^2
  y22[y22 < 0] <- 0
  ySd <- sqrt(y22)
  
  cMu <- cuts
  cSe <- numeric(0)
  
  ms <- sums2meanSd( wpred, wpred2, ntot )  
  wMu <- ms$mean
  wSd <- ms$sd
  
  if('OC' %in% typeNames){
    yMu[,ordCols] <- yMu[,ordCols] + ordMatShift
    wMu[,ordCols] <- wMu[,ordCols] + ordMatShift
  }
  
  meanDev <- sumDev/ntot
  
  if( termB ){
    beta <- betaStandXmu
    if( is.null(beta) )beta <- betaMu
    beta[ is.na(beta) ] <- 0  # if prior is set to zero
  }
  
  if(!TIME){
    
    muw <- x%*%beta[,notOther, drop=F]
    tmp <- .dMVN(wMu[,notOther], muw, sMean[notOther,notOther], log=T)
    pd  <- meanDev - 2*sum(tmp )
    DIC <- pd + meanDev
  }
  
  yscore <- colSums( .getScoreNorm(y[,notOther],yMu[,notOther],
                                   ySd[,notOther]^2),na.rm=T )  # gaussian w
  xscore <- xpredMu <- xpredSd <- NULL
  standX <- xmissMu <- xmissSe <- NULL
  
  if(RANDOM){
    ns <- 500
    simIndex <- sample(burnin:ng,ns,replace=T)
    tmp <- .expandSigmaChains(snames, alphaVarGibbs, otherpar, simIndex=simIndex,
                              sigErrGibbs, kgibbs, REDUCT=F, CHAINSONLY=F)
    alphaRandGroupVarMu <- tmp$sMu
    alphaRandGroupVarSe <- tmp$sSe
    
    ms <- sums2meanSd( alphaRanSums, alphaRanSums2, ntot )  
    alphaRandByGroup   <- ms$mean
    alphaRandByGroupSe <- ms$sd
  }
  
  if(PREDICTX){
    ms <- sums2meanSd( predx, predx2, ntot )  
    xpredMu <- ms$mean
    xpredSd <- ms$sd
    
    if(!TIME){
      xrow <- standRows
      xmu  <- standMatMu[,1]
      xsd  <- standMatSd[,1]
    }else{
      if(termB | termR){
        xrow <- numeric(0)
        if(termB){xrow <- standRows}
        if(termR){
          xrow <- c( xrow, standRowsL )
          ww   <- !duplicated(names(xrow))
          xrow <- names(xrow)[ww]
          
          xmu <- xsd <- numeric(0)
          
          if(termB){
            xmu  <- standMatMu[xrow,1]
            xsd  <- standMatSd[xrow,1]
          }
          if(termR){
            
            ww <- which(!rownames(standMatMuL) %in% names(xrow) )
            ww <- ww[ ww != 1 ]
            if(length(ww) > 0){
              xmu  <- c(xmu, standMatMuL[ww,1])
              xsd  <- c(xsd,standMatSdL[ww,1])
            }
          }
        }
      }
    }
    
    # if( ncol(x) > 1 & !UNSTAND ){
    
    if( ncol(x) > 1 ){  
      
      #   xf  <- NULL
      #   if(length(facNames) > 0){
      #     xf <- xdata[, facNames, drop=F]
      #   }
      
      xpredMu <- .getUnstandX(formula, xs = xpredMu, xdata, xrow )$xu
      xpredSd[,xrow] <- xpredSd[,xrow]*matrix( xsd[xrow], n, length(xrow), byrow=T ) 
    }
    if(Q == 2)xscore <- mean( .getScoreNorm(x[,2],
                                            xpredMu[,2],xpredSd[,2]^2) )
    if(Q > 2)xscore <- colMeans(.getScoreNorm(x[,-1],
                                              xpredMu[,-1],xpredSd[,-1]^2) )
    
    if(TIME){
      wz <- muw <- wMu
      wz[wz < 0] <- 0
      if(termB){
        muw <- x[,rownames(beta),drop=F]%*%beta[,notOther,drop=F]
      }
      if(termR){
        Vmat[tindex[,2],] <- wz[tindex[,2], 
                                gindex[,'colW']]*xl[tindex[,2], gindex[,'colX']]
        Vmat[timeZero,]   <- wz[timeZero, 
                                gindex[,'colW']]*xl[timeZero, gindex[,'colX']]
        Rmat[ gindex[,c('rowL','colW')] ] <- RmatStandXmu[ gindex[,c('rowG','colW')] ]
        muw <- muw + Vmat%*%Rmat[,notOther]
      }
      if(termA){
        Umat <- wz[,uindex[,1]]*wz[,uindex[,2]] 
        Amat[ wA ] <- alphaMu[ aindex[,c('toW','fromW')] ]
        
        muw <- muw + Umat%*%Amat[,notOther]
      }
    }
  }
  tmp <- .dMVN(wMu[,notOther],muw, sMean[notOther,notOther], log=T )
  pd  <- meanDev - 2*sum(tmp )
  DIC <- pd + meanDev
  
  if(termB){
    if(nmiss > 0){
      ms <- sums2meanSd( xmissSum, xmissSum2, ntot )  
      xmissMu <- ms$mean
      xmissSe <- ms$sd
    }
  }
  if(length(standRows) > 0){                #unstandardize
    standX <- cbind(standMatMu[,1],standMatSd[,1])
    colnames(standX) <- c('xmean','xsd')
    rownames(standX) <- rownames(standMatSd)
  }
  
  # betaSens, sigma and R
  
  ns <- 200
  simIndex <- sample(burnin:ng,ns,replace=T)
  tmp <- .expandSigmaChains(snames, sgibbs, otherpar, simIndex=simIndex,
                            sigErrGibbs, kgibbs, REDUCT, CHAINSONLY=F, verbose)
  corMu <- tmp$rMu; corSe <- tmp$rSe
  sigMu <- tmp$sMu; sigSe  <- tmp$sSe
  
  if( !TIME | (TIME & termB) ){
    ematrix  <- ess/ntot
    fmatrix  <- fmat/ntot
    
    ms <- sums2meanSd( esens1, esens2, ntot )  
    sensBeta   <- cbind( ms$mean, ms$sd )
    colnames(sensBeta) <- c('Estimate', 'SE')
    rownames(sensBeta) <- colnames(y)
  }
  
  if(termR){
    if(ncol(xl) > 1){
      ematrixL <- essL/ntot
      ms <- sums2meanSd( lsens1, lsens2, ntot )  
      sensRho   <- cbind( ms$mean, ms$sd )
      colnames(sensRho) <- c('Estimate', 'SE')
      rownames(sensRho) <- colnames(y)
    }
  }
  if(termA){
    ematrixA <- essA/ntot
    ms <- sums2meanSd( asens1, asens2, ntot )  
    sensAlpha  <- cbind( ms$mean, ms$sd )
    colnames(sensAlpha) <- c('Estimate', 'SE')
    rownames(sensAlpha) <- colnames(y)
  }
  
  tMu <- tSd <- tMuOrd <- btMu <- btSe <- stMu <- stSe <- numeric(0)
  cat("Reached9")
  
  if(TRAITS){
    
    ms  <- sums2meanSd( tpred, tpred2, ntot )  
    tMu <- ms$mean
    tSd <- ms$sd
    wo  <- which(traitTypes == 'OC')    #predict ordinal scores
    M   <- ncol(tMu)
    
    if(length(wo) > 0){
      tMuOrd <- tMu*0
      for(j in wo)tMuOrd[,j] <- round(tMu[,j],0) - 1
      tMuOrd <- tMuOrd[,wo]
    }
    
    tmp <- .chain2tab(bTraitGibbs[burnin:ng,], tnames, xnames)     # standardized
    betaTraitXMu <- tmp$mu
    betaTraitXTable <- tmp$tab
    
    tmp <- .chain2tab(bTraitUnstGibbs[burnin:ng,], tnames, xnames) #  unstandardized
    betaTrait      <- tmp$mu
    betaTraitTable <- tmp$tab
    
    
    tmp <- .chain2tab(mgibbs[burnin:ng,], tnames, tnames) 
    varTraitMu <- tmp$mu
    varTraitTable <- tmp$tab
    
    tmp <- .chain2tab(bTraitFacGibbs[burnin:ng,], tnames, rownames(tagg) )
    betaTraitXWmu <- tmp$mu
    betaTraitXWTable <- tmp$tab
  }
  
  if('OC' %in% typeNames){
    
    nk  <- length(ordCols)
    nc  <- ncut - 3
    
    os <- rep(ordShift,nc)
    
    cgibbs <- cgibbs + matrix(os,ng,length(os),byrow=T)
    
    tmp <- .chain2tab(cgibbs[burnin:ng,], cnames[-c(1,2,ncut)], snames[ordCols], sigfig = 4 )
    
    # tmp <- .processPars(cgibbs)$summary
    
    #  cMu <- matrix(tmp[,'estimate'],nk,nc)
    #  cSe <- matrix(tmp[,'se'],nk,ncut-3)
    cMu <- tmp$mu
    cSe <- tmp$se
    cMu <- cbind(ordShift,cMu)
    cSe <- cbind(0,cSe)
    colnames(cMu) <- colnames(cSe) <- cnames[-c(1,ncut)]
    rownames(cMu) <- rownames(cSe) <- snames[ordCols]
    breakMat[ordCols,c(2:(2+(ncol(cMu))-1))] <- cMu
  }
  
  if('PA' %in% typeNames){
    zMu <- yMu
    zSd <- ySd
  }
  
  if( termB ){
    varContribution <- signif(rbind(varTot, varMu, varExp, varRn, varMod), 3) 
    rownames(varContribution) <- c('total variance', 'mean fraction','R2',
                                   'RE fraction', 'mean + RE fraction')
    if(!RANDOM)varContribution <- varContribution[1:3,]
  }else{
    varContribution <- signif(varTot, 3) 
    names(varContribution) <- c('total variance')
  }
  
  
  
  # outputs
  if(length(reductList) == 0)reductList <- list(N = 0, r = 0)
  reductList$otherpar <- otherpar
  
  modelList$effort    <- effort;      modelList$formula <- formula
  modelList$typeNames <- typeNames;    modelList$censor <- censor
  modelList$effort    <- effort; modelList$holdoutIndex <- holdoutIndex
  modelList$REDUCT    <- REDUCT;       modelList$TRAITS <- TRAITS
  modelList$ematAlpha <- ematAlpha; modelList$traitList <- traitList
  modelList$reductList <- reductList; modelList$ng <- ng
  modelList$burnin <- burnin
  
  inputs <- list(xdata = xdata, xStand = x, xUnstand = xUnstand, 
                 xnames = xnames, effMat = effMat,
                 y = y, notOther = notOther, other = other, breakMat = breakMat, 
                 classBySpec = classBySpec, RANDOM = RANDOM)
  missing <- list(xmiss = xmiss, xmissMu = xmissMu, xmissSe = xmissSe, 
                  ymiss = ymiss, ymissMu = ymissPred, ymissSe = ymissPred2)
  parameters <- list(corMu = corMu, corSe = corSe, 
                     sigMu = sigMu, sigSe = sigSe,
                     wMu = wMu, wSd = wSd)
  prediction <- list(presence = presence, xpredMu = xpredMu, xpredSd = xpredSd,
                     ypredMu = yMu, ypredSd = ySd, richness = richness)
  chains <- list(sgibbs = sgibbs) 
  fit <- list(DIC = DIC, yscore = yscore, xscore = xscore, rmspeAll = rmspeAll,
              rmspeBySpec = rmspeBySpec,
              fractionExplained = varContribution )
  
  parXS <- 'standardized for X'
  parXU <- 'unstandardized for X'
  parWS <- 'correlation scale for W'
  parWU <- 'variance scale for W'
  parXF <- 'centered factors'
  parSep <- ', '
  
  getDescription <- function( nlist, words ){
    
    out <- numeric(0)
    nm  <- character(0)
    
    for(k in 1:length(nlist)){
      vk <- get( nlist[k] ) 
      if( is.null(vk) )next
      attr( vk, 'description') <- words 
      out <- append( out, list(assign( nlist[k], vk )) )
      nm  <- c( nm, nlist[k])
    }
    names(out) <- nm
    
    out
  }
  
  
  
  if( !TIME | (TIME & termB) ){
    
    if( all(diag(ematrix) == 0) )ematrix <- NULL
    
    nlist <- c( 'ematrix', 'fmatrix' )
    words <- paste( parXS, parWS, parXF, sep=parSep)
    tlist <- getDescription( nlist, words )
    if(length(tlist) > 0){
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
    }    
    parameters <- c(parameters, list(ematrix = ematrix, fmatrix = fmatrix, 
                                     sensBeta = sensBeta))
  }
  
  if(termB){
    
    nlist <- c('betaMu', 'betaSe', 'betaTable', 'bgibbsUn' )
    words <- paste( parXU, parWU, sep=parSep)
    tlist <- getDescription( nlist, words )
    if(length(tlist) > 0){
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
    }    
    
    nlist <- c('betaStandXmu', 'betaStandXTable', 'bgibbs')
    words <- paste( parXU, parWU, sep=parSep )
    tlist <- getDescription( nlist, words )
    if(length(tlist) > 0){
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
    }
    
    nlist <- c( 'betaStandXWmu', 'betaStandXWTable' )
    if(!is.null(sensTable))nlist  <- c(nlist, 'sensTable')
    if(!is.null(fSensGibbs))nlist <- c(nlist, 'fSensGibbs')
    if(!is.null(bFacGibbs))nlist  <- c(nlist, 'bFacGibbs')
    
    
    words <- paste( parXS, parWS, parXF, sep = parSep)
    tlist <- getDescription( nlist, words )
    
    if(length(tlist) > 0){
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
    }
    
    attr(sensBeta, 'description') <- parXS
    
    inputs <- c(inputs, list(standMatSd = standMatSd, standRows = standRows, standX = standX,
                             notOther = notOther, other = other,  designTable = designTable,  
                             factorBeta = factorBeta, interBeta = interBeta,
                             linFactor = linFactor, intMat = intMat) )
    chains <- c(chains, list(bgibbs = bgibbs, bgibbsUn = bgibbsUn,
                             fSensGibbs = fSensGibbs, bFacGibbs = bFacGibbs) )
    parameters <- c(parameters, list(betaMu = betaMu, betaSe = betaSe, betaTable = betaTable, 
                                     betaStandXmu = betaStandXmu, 
                                     betaStandXTable = betaStandXTable,
                                     betaStandXWmu =  betaStandXWmu,
                                     betaStandXWTable = betaStandXWTable,
                                     sensTable = sensTable))
  }
  
  if(FULL)chains <- append(chains, list(ygibbs = ygibbs))
  if(RANDOM){
    parameters <- append(parameters,
                         list( randGroupVarMu = alphaRandGroupVarMu,
                               randGroupVarSe = alphaRandGroupVarSe,
                               randByGroupMu = alphaRandByGroup,
                               randByGroupSe = alphaRandByGroupSe,
                               groupIndex = groupIndex) )
  }
  if(RICHNESS){
    prediction <- append(prediction, 
                         list(yPresentMu = ypredPresMu, yPresentSe = ypredPresSe))
  }
  if(REDUCT) {
    ms  <- sums2meanSd( rndTot, rndTot2, ntot )  
    rndEffMu <- ms$mean
    rndEffSe <- ms$sd
    parameters <- append( parameters, list(rndEffMu = rndEffMu, rndEffSe = rndEffSe) )
    chains <- append( chains, list(kgibbs = kgibbs, sigErrGibbs = sigErrGibbs) )
  }
  
  if('OC' %in% typeNames){
    parameters <- c(parameters,list(cutMu = cMu, cutSe = cSe))
    chains <- c(chains,list(cgibbs = cgibbs))
    modelList <- c(modelList,list(yordNames = yordNames))
  }
  
  if(TRAITS){
    parameters <- c(parameters,
                    list(betaTrait = betaTrait, 
                         betaTraitTable = betaTraitTable,
                         betaTraitXMu = betaTraitXMu, 
                         betaTraitXTable = betaTraitXTable,
                         varTraitMu = varTraitMu, 
                         varTraitTable = varTraitTable,
                         betaTraitXWmu = betaTraitXWmu,
                         betaTraitXWTable = betaTraitXWTable))
    prediction <- c(prediction, list(tMuOrd = tMuOrd, tMu = tMu, tSe = tSd))
    chains <- append( chains,list(bTraitGibbs = bTraitGibbs,
                                  bTraitFacGibbs = bTraitFacGibbs,
                                  mgibbs = mgibbs) ) 
  }
  
  if(TIME){
    
    inputs <- c(inputs, list(timeList = timeList))
    
    if(termB){
      parameters <- c(parameters, list(wB = wB, tindex = tindex))
    }
    if(termR){
      
      nlist <- c('lgibbs', 'rhoStandXmu', 'rhoStandXse', 'rhoStandXTable')
      tlist <- getDescription( nlist, words = parXS )
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
      
      nlist <- c('lgibbsUn', 'rhoMu', 'rhoSe', 'rhoTable')
      tlist <- getDescription( nlist, words = parXU )
      for( k in 1:length(tlist) ) assign( names(tlist)[k], get( names(tlist[k]) ) )
      
      inputs <- c(inputs, list(xlnames = xlnames, xRho = xl, interRho = interRho, 
                               factorRho = factorRho))
      chains <- c(chains, list(lgibbs = lgibbs, lgibbsUn = lgibbsUn))
      parameters <- c(parameters, 
                      list(gindex = gindex, rhoMu = rhoMu, rhoSe = rhoSe,
                           rhoStandXmu = rhoStandXmu, rhoStandXse = rhoStandXse, 
                           rhoTable = rhoTable,
                           rhoStandXTable = rhoStandXTable,  
                           RmatStandXmu = RmatStandXmu, RmatStandXse = RmatStandXse,
                           rhoLo = loL, rhoHi = hiL, wL = wL))
      
      if( ncol(xl) > 1 ){
        attr(ematrixL, 'description') <- attr(sensRho, 'description') <- parXS
        parameters <- c(parameters, 
                        list(ematrixL = ematrixL, sensRho = sensRho))
      }
    }
    if(termA){
      
      chains <- c(chains, list(alphaGibbs = alphaGibbs))
      parameters <- c(parameters, list(alphaTable = alphaTable,
                                       alphaMu = signif(alphaMu, 4), 
                                       alphaSe = signif(alphaSe, 4),
                                       Amu = signif(Amu, 4), 
                                       Ase = signif(Ase, 4),
                                       alphaLo = loA, alphaHi = hiA,
                                       aindex = aindex, wA = wA, uindex = uindex,
                                       ematrixA = ematrixA, sensAlpha = sensAlpha,
                                       alphaEigen = eigen(alphaMu)$values))
    }
  }
  
  chains     <- chains[ sort( names(chains) ) ]
  fit        <- fit[ sort( names(fit) )]
  inputs     <- inputs[ sort( names(inputs) )]
  missing    <- missing[ sort( names(missing) )]
  modelList  <- modelList[ sort( names(modelList) )]
  parameters <- parameters[ sort( names(parameters) )]
  prediction <- prediction[ sort( names(prediction) )]
  
  wk <- sapply( chains, length )
  chains <- chains[ wk > 0 ]
  
  wk <- sapply( fit, length )
  fit <- fit[ wk > 0 ]
  
  wk <- sapply( inputs, length )
  inputs <- inputs[ wk > 0 ]
  
  wk <- sapply( missing, length )
  missing <- missing[ wk > 0 ]
  
  wk <- sapply( modelList, length )
  modelList <- modelList[ wk > 0 ]
  
  wk <- sapply( parameters, length )
  parameters <- parameters[ wk > 0 ]
  
  wk <- sapply( prediction, length )
  prediction <- prediction[ wk > 0 ]
  
  cat("Reached10")
  
  all <- list(chains = chains, fit = fit, inputs = inputs, missing = missing,
              modelList = modelList, parameters = parameters,
              prediction = prediction)
  all$call <- match.call()
  all <- all[ sort(names(all)) ]
  class(all) <- "gjam"
  
  all
}
