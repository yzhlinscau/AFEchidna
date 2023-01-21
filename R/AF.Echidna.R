## version: public

# update: 2022-11-28
#' @title Summary of added functions for Echidna
#' 
#' @param dat.file	 data file to generate .es file.
#' @param es.file	   the .es file to generate .es0 file.
#' @param es0.file	 the .es0 file.
#' @param object	   Echidna result object in R.
#' @param path    the path for data files.	
#' @param softp   the path for Echidna software.
#  @param update  update for Echidna software.
#' @param trace	  show iteration procedure,FALSE(default). 
#' @param maxit   maximum number of iterations, 30(default).
#' @param Fmv     make missing values into fixed terms, FALSE(default).
#' @param mu.delete     delete term mu or Trait from model, FALSE(default).
#' @param mulT	  multi-trait model,FALSE(default).
#' @param met	    multi-environment trial model,FALSE(default).
#' @param mulN	  trait number for multi-trait analysis at one time, 2(default).
#' @param mulp	  multi-pin formula to run at one time, NULL(default). 
#' @param cycle	  Echidna result from qualifier cycle,FALSE(default).
#' @param trait	   aim trait for analysis, such as, 'h3', 'h3 h4',~h3+h4, etc, NULL(default).
#' @param family  such as esr_binomial(), esr_poisson().
#' @param selfing  the probability of selfing for parent, such as 0.1.
#' @param weights   A variable used as weights in the fit.
#' @param fixed      fixed effects, such as, c('Rep'), c('Site', 'Site.Rep') or 'Site Site.Rep', h3~1+Rep, etc.	
#' @param random	   random effects, such as,'Mum','Mum Mum.Rep',~Mum+Mum:Rep, etc. 
#' @param residual	 residual effects, such as,'units','ar1(row).ar1(col)',~ar1(row):ar1(col), etc.
#' @param batch     run batch analysis for more than 2 trait at one time, FALSE(default).
#' @param batch.G   run more than 2 G structures at one time, FALSE(default). 
#' @param batch.R   run more than 2 R structures at one time, FALSE(default). 
#' @param foldN	    new folder name to store each run's results, only works when delf is 'FALSE'.
#' @param delf      delete all Echidna result files from the folder of .es0 file, TRUE(default).	
#' @param message	  show running procedure,FALSE(default). 
#' @param run.purrr  using purrr packages for batch analysis,FALSE(default).
#' @param predict	   prediction for model terms. 
#' @param vpredict	 run vpredict statements with Echidna soft.
#' @param jobqualf	 header line qualifiers, mainly '!view'. 
#' @param qualifier	 model qualifiers, such as '!extra 5'.
#' 
#' @details
#'   This package would supply some functions for Echidna. Details as following:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Description} \cr
#' \code{esRT}       \tab load Echidna results into R. \cr
#' \code{wald}       \tab output wald results. \cr
#' \code{Var}        \tab output variance components. \cr
#' \code{summary}    \tab output summary results. \cr
#' \code{IC}         \tab output AIC and BIC values. \cr
#' \code{pin}        \tab run pin functions.\cr
#' \code{predict}    \tab output predict results.\cr
#' \code{plot}       \tab output model diagnose results.\cr
#' \code{coef}       \tab output fixed and random effects.
#' }
#'
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#'  Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 \cr
#'  Gilmour, A.R. (2020) Echidna Mixed Model Software www.EchidnaMMS.org
#' @name AF.Echidna
#' @examples
#' \dontrun{
#'
#'  library(AFEchidna)
#'
#'  ##  Echidna
#'  path='D:/Echidna/Jobs'
#'
#'  setwd(path)
#'  
#'  ## generate .es0 file
#'  get.es0.file(dat.file='fm.csv')
#'  get.es0.file(es.file='fm.es')
#'  # file.edit('fm.es0')
#'
#' res<-echidna(trait='h3',
#'               fixed='Rep',random='Fam',
#'               residual=NULL,predict=c('Fam'),
#'               es0.file="fm.es0")
#' 
#' ## method 2                           
#' # res<-echidna(fixed=h3~1+Rep,random=~Fam,
#' #              residual=NULL,predict=c('Fam'),
#' #              es0.file="fm.es0")
#'
#'  names(res)
#'  class(res)
#'
#'  # model diagnose
#'  plot(res) 
#'
#'  # wald result
#'  wald(res)
#'  waldT(res, term=c('mu','Rep'))
#'  
#'
#'  # variance components
#'  Var(res)
#'
#'  # summary result
#'  summary(res)
#'
#'  # AIC,BIC result
#'  IC(res)
#'
#'  # fixed and random effects
#'  coef(res)$fixed
#'  coef(res)$random
#'
#'  # predict results if using predict functions
#'  mm<-predict(res)
#'  mm$pred
#'
#'  # show vc results by using vpredict statements
#'  pin(res)
#'
#'  # run pin function to count genetic parameters
#'  pin11(res,h2~V1/(V1+V2))
#'  pin(res,mulp=c(h2~V1/(V1+V2),h2f~V1/(V1+V2/4)))
#'
#'  # model converge stage
#'  trace(res)
#'  res$Converge
#'
#' }
NULL

############### following functions to input results from Echidna

#' @rdname  AF.Echidna
#' @usage get.es0.file(dat.file=NULL,es.file=NULL,
#'                         path=NULL,message= FALSE,
#'                         softp=NULL,
#'                         faS=NULL,pedS=NULL)
#' @export
get.es0.file <- function(dat.file=NULL,es.file=NULL,path=NULL, 
                       message = FALSE,softp=NULL,#update=FALSE,
                       faS=NULL,pedS=NULL) { # ,AfaS=NULL
  
  if(is.null(softp)) Echsf<-AFEchidna::loadsoft()
  if(!is.null(softp)) Echsf<-softp
   
  if(!is.null(dat.file)){ #nextp==FALSE
    
    if (!is.null(path))  setwd(path)
    
    if (message == TRUE)  mess1 <- FALSE
    else mess1 <- TRUE
    
    if(!file.exists(dat.file)) stop('data file does not exist.')
     else {
      esjob <- dat.file# esjob<-'fm.csv'
      runes <- paste(Echsf, esjob, sep = " ")
      system(runes, show.output.on.console = TRUE, wait = FALSE, invisible = mess1)
      #system2(Echsf, args=esjob, wait = FALSE, invisible = mess1)
      
      #flst <- dir()
      #temp <- flst[grep("\\.es$", flst)]
      #temp<-sub('.csv','.es',dat.file)
      #if(file.exists(temp))
        cat("Generating .es temple for",dat.file,": ", "--done!\n") # temp,
      #else stop('Generating .es temple fails.')
    } 
    
  }
  if(!is.null(es.file)){
    
    if(!file.exists(es.file)) 
      stop('es file does not exist.\n Error reason: Echidna may not work.')
    
    if (!is.null(path)) path0<-path 
    else path0<-getwd()
    setwd(path0)
    
    #flst<-dir()
    #if(is.null(es.file)) es.file <- flst[grep("\\.es$", flst)]
    #else es.file<-es.file#"barley.es" 
    
    new_dir<-'tempd'
    dir.create(new_dir)
    file.copy(es.file,new_dir, overwrite=TRUE)
    
    path1<-paste(path0,new_dir,sep='/')
    
    setwd(path1)
    tempf <- base::readLines(es.file) #es.file<-"dm.es"
    tempf <- sub("\\!DOPART \\$1", "#!DOPART $1", tempf)
    
    tempf <- sub(" correctly classified", "", tempf)
    
    ## data fields
    ## head 4 lines: jobq+title+header+data1
    ## variable: 4+orgS
    ## faS example: 1~6; faS=1:6
    
    #if(!is.null(pedS)) tempf[4+pedS]<-sub('  #','!P #',tempf[4+pedS])
    #if(!is.null(faS)) tempf[4+faS]<-sub('  #','!I #',tempf[4+faS])
    #if(!is.null(AfaS)) tempf[4+AfaS]<-sub('  #','!A #',tempf[4+AfaS])
    
    ## data fields
    #if(is.null(pedS) ){
      b<-NULL
      b[1]<-4
      b[2]<-which(grepl("(# Verify data.*)", tempf))
      datf<-tempf[4:b[2]]
      
      c<-NULL
      if(!is.null(faS)) c <- faS+1
        else c<-which(grepl("(^[A-Z])", datf))
      c0<-which(grepl("(.*!A.*)", datf))
      c<-c[!c %in% c0]
      tempf[3+c]<-sub('  #','!I #',tempf[3+c])
    #}
    
    a <- NULL
    a[1] <- which(grepl("(Y ~ mu Fixed.*)", tempf))
    a[2] <- which(grepl("(residual.*)", tempf))
    tempf <- tempf[-a]
    
    dat.fl <- which(grepl("(!SKIP)", tempf))
    if(!is.null(pedS)) {
      tempf[4+pedS]<-sub(' !I #',' !P #',tempf[4+pedS])
      tempf <- append(tempf,tempf[dat.fl])
      tempf[length(tempf)-1] <- paste0(tempf[length(tempf)-1], ' !PARTIALSELF 0')
    }
      
    
    flst <- dir()
    flst <- gsub("\\.es", "\\.es0", flst)
    temp <- flst[grep("\\.es0$", flst)]
    write(tempf, file = temp)
    
    file.copy(temp,path0,overwrite = TRUE)
    #file.remove(dir())
    
    setwd(path0)
    unlink(new_dir,recursive =TRUE, force=TRUE)
    
    esf<-dir(pattern='.es$')
    file.remove(esf)
    
    if(!is.null(pedS)) 
      message('Make sure there has a correct pedigree file in the .es0 file.\n')
    
    cat("Generating .es0 file:", "-- done!\n") # temp,
    
    invisible()
  }
  
}

##########
#' @rdname  AF.Echidna
#' @usage echidna(fixed,random,residual,
#'                    trait,family,weights, 
#'                    es0.file,softp,  
#'                    delf,foldN,
#'                    trace,maxit,
#'                    Fmv,mu.delete,
#'                    mulT,met,cycle,
#'                    batch,mulN,mulp,
#'                    batch.G,batch.R,
#'                    run.purrr,selfing,
#'                    predict,vpredict,
#'                    qualifier,jobqualf) 
#' @export
echidna <- function(fixed=NULL,random=NULL,residual=NULL,
                  trait=NULL,family=NULL,#trait.mod=NULL,
                  weights=NULL,
                  es0.file,softp=NULL,
                  delf=TRUE,foldN=NULL,
                  trace=TRUE,maxit=30,
                  Fmv=FALSE,mu.delete=FALSE,
                  mulT=FALSE,met=FALSE,cycle=FALSE,
                  batch=FALSE,mulN=NULL,mulp=NULL, 
                  batch.G=FALSE,batch.R=FALSE,
                  run.purrr=FALSE,selfing=NULL,
                  predict=NULL,vpredict=NULL,
                  qualifier=NULL,jobqualf=NULL){
  
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  
  if(!is.null(trait)){
    #trait=~h3+h4+h5
    #trait=c(~h3,~h4,~h5)
    #trait=c('h3','h4','h5')
    if(class(trait)=="formula") {
      trait1 <- as.character(trait)
      #trait1<-trait1[trait1!='~']
      trait <- strsplit(trait1[2],'\\+')[[1]]
    }
    
    if(class(trait)=="list") {
      #trait1<-list();length(trait1)<-length(trait)
      trait1 <- vector("list", length(trait))
      trait1 <- lapply(trait, as.character)
      #trait1<-as.character(trait)
      if(length(trait1[[1]])==3){
        trait0 <- unlist(lapply(trait1, function(x) x[2]))
        trait  <- unlist(lapply(trait1, function(x) x[3]))
        #trait1<-gsub('\\+',' ',trait1)
      }
      if(length(trait1[[1]])==2){
        ran0 <- NA
        trait <- unlist(lapply(trait1, function(x) x[2]))
        #trait1<-gsub('\\+',' ',trait1)
      } 
    }
    
  }
  
  test<-function(mode=c("batch.Y", "batch.G", "batch.R" )){
    
    mode <- switch(mode, "batch.Y" = 1, 
                   "batch.G" = 2, "batch.R" = 3)
    
    if (as.numeric(mode)==1) {
      
      if(batch==FALSE){ # AFEchidna::  ###  batch.Y, batch.G, batch.R
        ttN <- 1
        batch0 <- FALSE
      } else{ 
        # batch
        require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
        require(tidyr,warn.conflicts=FALSE,quietly=TRUE) # unite
        batch0 <- TRUE
        
        cat('\nProgram starts running batch analysis ------ \n')
        
        if(trace==FALSE & !is.null(mulp)){
          cat('\npin formula: \n');for(i in 1:length(mulp)) print(mulp[[i]])
          #cat('\n')
        }
        
        cc <- trait; ccN <-length(cc)
        if(mulT==FALSE){ ttN <- trait}
        
        if(mulT==TRUE){
          if(is.null(mulN)) mulN <- 2
          
          if((ccN/mulN)>1) { bb <- utils::combn(cc,mulN);bbn <- ncol(bb)}
          if((ccN/mulN)==1){ bb <- cc;bbn <- 1}
          if((ccN/mulN<1)){ stop("\nThe trait No is less than in the model!\n")}
          
          ttN <- 1:bbn
        }
      }
      
      run.fun1 <- function(x){
        
        if(trace==TRUE & batch==TRUE) cat('\nrun',x,'-- -- --:')
        if(batch==FALSE) {
          x <- trait
        }
        
        if(batch==TRUE){
          if(mulT==FALSE) x <- x else {
            if(bbn>1)  x <- paste(bb[,x],collapse=' ')
            if(bbn==1) x <- paste(bb,collapse=' ')
          }
        }
        
        AFEchidna::run.mod(es0.file=es0.file,softp=softp,
                           trait=x,family=family,#trait.mod=trait.mod,
                           weights=weights,
                           fixed=fixed,random=random,residual=residual,
                           mulT=mulT,met=met,selfing=selfing,
                           trace=trace,delf=delf,foldN=foldN,
                           predict=predict,vpredict=vpredict,
                           maxit=maxit,cycle=cycle,
                           Fmv=Fmv,mu.delete=mu.delete,
                           qualifier=qualifier,jobqualf=jobqualf)
       } 
        
      if(!run.purrr) ss <- lapply(ttN, run.fun1)
        else ss <- ttN %>% purrr::map( run.fun1 )
      
      #
      if(batch==FALSE) {
        ss <- ss[[1]]
        if(cycle==FALSE) NTrait <- ss$Traits
        if(cycle==TRUE) NTrait <- names(ss$esr.all)
      }
      
      if(batch==TRUE){
        if(mulT==FALSE) names(ss) <- trait
        if(mulT==TRUE) names(ss) <- unlist(lapply(ss,function(x) x$Traits))
        
        NTrait <- names(ss) 
      }
      NTrait <- gsub(' ','-',NTrait)
      NTrait <- sub('-$','',NTrait)
      
      tt <- tt1 <- NULL
      
      if(batch==FALSE) {
        tt <- ss
      }
      if(batch==TRUE){
        tt$res.all <- ss
        Converge <- sapply(tt$res.all,function(x) x$Converge)
        maxit <- sapply(tt$res.all,function(x) x$maxit)
      }
      
      call <- list(fixed=fixed,random=random,residual=residual)
      
      org.par <- list(es0.file=es0.file,softp=softp,
                    trait=trait,family=family,#trait.mod=trait.mod,
                    weights=weights,selfing=selfing,
                    fixed=fixed,random=random,residual=residual,
                    mulT=mulT,mulN=mulN,mulp=mulp,
                    met=met,trace=trace,delf=delf,
                    Fmv=Fmv,mu.delete=mu.delete,
                    cycle=cycle,batch=batch,
                    batch0=batch0,
                    call=call,run.purrr=run.purrr,
                    batch.G=batch.G,batch.R=batch.R,
                    predict=predict,vpredict=vpredict,
                    qualifier=qualifier,jobqualf=jobqualf)
    }
    
    # for 2 more G-structures
    if (as.numeric(mode)==2) {
      batch0 <- TRUE
      
      cat('\nProgram runs for 2 more G-structure at one time. ------ \n')
      
      random1 <- lapply(random, as.character)
      if(length(random1[[1]])==3){
        ran0 <- unlist(lapply(random1, function(x) x[2]))
        ran1 <- unlist(lapply(random1, function(x) x[3]))
        ran1 <- gsub('\\+',' ',ran1)
      }
      if(length(random1[[1]])==2){
        ran0 <- NA
        ran1 <- unlist(lapply(random1, function(x) x[2]))
        ran1 <- gsub('\\+',' ',ran1)
      }
      
      ttN <- length(ran1)
      if(is.na(ran0)) ran0 <- paste0('G',1:ttN)
        
      run.fun2 <- function(x){
        #x=1
        if(trace==TRUE) cat('\nrun',x,'-- random effects:', ran1[x])
        # 
        if(!is.na(ran1[x]))
          AFEchidna::run.mod(es0.file=es0.file,softp=softp,
                  fixed=fixed,random=ran1[x],residual=residual,
                  mulT=mulT,met=met,selfing=selfing,
                  trace=trace,delf=delf,foldN=foldN,
                  predict=predict,vpredict=vpredict,
                  maxit=maxit,cycle=cycle,
                  Fmv=Fmv,mu.delete=mu.delete,
                  qualifier=qualifier,jobqualf=jobqualf)
        else
          AFEchidna::run.mod(es0.file=es0.file,softp=softp,#trait=x,
                             fixed=fixed,random=NULL,residual=residual,
                             mulT=mulT,met=met,selfing=selfing,
                             trace=trace,delf=delf,foldN=foldN,
                             predict=predict,vpredict=vpredict,
                             maxit=maxit,cycle=cycle,
                             Fmv=Fmv,mu.delete=mu.delete,
                             #batch0=batch0,
                             qualifier=qualifier,jobqualf=jobqualf)
      
      }
      
      if(!run.purrr) ss <- lapply(1:ttN, run.fun2) 
        else ss <- 1:ttN %>% purrr::map( run.fun2 ) 
      
      names(ss) <- ran0
      
      tt <- NULL
      
      tt$res.all <- ss
      call <- list(fixed=fixed,random=random,residual=residual)
      
      org.par <- list(es0.file=es0.file,softp=softp,
                    trait=trait,family=family,#trait.mod=trait.mod,
                    weights=weights,selfing=selfing,
                    fixed=fixed,random=random,residual=residual,
                    mulT=mulT,mulN=mulN,mulp=mulp,
                    met=met,trace=trace,delf=delf,
                    #batch=TRUE,
                    batch0=batch0,batch=batch,
                    cycle=cycle,
                    Fmv=Fmv,mu.delete=mu.delete,
                    call=call,run.purrr=run.purrr,
                    batch.G=batch.G,batch.R=batch.R,
                    predict=predict,vpredict=vpredict,
                    qualifier=qualifier,jobqualf=jobqualf)
 
    }
    
    # for 2 more R-strucutre
    if (as.numeric(mode)==3) {
      batch0 <- TRUE
      cat('\nProgram runs for 2 more R-structure at one time. ------ \n')
      
      residual1 <- lapply(residual, as.character)
      if(length(residual1[[1]])==3){
        resid0 <- unlist(lapply(residual1, function(x) x[2]))
        resid1 <- unlist(lapply(residual1, function(x) x[3]))
        resid1 <- gsub('\\+',' ',resid1)
      }
      if(length(residual1[[1]])==2){
        resid0 <- NA
        resid1 <- unlist(lapply(residual1, function(x) x[2]))
        resid1 <- gsub('\\+',' ',resid1)
      }
      
      ttN <- length(resid1)
      if(is.na(resid0)) resid0 <- paste0('R',1:ttN)
      
        
      run.fun3 <- function(x){
        #x=1
        if(trace==TRUE) cat('\nrun',x,'-- residual effects:', resid1[x])

        AFEchidna::run.mod(es0.file=es0.file,softp=softp,
                           fixed=fixed,random=random,residual=resid1[x],
                           mulT=mulT,met=met,selfing=selfing,
                           trace=trace,delf=delf,foldN=foldN,
                           predict=predict,vpredict=vpredict,
                           maxit=maxit,cycle=cycle,
                           Fmv=Fmv,mu.delete=mu.delete,
                           qualifier=qualifier,jobqualf=jobqualf)

      }
      
      if(!run.purrr) ss <- lapply(1:ttN, run.fun3 ) 
      else ss <- 1:ttN %>% purrr::map(run.fun3 )
      
      names(ss) <- resid0
      
      tt <- NULL
      
      tt$res.all <- ss
      call <- list(fixed=fixed,random=random,residual=residual)
      
      org.par <- list(es0.file=es0.file,softp=softp,
                    trait=trait,family=family,#trait.mod=trait.mod,
                    weights=weights,selfing=selfing,
                    fixed=fixed,random=random,residual=residual,
                    mulT=mulT,mulN=mulN,mulp=mulp,
                    met=met,trace=trace,delf=delf,
                    Fmv=Fmv,mu.delete=mu.delete,
                    cycle=cycle,
                    #batch=TRUE,
                    batch0=batch0,batch=batch,
                    call=call,run.purrr=run.purrr,
                    batch.G=batch.G,batch.R=batch.R,
                    predict=predict,vpredict=vpredict,
                    qualifier=qualifier,jobqualf=jobqualf)
    }
    
    tt$org.par <- org.par
    
    class(tt) <- c('esR') 
    
    return(tt)
  }
  
  if(batch.G==FALSE & batch.R==FALSE)  tt <- test(mode="batch.Y")
  if(batch.G==TRUE  & batch.R==FALSE)  tt <- test(mode="batch.G")
  if(batch.R==TRUE  & batch.G==FALSE)  tt <- test(mode="batch.R")
      
  return(tt)
}




#' @export
run.mod <- function(es0.file,softp=NULL,
                    trait=NULL,family=NULL,#trait.mod=NULL,
                    weights=NULL,
                    fixed=NULL,random=NULL,residual=NULL,
                    delf=TRUE,foldN=NULL,selfing=NULL,
                    trace=TRUE,maxit=30,
                    Fmv=TRUE,mu.delete=FALSE,
                    mulT=FALSE,met=FALSE,cycle=FALSE,
                    batch=FALSE, 
                    predict=NULL,vpredict=NULL,
                    qualifier=NULL,jobqualf=NULL) {
  
  if(is.null(softp))  Echsf <- AFEchidna::loadsoft()
  if(!is.null(softp)) Echsf <- softp
  
  #setwd(es0.path)
  flst <- dir()

  if(is.null(es0.file)) stop('Needs an .es0 file.\n')
  # if(met==TRUE) mulT <- TRUE
  
  flst0 <- flst
  
  if(class(fixed)=="formula") {
    fixed1 <- as.character(fixed)
    if(length(fixed1)==2) # fixed=~Rep
      fixed <- gsub('\\+','',fixed1[2])
    if(length(fixed1)==3){ 
      # trait0<-sub('cbind\\(','',fixed1[2])
      # trait0<-sub('\\)$','',trait0)
      
      #fixed=cbind(h3,h4)~Trait+Trait:Rep
      patt <- '(?<=\\().+?(?=\\))' # match text in the (...)
      trait0 <- stringr::str_extract(string =fixed1[2], pattern = patt)
      if(is.na(trait0)) trait0 <- fixed1[2] # fixed=h3~1+Rep+plot
      if(is.null(family)) trait0 <- gsub(',',' ',trait0)
      #family=c(esr_binomial(),esr_binomial())
      
      if(!is.null(family)){
        trait0 <- strsplit(trait0,', ')[[1]]
        trait00<-NULL
        for(i in 1:length(trait0))
          trait00[i]<-paste0(trait0[i],family[i])
        trait0 <- paste0(trait00,collapse = ' ')
      }
      
      fixed <- gsub('\\+','',fixed1[3])
      fixed <- sub('^1\\s','',fixed)
      fixed <- sub('^Trait\\s','',fixed)
    }
  }
  
  if(is.null(trait)) trait <- trait0
   else trait <- trait
  
  #i=1;
  if(trace==TRUE) cat('\nRunning Echidna for analysis: ',trait,'\n')
  
  qualF<-NULL;qualF[1]<-' '
  qualF[2]<-' !SLN !YHT '
  qualF[3]<-paste0(' !maxit ',maxit, ' ')
  if(!is.null(qualifier)) qualF[4]<-qualifier
  qualF<-paste(qualF,sep=' ')
  
  estxt<-NULL;
  #if(!is.null(qualifier)) estxt[1]<-qualifier else estxt[1]<-''
  if(cycle==TRUE) {
    estxt[1]<-paste('\n!cycle', paste(trait,collapse = " "), sep=' ')
    #estxt[1]<-paste0(estxt[1], cytxt)
  } else estxt[1]<-''
  estxt[2]=''
  estxt<-paste(estxt,sep=' ')
  
  # linear model
  lmtxt<-NULL
  
  if(mulT==FALSE) {
    if(cycle==FALSE){
      if(is.null(fixed)) {lmtxt[1]<-paste0(trait,'~ mu ,') 
      } else lmtxt[1]<-paste0(paste(trait,paste(fixed,collapse = " "),sep='~ mu '),' ,') # fix
    } else {
      if(is.null(fixed)){ lmtxt[1]<-paste0(paste('$I',' ~ mu ,'))
      } else lmtxt[1]<-paste0(paste('$I',paste(fixed,collapse = " "),sep=' ~ mu '),' ,')                                   
    } 
  }else{
    if(is.null(fixed)){ lmtxt[1]<-paste0(trait,' ~ Trait ,')
    } else lmtxt[1]<-paste0(paste(trait,paste(fixed,collapse = " "),sep=' ~ Trait '),' ,')
  }
  
  # if(met==TRUE) lmtxt<-gsub('Trait',' mu ',lmtxt)
  lmtxt<-gsub('~ mu 1','~ mu ',lmtxt)
  
  ## weights
  if(!is.null(weights))
    lmtxt<-gsub('~ mu',paste0(' !WT ',weights,' ~ mu '),lmtxt)

  ## family
  # if(!is.null(family))
  #  lmtxt<-gsub('~ mu',paste0(' ',family,' ~ mu '),lmtxt)
  
  
  if(class(random)=="formula"){
    random1 <- as.character(random)
    
    if(!grepl('init',random1[2])){
      random <- gsub('\\+','',random1[2])
      if(grepl('\\*',random)) random <- gsub(' ','',random)
    } else { # random=~us(Trait,init=c(.1,.1,.1)):Fam
      random <- AFEchidna::init.tr(random1)
    }
  } 
  if(is.null(random)) {
    lmtxt[2] <- '!r '
  }else lmtxt[2] <- paste0('!r ',paste(random,collapse = " ")) # ran
  
  if(Fmv==TRUE) lmtxt[2] <- paste0(lmtxt[2], '  !f mv')
  
  if(class(residual)=="formula"){
    residual1 <- as.character(residual)
    
    if(!grepl('init',residual1[2])){
      residual <- gsub('\\+','',residual1[2])
    } else { # residual=~units:us(Trait,init=c(.1,.1,.1))
      residual <- AFEchidna::init.tr(residual1)
    }
  } 
  
  
  if(is.null(residual)) {# resid
    lmtxt[3]<- ' '  #  'residual units'
  }  else lmtxt[3]<-paste0('residual ',residual)
  
  #### !!!!! spatial
  lmtxt<-gsub('ar1v','ar1',lmtxt) 
  lmtxt<-gsub('idv','id',lmtxt)


  #### !!!!! delete mu or Trait
  if(mu.delete==TRUE) {
      if(mulT==TRUE) lmtxt<-gsub('~ Trait','~ ',lmtxt)
      if(mulT==FALSE) lmtxt<-gsub('~ mu',' ~ ',lmtxt)
  }
  
  # predict
  predtxt<-NULL
  if(!is.null(predict)){

    predtxt<-sapply(1:length(predict), function(x) paste('predict ',predict[x],' '))
  } 
  predtxt<-c('',predtxt)
  
  # vpredict
  vptxt<-vptxt2<-NULL
  vptxt[1]<-''
  vptxt[2]<-'vpredict'
  vptxt[3]<-'W components'
  if(!is.null(vpredict)){
    vptxt2<-sapply(1:length(vpredict), function(x) vpredict[x])
  }
  vptxt<-c(vptxt,vptxt2)
  
  file.copy(es0.file,'temp.es',overwrite=TRUE)
  
  estxt<-c(qualF,estxt,lmtxt,predtxt)#,vptxt)
  estxt0<-gsub(':','.',estxt)
  estxt<-c(estxt0,vptxt)
                   
  if(!is.null(selfing)) {
    #es0.file='pine_provenance.es0'
    tempf <- base::readLines(es0.file)
    #selfing=0.5
    selfq<-paste0('!PARTIALSELF ',selfing)
    tempf<-gsub('!PARTIALSELF 0',selfq,tempf)
    utils::write.table(tempf,file='temp.es',quote=FALSE,
                       row.names=FALSE,col.names=FALSE,append=FALSE)
  }else file.copy(es0.file,'temp.es',overwrite=TRUE)                 
                   
  utils::write.table(estxt,file='temp.es',quote=FALSE,
              row.names=FALSE,col.names=FALSE,append=TRUE)
  
  if(!is.null(jobqualf)){
    write(jobqualf,file='temp')
    file.append('temp','temp.es')
    file.copy('temp','temp.es',overwrite=TRUE)
    file.remove('temp')
  }
  
  #dir()
  esjob<-'temp.es'
  runes<-paste(Echsf,esjob,sep=' ')
  
  system(runes,show.output.on.console=FALSE) # run program
  
  es0.path<-getwd()
  df<-AFEchidna::esRT(es0.path,trace=FALSE,mulT=mulT,met=met,
                      cycle=cycle,vpredict=vpredict)
  
  ## Iteration procedure
  if(trace==TRUE) {
    if(cycle==FALSE){
      cat('\n',df$StartTime,'\n')
      if(!is.null(family)|met==TRUE) cat(df$Iterations00,'\n')
       else print.data.frame(df$Iterations)
      cat(df$FinishAt,'\n\n')
    }
    if(cycle==TRUE){
      nn<-length(df$esr.all)
      trt<-unlist(lapply(df$esr.all,function(x) x$Traits))
      
      for(i in 1:nn){
        cat('\n\nIteration procedure for trait: ',trt[i],'\n')
        cat('\n',df$esr.all[[i]]$StartTime,'\n')
        print.data.frame(df$esr.all[[i]]$Iterations)
        cat(df$esr.all[[i]]$FinishAt,'\n')
      }
    }
    if(batch==TRUE){
      
      trt<-unlist(lapply(df$res.all,function(x) x$Traits))
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      
      
      for(i in 1:length(trt)) {
        
        cat('\n\nIteration procedure for trait: ',trt[i],'\n')
        cat('\n',df$res.all[[i]]$StartTime,'\n')
        print.data.frame(df$res.all[[i]]$Iterations)
        cat(df$res.all[[i]]$FinishAt,'\n')
      }
    }
  }
  
  flst<-dir()
  dlst<-base::setdiff(flst,flst0)
  esv<-flst[grep('\\.esv$',flst)]
  dlst<-dlst[dlst!=esv]
  #dlst<-dlst[!grep('\\.esv$',dlst)]
  
  if(delf==TRUE) file.remove(dlst)
  if(delf==FALSE) {
    if(!is.null(foldN))  new_dir<-foldN
    if(is.null(foldN)) {
      if(cycle==FALSE) new_dir <-paste(trait,'result',sep='.') else new_dir<-'cyl.result'
    } 
    if(!dir.exists(new_dir)) dir.create(new_dir)
    for(file in dlst) file.copy(file,new_dir, overwrite=TRUE)
    
    file.remove(dlst)
  }
  
  dlst0<-dir(".", pattern="^temp")
  dlst0<-dlst0[!grepl('\\.esv$',dlst0)]
  file.remove(dlst0)
  #file.remove('fort.13')
  
  return(df)
}

## transform for initial values in G and R
#' @export
init.tr <- function(random1) {
  random2<-strsplit(random1[2],'\\+')[[1]]
  random2<-gsub(',',' ',random2)
  random2<-gsub('init',' ',random2)
  random2<-gsub('\\)\\)','\\)',random2)
  random2<-gsub('c\\(','',random2)
  random2<-gsub('=','',random2)
  random2<-paste(random2,collapse = ' ')
  
  return(random2)
}

## ==========================
#' @export
update <- function(object,trait=NULL,fixed=NULL,
                   random=NULL,residual=NULL,
                   predict=NULL,vpredict=NULL,
                   qualifier=NULL,jobqualf=NULL,
                   trace=NULL,maxit=30,
                   selfing=NULL,mu.delete=FALSE,
                   mulT=NULL,met=NULL,
                   cycle=NULL,softp=NULL,
                   batch=NULL,mulN=NULL, 
                   batch.G=NULL,batch.R=NULL,
                   delf=NULL,foldN=NULL,...){
  UseMethod("update",object)
}
#' @rdname  AF.Echidna
#' @method  update esR
#' @export  update.esR
#' @export
update.esR<-function(object,trait=NULL,fixed=NULL,
                     random=NULL,residual=NULL,
                     predict=NULL,vpredict=NULL,
                     qualifier=NULL,jobqualf=NULL,
                     trace=NULL,maxit=30,
                     selfing=NULL,mu.delete=NULL,
                     mulN=NULL,mulT=NULL,met=NULL,
                     batch=NULL, 
                     batch.G=NULL,batch.R=NULL,
                     delf=NULL,foldN=NULL,...){
  #object<-res21
  org.par<-object$org.par
  
  es0.file<-org.par$es0.file
  
  if(is.null(trait))    trait<-org.par$trait
  if(is.null(fixed))    fixed<-org.par$fixed
  if(is.null(random))   random<-org.par$random
  if(is.null(residual)) residual<-org.par$residual
  
  if(is.null(predict))   predict<-org.par$predict
  if(is.null(vpredict))  vpredict<-org.par$vpredict
  if(is.null(qualifier)) qualifier<-org.par$qualifier
  if(is.null(jobqualf))  jobqualf<-org.par$jobqualf
  if(is.null(selfing))   selfing<-org.par$selfing
  if(is.null(mu.delete))  mu.delete<-org.par$mu.delete

  if(is.null(delf))    delf<-org.par$delf
  if(is.null(trace))   trace<-org.par$trace
  if(is.null(foldN))   foldN<-org.par$foldN
  if(is.null(batch))   batch<-org.par$batch
  if(is.null(mulN))    mulN<-org.par$mulN
  if(is.null(batch.G)) batch.G<-org.par$batch.G
  if(is.null(batch.R)) batch.R<-org.par$batch.R
  
  if(is.null(mulT))     mulT<-org.par$mulT
  if(is.null(met))      met<-org.par$met
  if(is.null(cycle))    cycle<-org.par$cycle
  if(is.null(softp))    softp<-org.par$softp
  
  AFEchidna::echidna(es0.file=es0.file,
                     softp=softp,
                     trait=trait,
              fixed=fixed,random=random,residual=residual,
              mulT=mulT,met=met,cycle=cycle,
              predict=predict,vpredict=vpredict,
              qualifier=qualifier,jobqualf=jobqualf,
              trace=trace,maxit=maxit,selfing=selfing,
              mu.delete=mu.delete,
              batch=batch,mulN=mulN,
              batch.G=batch.G,batch.R=batch.R,
              delf=delf,foldN=foldN)
  
}
                         
######### subset function for MET 
#' @rdname  AF.Echidna
#' @usage subF(fixed,random,residual,es0.file,
#'             subV.org, subV.nL,subV.new,mulN,res.n0)
#' 
#' @export
# case: https://blog.csdn.net/yzhlinscau/article/details/127440289?spm=1001.2014.3001.5501
#
subF <- function(fixed=NULL, random=NULL, residual=NULL,es0.file,
                 subV.org, subV.nL,subV.new,mulN=2,res.n0=NULL) {
                 
  cat('Starting analysis.\n')
  cc<-1:subV.nL
  bb <- utils::combn(cc,mulN)
  if(is.null(res.n0)) bbn <- ncol(bb) 
  else bbn <-res.n0
  
  res<-vector("list", bbn)
  
  res<-lapply(1:bbn, function(x){
    #cc<-paste0('Site-',bb[1,x],':',bb[2,x])
    subsetcc<-paste0('!subset ',subV.new, subV.org,' ',bb[1,x],' ',bb[2,x])
    mm <- echidna(fixed=fixed,
                  random=random,
                  residual=residual,
                  qualifier = subsetcc,
                  trace=F,
                  es0.file = es0.file)
    })
  
  names(res)<- lapply(1:bbn, function(x) paste0('Site-',bb[1,x],':',bb[2,x]))
  cat('works done.\n')
  res
}                                                  
                         

## batch results to each-single result
#' @usage b2s(object)
#' @rdname  AF.Echidna
#' @export
b2s<-function(object){
  #object<-res22
  
  org.par<-object$org.par
  org.par$batch<-FALSE
  
  data<-object$res.all
  trt<-names(data)
  trtN<-length(trt)
  
  #res<-list();length(res)<-trtN
  res<-vector("list", trtN)
  
  res<-lapply(1:trtN, function(x){
    xx<-data[[x]]
    xx$org.par<-org.par
    class(xx)<-'esR'
    xx
  })
  names(res)<-trt
  #class(res[[1]])
  return(res)
}



#' @export
vcms.fun <- function(aa1,mulN) {
  
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  
  mat1<-diag(mulN)
  df.mat1<-reshape::melt(lower.tri(mat1,diag=TRUE))
  
  for(i in 1:2) {
    #df.mat0[,i]<-paste0('t',df.mat0[,i])
    df.mat1[,i]<-paste0('t',df.mat1[,i])
  }

  
  df.mat11<-df.mat1 %>% dplyr::filter(value==TRUE) %>% 
    dplyr::arrange(X1) %>% tidyr::unite('tt',X1:X2,sep='.',remove=F)
  ttn<-df.mat11$tt
  
  
  ## diag
  ttn0<-paste0('t',1:mulN)
  aa1[grepl('units\\.diag\\(Trait\\)',aa1)]<-paste0('units_',ttn0)# resid
  
  vcn.gen<-aa1[grepl('diag\\(Trait\\)\\.',aa1)]  ## genetic
  vgn<-sub('diag\\(Trait\\)\\.','',vcn.gen[1])
  vgn<-sub(';diag\\(Trait\\)','',vgn)
  aa1[grepl(';diag\\(Trait\\)',aa1)]<-paste(vgn,ttn0,sep='_')## genetic
  
  ## corgh
  aa1[grepl('units\\.corgh\\(Trait\\)',aa1)]<-paste0('units_',ttn0)# resid
  
  vcn.gen<-aa1[grepl('corgh\\(Trait\\)\\.',aa1)]  ## genetic
  vgn<-sub('corgh\\(Trait\\)\\.','',vcn.gen[1])
  vgn<-sub(';corgh\\(Trait\\)','',vgn)
  aa1[grepl(';corgh\\(Trait\\)',aa1)]<-paste(vgn,c('corr',ttn0),sep='_')## genetic
  
  # unstructure: us
  aa1[grepl('units\\.us\\(Trait\\)',aa1)]<-paste0('units_',ttn)# resid
  
  vcn.gen<-aa1[grepl('us\\(Trait\\)\\.',aa1)]  ## genetic
  vgn<-sub('us\\(Trait\\)\\.','',vcn.gen[1])
  vgn<-sub(';us\\(Trait\\)','',vgn)
  aa1[grepl(';us\\(Trait\\)',aa1)]<-paste(vgn,ttn,sep='_')## genetic
  
  return(aa1)
}


#' @rdname  AF.Echidna
#' @usage esRT(path,trace=FALSE,mulT=FALSE,met=FALSE,cycle=FALSE) 
#' @export
esRT<-function(path,trace=FALSE,mulT=FALSE,met=FALSE,cycle=FALSE,vpredict=NULL){
  suppressMessages(esRT0(path=path,trace=trace,mulT=mulT,met=met,
                         cycle=cycle,vpredict=vpredict))
} 
  

#' @export
esRT0 <- function(path,trace=FALSE,mulT=FALSE,met=FALSE,
                  cycle=FALSE,vpredict=NULL) {
  
  if(!require(readr,warn.conflicts=FALSE,quietly=TRUE))
    stop('Need package: readr.\n')
  if(!require(stringr,warn.conflicts=FALSE,quietly=TRUE))
    stop('Need package: stringr.\n')

  require(readr,warn.conflicts=FALSE,quietly=TRUE)

  setwd(path)

  flst<-dir()

  if(any(grepl('_e.R$',flst))){
    Rresf<-flst[grep('_e.R$',flst)]

    ff<-readr::read_file(file=Rresf)
    ff<-paste0('mm=function(){',ff,'}')
    if(grepl('Vpredict',ff))  ff<-sub('Vpredict','if(0==1) Vpredict',ff)
    readr::write_file(ff,'ff1.R')
    tt<-source('ff1.R')$value()
    file.remove('ff1.R')

    aa<-strsplit(tt$FinishAt,'LogL ')[[1]][2]
    if(aa=="Converged") tt$Converge<- TRUE else tt$Converge<- FALSE
  } else tt<-list()

  tt<-tt[!sapply(tt, is.null)]

  ## input .esr results
  if(length(grep('\\.esr$',flst))==1) {
    esrf<-flst[grep('\\.esr$',flst)]
    esr<-readr::read_file(file=esrf)
    
    if(!any(grepl('_e.R$',flst))&cycle==FALSE) tt<-AFEchidna::esr.res(esr, mulT=mulT, met=met)
      
    tt$esr<-esr
  }

  ## fixed and random effects
  if(length(grep('\\.ess$',flst))==1) {
    essf<-flst[grep('\\.ess$',flst)]
    if(cycle==TRUE) tt$coef<-base::readLines(essf)
     else {
       
       coef<-base::readLines(essf)
       skipn<-grep('at\\(',coef)
       
       ## new here
       if(length(skipn)!=0) 
        df <- readr::read_lines(file=essf,skip=skipn)
       else df <- readr::read_lines(file=essf)
       
       dfL<-vector('list',length(df))
       
       for(i in 1:length(df))
         dfL[[i]]<-strsplit(df[i],'\\,\\s+')[[1]]
       
       df0<-do.call('rbind',dfL)
       df1<-as.data.frame(df0[-1,])
       names(df1)<-df0[1,]
       
       tt$coef <- df1
       rm(df,df0,df1,dfL)
       ## new here
       
     #  if(length(skipn)!=0) tt$coef <- utils::read.csv(file=essf,header=TRUE,skip=skipn)
     #    else tt$coef <- utils::read.csv(file=essf,header=TRUE)
     }
  }


  ## model diagnosis for residuals
  if(length(grep('\\.esy$',flst))==1) {
    esyf<-flst[grep('\\.esy$',flst)]
    if(cycle==TRUE) tt$yht<-base::readLines(esyf) 
     else tt$yht<-utils::read.csv(file=esyf,header=TRUE)
  }else{
    if(mulT==TRUE) 
      warning('.esy file not exists.Please use !YHT to generate it.')
  }

  ## input predict results
  if(length(grep('\\.epv$',flst))==1) {
    epvf<-flst[grep('\\.epv$',flst)] 
    if(cycle==TRUE) tt$pred<-readr::read_file(file=epvf)
     else tt$pred<-base::readLines(epvf)
  }

  ## input vpredict results
  if(length(grep('\\.evp$',flst))==1) {
    evpf<-flst[grep('\\.evp$',flst)]
    evp<-readr::read_file(file=evpf)
    evp0<-base::readLines(evpf)
    tt$evp<-evp
    tt$evp0<-evp0
  }

  ## input var.comp results
  if(length(grep('\\.vpc$',flst))==1) {
    vpcf<-flst[grep('\\.vpc$',flst)]
    vpc<-utils::read.table(file=vpcf,header=F)
    names(vpc)<-c('Vc','Term')
    tt$vpc<-vpc
  }
  
  #flst<-dir()
  ## input variances of var.comp
  if(length(grep('\\.vpv$',flst))==1) {
    vpvf<-flst[grep('\\.vpv$',flst)]
    vpv<-scan(file=vpvf,quiet=TRUE)
    #tt$vpv<-vpv

    if(cycle==FALSE){
      vpv.mat<-diag(1,nrow=nrow(vpc))
      vpv.mat[upper.tri(vpv.mat,diag=TRUE)]<-vpv
      a<-vpv.mat
      a[lower.tri(a)]<-t(a)[lower.tri(a)]
      rownames(a)<-colnames(a)<-paste0('V',1:nrow(vpc))
      vpv.mat<-a
      tt$vpv.mat<-vpv.mat
    } else tt$vpv<-vpv

  }
  
  if(cycle==TRUE){### !cycle
    res<-tt#<-res13
    
    temp<-list()
    
    ##1 esr
    esrr<-strsplit(res$esr,'\\s+?Echidna\\s+')[[1]]
    esrr<-esrr[esrr!=""]
    esrr<-paste('Echidna',esrr,sep=' ')
    
    # trait number
    trtn<-length(esrr)
    
    for(i in 1:trtn) temp[[i]]<-esrr[i]
    
    esr.all <- lapply(temp,esr.res)
    
    # trait names
    trt<-lapply(esr.all,function(x) x$Traits)
    trt<-unlist(trt)
    
    ##2 evp
    temp<-NULL
    
    evpr<-strsplit(res$evp,'\\s+?Warning:\\s+')[[1]]
    evpr<-evpr[evpr!=""]
    
    for(i in 1:trtn) temp[[i]]<-evpr[i]
    evp.all <- lapply(temp,evp.res)
    
    rm(temp)
    
    ##3 vpc
    vpc<-res$vpc
    
    termn<-nrow(vpc)/trtn
    vpc.all<-split(vpc, ceiling(seq_along(vpc$Vc)/termn))
    
    
    ##4 vpv
    vpv<-res$vpv
    matn<-termn*(termn+1)/2
    
    vpv.all<-split(vpv, ceiling(seq_along(vpv)/matn))
    
    names(esr.all)<-trt
    names(evp.all)<-trt
    names(vpc.all)<-trt
    names(vpv.all)<-trt
    
    res$esr.all<-esr.all
    res$evp.all<-evp.all
    res$vpc.all<-vpc.all
    res$vpv.all<-vpv.all
    
    tt<-res
  }

  return(tt)
}


#' @export
esr.res <- function(esr, mulT=FALSE,met=FALSE) {
    require(stringr,warn.conflicts=FALSE,quietly=TRUE)
  
    tt<-list()
    
    #esr<-res$esr
    
    vpatt<-'(Echidna\\s+.*Windows)'
    tt$Version<-stringr::str_extract(string =esr, pattern = vpatt)[[1]]
    
    jpatt<-'(?<=TITLE:\\s).+(.*)'
    tt$job<-stringr::str_extract(string =esr, pattern = jpatt)[[1]]
    
    #cat(heads)
    st.patt <- "(?<=at\\s).+(\\d+:\\d+:\\d+\\s\\d+)"
    tt$StartTime <- stringr::str_extract(string =esr, pattern = st.patt)[[1]]
    
    It.patt <- "(\\s+.*LogL=.*\\sDF.*)"
    Iter <- stringr::str_extract_all(string =esr, pattern = It.patt)[[1]]
    #cat(Iter)
    tt$Iterations00<-Iter
    Iterv<-as.numeric(unlist(regmatches(Iter,
                                        gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
                                                 Iter, perl=TRUE))))
    #if(family ==" !poisson !log !disp 1")
    
    if(mulT==FALSE){
      Iterations <- data.frame(matrix(Iterv,ncol=4,byrow=TRUE))
      names(Iterations)<-c("Iteration","LogL","eSigma","NEDF")
    }
    if(mulT==TRUE|met==TRUE){
      Iterations <- data.frame(matrix(Iterv,ncol=3,byrow=TRUE))
      names(Iterations)<-c("Iteration","LogL","NEDF")
    }
    
    tt$Iterations<-Iterations
    
    tt$LogLikelihood<-utils::tail(Iterations$LogL,1)
    tt$Residual.DF <-utils::tail(Iterations$NEDF,1)
    tt$maxit<-nrow(Iterations)
    
    IC.patt<-'(Akaike\\s+.*)|(Bayesian\\s+.*)'
    IC<-stringr::str_extract_all(string =esr, pattern = IC.patt)[[1]]
    dd<-as.numeric(unlist(regmatches(IC,
                                     gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
                                              IC, perl=TRUE))))
    tt$AkaikeIC<-dd[1];tt$BayesianIC<-dd[3]
    tt$ICparameter.Count<-dd[2]
    
    Tr.patt<-'(?<=Analysis of\\s).+(.*)'
    tt$Traits<-stringr::str_extract_all(string =esr, pattern = Tr.patt)[[1]]
    
    #if(crayon) cat(blue(heads)) else cat(heads)
    
    heads<-strsplit(esr,'\n Akaike Information Criterion')[[1]][1]
    
    mids<-strsplit(esr,'Akaike Information Criterion')[[1]][2]
    mids1<-paste0(' Akaike Information Criterion',mids)
    #cat(mids1)
    
    mids2<-strsplit(mids1,'Finished: ')[[1]][1]
    mids2a<-strsplit(mids2,'\r\nCovariance')[[1]][1]
    #cat(mids2a)
    tt$keyres<-mids2a
    
    # wald parts
    mids2b<-strsplit(mids2a,'\n Model_Term')[[1]][1]
    tt$waldT<-paste0('\t\t\tWald',strsplit(mids2b,'Wald')[[1]][2])
    #cat(tt$waldT)
    
    # varcomp parts
    mids3<-strsplit(mids2a,'\n Model_Term')[[1]][2]
    mids3a<-paste0(' Model_Term',mids3)
    #cat(mids3a)
    tt$components<-mids3a
    
    vc.patt<-"[-+]?\\d+\\.\\d+.*"
    vct<-stringr::str_extract_all(string =mids3a, pattern = vc.patt)[[1]]

    
    ft.patt <- '(?<=Finished:\\s).+(.*Converged)'
    tt$FinishAt<-stringr::str_extract(string =esr, pattern = ft.patt)[[1]]
    
    #tt='Finished: Wed Dec 30 13:05:02 2020 LogL NOT Converged  teaching'
    cg.patt<-"(LogL.*Converged)"
    aa<-stringr::str_extract(string =tt$FinishAt, pattern = cg.patt)[[1]]
    if(aa=="LogL Converged") tt$Converge<- TRUE else tt$Converge<- FALSE
    
  return(tt)
}  

#' @export
evp.res <- function(evp) {
  require(stringr,warn.conflicts=FALSE,quietly=TRUE)
  #evp<-gsub('Vmat 2','Vmat.2',evp)
  #evp<-mm$evp
  evp1<-evp
  
  evp1<-strsplit(evp1,'W components')[[1]][1]
  #cat(evp1)
  
  pattern<-"(\\s+.\\d+.\\w+.*\\d+.)"
  xx<-stringr::str_extract_all(string =evp1, pattern = pattern)[[1]]
  df<-utils::read.table(text=xx,header=FALSE)

  names(df)<-c('NO','Term','Sigma','SE')
  
  return(df)
}
  
#======================================================
#' @title output model diagnose results
#'
#' @description
#' \code{plot} This function output model diagnose results.
#'
#' @details
#' Test trait's norm for Echidna object,similar to asreml.
#' @aliases plot
#' @param object an object of Echidna-R result.
#' @param idx  trait order(1,2,...) when use !cycle in Echidna.
#' @param mulT	  multi-trait model,FALSE(default). 
#' @param meanN  make hat(y) higher than 4*mean(y) to NA.
#'
# @export
# plot <- function(object,...) {
#   UseMethod("plot")
# }
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' @name  plot
#' @examples
#'
#' \dontrun{
#' library(AFEchidna)
#'
#'  ##  Echidna
#'  path='D:/Echidna/Jobs/METb11'
#'
#'  #  mainly works for '_e.R'
#'  res<-esRT(path=path,trace=T) # for single trait
#'
#'  names(res)
#'  class(res)
#'
#'  # check .esy exist or not
#'  plot(res)
#' }
#'
#' @export
plot <- function(object,idx=NULL,mulT=FALSE,meanN=2.5){
  UseMethod("plot",object)
}
#' @method  plot esR
#' @export  plot.esR
#' @rdname  plot
#' @export
#'
plot.esR <- function(object,idx=NULL,mulT=FALSE,meanN=2.5) {
  #object<-res
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$yht)) stop('Please use !view in Echidna.')
    
    if(mulT==TRUE){# mulT=T, each trait y: y-resid, x: y-hat
      plot1f(object,mulT=TRUE,meanN=2.5)
    }else {
      
      if(is.null(object$esr.all)){ # for single-trait
        if(!is.null(object$Traits)){ 
          title<-paste0('Plots for trait: ',object$Traits)
          
          plot1f(object$yht,title=title,meanN=meanN)
        }
      }else{ # for !cycle single-trait
        df<-list()
        
        trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
        
        yhtt<-object$yht
        aa<-which(grepl('(Record,\\s+.*)', yhtt))
        
        for(i in 1:(length(aa)-1)){
          df[[i]]<-utils::read.table(text=yhtt[aa[i]:(aa[i+1]-1)],header=TRUE,sep=',')
          attr(df[[i]],'terms')<-trt[i]
        }
        df[[length(aa)]]<-utils::read.table(text=yhtt[aa[length(aa)]:length(yhtt)],
                                     header=TRUE,sep=',')
        attr(df[[length(aa)]],'terms')<-trt[length(aa)]
        
        names(df)<-trt
        
        if(is.null(idx)) lapply(df,plot1f) 
         else lapply(df[idx],plot1f)
        
      }
    }
  }
  
  if(object$org.par$batch==TRUE){
    mulT<-object$org.par$mulT
    
    dat<-lapply(object$res.all,function(x) x$yht)
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    names(dat)<-trt
    trtN<-length(trt)
    ## single trait batch
    if(mulT==FALSE){
      
      title<-paste0('Plots for trait: ',trt)
      if(is.null(idx)) lapply(1:trtN,function(x) plot1f(dat[[x]],title=title[x]))
      else plot1f(dat[[idx]],title=title[idx])
    }else{
      
      #dat0<-list();length(dat0)<-trtN
      dat0 <- vector("list", trtN)
      for(i in 1:trtN){
        dat0[[i]]<-list(yht=dat[[i]],Traits=trt[i])
      }
      
      if(is.null(idx)) lapply(1:trtN,function(x) plot1f(dat0[[x]],mulT=TRUE,meanN=2.5))
      else plot1f(dat0[[idx]],mulT=TRUE,meanN=2.5)
    }
  }
  
}



#' @export
plot1f <- function(object,title=NULL,mulT=FALSE,meanN=2.5) {
  
  dat<-object
  
  if(mulT==FALSE){ # single trait
    if(!is.null(attr(dat,'terms'))) 
      title<-paste0('Plots for trait: ',attr(dat,'terms'))
    else title<-title
    
    graphics::par(mfrow=c(2,2))
    
    dat$HatValue[dat$HatValue>meanN*mean(dat$HatValue,na.rm=T)]<-NA
    
    graphics::hist(dat$Residual,main='',xlab='Residuals',col='blue')
    
    stats::qqnorm(dat$Residual,main='',col='blue',ylab='Residuals')
    
    graphics::plot(Residual~HatValue,data=dat,xlab='Fitted',ylab='Residuals',col='blue')
    graphics::abline(h=0)
    
    graphics::plot(Residual~Record,data=dat,xlab='Unit Number',ylab='Residuals',col='blue')
    graphics::abline(h=0)
    
    if(!is.null(title)) graphics::mtext(title,side=3,line=-02,outer=TRUE)
    
    graphics::par(mfrow=c(1,1))
  } else{ # multT==T

    df<-dat$yht
    
    trt<-dat$Traits
    trt<-strsplit(trt,' ')[[1]]
    trtN<-length(trt)
    trtL<-trt#paste0('T',1:trtN)
    
    df$trait<-trtL

    library(dplyr,warn.conflicts=FALSE,quietly=TRUE)
    
    #df0<-list();length(df0)<-trtN
    df0 <- vector("list", trtN)
    for(i in 1:trtN){
      df0[[i]]<-df %>% dplyr::filter(trait==trtL[i]) %>% 
        dplyr::mutate(HatValue = ifelse(HatValue>meanN*mean(HatValue,na.rm=TRUE), NA, HatValue))
    }
    df<-do.call(rbind,df0)
    
    require(ggplot2,warn.conflicts=FALSE,quietly=TRUE)
    
    print(ggplot2::ggplot(df,ggplot2::aes(x=HatValue,y=Residual,colour=trait))+
            ggplot2::geom_point()+ggplot2::xlab('Fitted values')+
            ggplot2::facet_grid(.~trait,scales='free'))
    
  }

}
## =====================================

#' @title Count error for h2 and corr.
#'
#' @description
#' \code{pin} This function counts standard error(se) for heritability(h2) and
#' corr value and also outputs significent level for corr value in asreml and breedR package.
#'
#' @details
#' Count error for h2 and corr value, also outputs significent level.
#' @aliases  pin
#' @param object	 Echidna result object in R.
#' @param formula	 formula for h2 or corr.
#' @param idx	    trait order to run, 1,2...
#' @param mulp	  multi-formula for h2, corr, etc.
#' @param signif	 Index to output signif levels, FALSE(default) for non-signif.
#' @param digit	 Index for decimal number, 3(default).
#' @param Rres  Index(TRUE) to restore results, FALSE(default).
#' @param all	 Show variance component and genetic parmameter together, FALSE(default).
#  @param cycle  run models with qualifier !cycle.
#  @param org	 print original results by vpredict statements, FALSE(default).
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @rdname  pin
#' @examples
#' \dontrun{
#'  library(AFEchidna)
#'
#'  ##  Echidna
#'  path='D:/Echidna/Jobs/METb11'
#'
#'  rm(ls())
#'
#'  #  mainly works for '_e.R'
#'  res<-esRT(path=path,trace=T)# for single trait
#'  #res<-esRT(path=path,trace=T,mulT=T) # for multi-traits
#'  
#'  # pin results if using vpredict function
#'  pin(res)
#'  
#'  # run pin function to count genetic parameters
#'  pin11(res,h2~V1/(V1+V2))
#'  pin(res,mulp=c(h2~V1/(V1+V2),h2f~V1/(V1+V2/4))
#' }
#' @export
pin <- function(object,mulp=NULL,idx=1,digit=3,
                all=FALSE,signif=FALSE,Rres=FALSE){ 
  UseMethod("pin",object)
}
#' @rdname  pin
#' @method  pin esR
#' @export  pin.esR
#' @export
pin.esR <- function(object,mulp=NULL,idx=1,digit=3,
                    all=FALSE,signif=FALSE,Rres=FALSE){
  
  
  if(is.null(mulp)){
    
    if(object$org.par$batch==FALSE){
      if(!is.null(object$vpc.all)){
        #object<-res13
        
        vpc0<-object$vpc.all[[idx]]
        vpc<-do.call(rbind,object$vpc.all)#[[idx]]
        vpc$vcS<-paste0('V',1:nrow(vpc0))
        
        trt<-lapply(object$esr.all,function(x) x$Traits)
        trt<-unlist(trt)
        vpc$trait<-rep(trt,each=nrow(vpc0))
        
        row.names(vpc)<-NULL
        vpc$Term<-gsub('\\.',':',vpc$Term)
        
        print(vpc)
      } else {
        #object<-res11
        
        dat<-object$vpc #object$evp
        if(is.data.frame(dat)) {
          dat$vcS<-paste0('V',1:nrow(dat))
          dat$Term<-gsub('\\.',':',dat$Term)
          print.data.frame(dat)
        } else cat(dat)
      }
    }
    if(object$org.par$batch==TRUE){
      #object<-res21
      
      vpc0<-object$res.all[[idx]]$vpc
      vpc<-lapply(object$res.all,function(x) x$vpc)
      vpc<-do.call(rbind,vpc)#[[idx]]
      vpc$vcS<-paste0('V',1:nrow(vpc0))
      
      trt<-lapply(object$res.all,function(x) x$Traits)
      trt<-unlist(trt)
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      vpc$trait<-rep(trt,each=nrow(vpc0))
      
      row.names(vpc)<-NULL
      vpc$Term<-gsub('\\.',':',vpc$Term)
      
      print(vpc)
    } 
  } 
    
  if(!is.null(mulp)){
    if(object$org.par$batch==FALSE){
      if(is.null(object$vpc.all)){
        aa <- lapply(mulp,function(x){
          aa1 <- AFEchidna::pin11(object,formula=x,digit=digit,Rres=TRUE) 
        })
        res<-do.call(rbind,aa)
        res<-as.data.frame(res)
        
        res<-AFEchidna::fdata(res,faS=list(0,c(2:3)),FtN=TRUE)
        
        vc<-AFEchidna::Var(object)[,c('Term','Sigma','SE')]
        vc1<-vc
        vc1$vcS<-paste0('V',1:nrow(vc1))
        
        if(Rres==FALSE){
          cat("variance components are as following:\n")
          print.data.frame(vc1)
          cat('\npin formula: \n');for(i in 1:length(mulp)) print(mulp[[i]])
          cat('\n')
        }
        
        names(vc)<-c('Term','Estimate','SE')
        res<-res[,c('Term','Estimate','SE')]
        
        if(all==TRUE) res<-rbind(vc,res)
        
        if(signif==TRUE) res$Siglevel<-AFEchidna::sig.level(res$Estimate,res$SE)
        
        df<-AFEchidna::output(res,signif=signif,digit=digit,Rres=TRUE)
        
        if(Rres==FALSE){
          print.data.frame(df)
          
          if(signif==TRUE) {
            cat("---------------")
            cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
          }
        } else return(df)
        
      }else{ # for !cycle
        #object<-res13
        if(!is.null(object$org.par)) cycle<-object$org.par$cycle
        else cycle<-FALSE
        
        trt<-lapply(object$esr.all,function(x) x$Traits)
        trt<-unlist(trt)
        trtN<-length(trt)
        
        xxx<-lapply(1:trtN, function(x){
          xx<-lapply(mulp,function(y){
            x<-AFEchidna::pin11(object,idx=x,formula=y,digit=digit,all=all,Rres=TRUE) 
          })
          res<-do.call(rbind,xx)
          res<-as.data.frame(res)
          row.names(res)<-NULL
          res<-dplyr::distinct(res,Term,.keep_all = TRUE)
          #res[,-1]<-round(res[,-1],3) # digit=3
          #nres<-ncol(res)
          res<-AFEchidna::fdata(res,faS=list(0,c(2:3)),FtN=TRUE)
          if(signif==TRUE) res$Siglevel<-AFEchidna::sig.level(res$Estimate,res$SE)
          res
        })
        
        if(Rres==FALSE){
          cat('\nresults as following: \n')
          cat('\npin formula: \n');for(i in 1:length(mulp)) print(mulp[[i]])
          cat('\n')
        }
        
        names(xxx)<-trt
        
        vcms<-xxx[[1]][,1] # pin sign
        #aa1<-paste(paste0('V',1:length(aa1)),aa1,sep='-')
        if(cycle==TRUE)
          vcms<-paste(paste0('V',1:length(vcms)),vcms,sep='-',collapse='; ')
        else vcms<-paste(vcms,collapse='; ')
        
        if(Rres==FALSE) cat('terms: ',vcms);cat('\n\n')
        
        if(signif==FALSE)
          df<-do.call(rbind,lapply(xxx,function(x){unlist(x[,2:3])}))
        if(signif==TRUE) 
          df<-do.call(rbind,lapply(xxx,function(x){unlist(x[,2:4])}))
        
        df<-as.data.frame(df)
        if(length(mulp)==1) names(df)[1]<-'Estimate1'
        
        if(cycle==TRUE) names(df)<-gsub('Estimate','V',names(df))
        
        trt<-gsub(' ','-',trt)
        trt<-sub('-$','',trt)
        row.names(df)<-trt
        
        if(Rres==FALSE) {
          print.data.frame(df)
          if(signif==TRUE) {
            cat("---------------")
            cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
          }
        } else return(list(vcms=vcms,res=df))  
      } 
    }
      
  if(object$org.par$batch==TRUE){
      #object<-res22
    mulT<-object$org.par$mulT
    mulN<-object$org.par$mulN
      
      trt<-names(object$res.all)#,function(x) x$Traits)
      trt<-unlist(trt)
      trtN<-length(trt)
      
      ## problem here!!!!
      xxx<-lapply(1:trtN, function(x){
        #x=1
        xx<-lapply(mulp,function(y){ 
          x<-AFEchidna::pin33(fm=object$res.all[[x]],formula=y)
        })
        
        xx<-do.call(rbind,xx)
        
        fm<-object$res.all[[x]]
        #names(fm)
        vc<-AFEchidna::evp.res(fm$evp)
        vc1<-vc[,-1]
        names(vc1)[2]<-'Estimate'
        
        if(all==TRUE) res<-rbind(vc1,xx)
         else res<-xx
        res<-dplyr::distinct(res,Term,Estimate,.keep_all = TRUE)
        res<-AFEchidna::fdata(res,faS=list(0,c(2:3)),FtN=TRUE)
        res[,2:3]<-round(res[,2:3],digits=digit) # first: Term
        if(signif==TRUE) res$Siglevel<-AFEchidna::sig.level(res$Estimate,res$SE)
        res
      })
      
      if(Rres==FALSE){
        cat('\nresults as following: \n')
        cat('\npin formula: \n');for(i in 1:length(mulp)) print(mulp[[i]])
        cat('\n')
      }
      
      names(xxx)<-trt
      
      vcms<-lapply(xxx,function(x) unlist(x[,1]))[[1]]
      if(mulT==TRUE) vcms<-AFEchidna::vcms.fun(vcms,mulN)
      vcms<-paste(paste0('V',1:length(vcms)),vcms,sep='-',collapse='; ')
      
      if(Rres==FALSE) cat('terms: ',vcms);cat('\n\n')
      
      if(signif==FALSE)
        df<-do.call(rbind,lapply(xxx,function(x){unlist(x[,2:3])}))
      else df<-do.call(rbind,lapply(xxx,function(x){unlist(x[,2:4])}))
      
      df<-as.data.frame(df)
      if(length(mulp)==1) names(df)[1]<-'Estimate1'
      
      names(df)<-gsub('Estimate','V',names(df))
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      if(mulT==FALSE) trt<-gsub('-','',trt)
      row.names(df)<-trt
      
      if(Rres==FALSE) {
        print.data.frame(df)
        if(signif==TRUE) {
          cat("---------------")
          cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
        }
      } else return(list(vcms=vcms,res=df)) 
    }
  }
}


#' @usage 
#'       pin11(object,formula=NULL,idx=1,signif=FALSE,
#'                 digit=3,all=FALSE,Rres=FALSE)
# @method  pin11
#' @rdname  pin
#' @export  pin11
#' @export
pin11 <-function(object,formula=NULL,idx=1,signif=FALSE,
                       digit=3,all=FALSE,Rres=FALSE){ 

  #object<-res11 

  if(!require(msm,warn.conflicts=FALSE,quietly=TRUE)){stop('Need package: msm.\n')}
  require(msm,warn.conflicts=FALSE,quietly=TRUE)
  
  if(is.null(formula)) {
    if(object$org.par$batch==FALSE){
      # original variance components
      #if(cycle==TRUE){
      if(!is.null(object$vpc.all)){  
        vpc<-object$vpc.all[[idx]]
        vpc$vcS<-paste0('V',1:nrow(vpc))
        print(vpc)
      } else {
        dat<-object$vpc #object$evp
        if(is.data.frame(dat)) {
          dat$vcS<-paste0('V',1:nrow(dat))
          print(dat)
        } else cat(dat)
      }
    }
    
    if(object$org.par$batch==TRUE){
      var1<-AFEchidna::evp.res(data[[1]]$evp)
      var1$vcS<-paste0('V',1:nrow(var1))
      print(var1)
    }
  }
  
  if(!is.null(formula)) {
    #object<-res11
    
    if(object$org.par$batch==FALSE){
      if(!is.null(object$vpc.all)){
        #idx=1
        #object<-res21
        if(!is.null(object$org.par)) cycle<-object$org.par$cycle
        else cycle<-FALSE
        
        vpc<-object$vpc.all[[idx]] # vc, data frame
        vpv<-object$vpv.all[[idx]] # vpv, vector
        if(cycle==TRUE) var<-object$evp.all[[idx]] # Vc with SE !!cycle
        else var<-AFEchidna::Var(object$esr.all[[idx]])
        #class(object$esr.all[[idx]])
        
        trt<-unlist(object$esr.all[[idx]]$Traits)
        if(Rres==FALSE) cat('Analysis trait is:',trt,'\n\n')  ##??
        
        # vc
        vc<-vpc[,1]
        
        # vvc.mat
        if(is.matrix(vpv)) vpv.mat<-vpv ###!!!
        else{
          vpv.mat<-diag(1,nrow=length(vc))
          vpv.mat[upper.tri(vpv.mat,diag=T)]<-vpv
          a<-vpv.mat
          a[lower.tri(a)]<-t(a)[lower.tri(a)]
          vpv<-a
        }
        
      } else{
        
        if(is.null(object$vpv.mat)) 
          stop('Please use "W components" under "vpredict".')
        
        vpc<-object$vpc
        vc <- vpc[,'Vc']
        
        vpv.mat<-object$vpv.mat
      }
      
      result<-AFEchidna::pin33(vpc=vpc,vc=vc,vpv.mat=vpv.mat,formula=formula)
      
      if(is.null(object$vpc.all)){
        vc1<-AFEchidna::Var(object)[,c('Term','Sigma','SE')]
      } else vc1<-var[,c('Term','Sigma','SE')] ##??
      
      vc0<-vc1
      names(vc1)<-c('Term','Estimate','SE')
      vc0$vcS<-paste0('V',1:nrow(vc0))
      
      if(Rres==FALSE){
        cat("variance components are as following:\n")
        print.data.frame(vc0)
        cat('\npin formula: ');print(formula)
        cat('\n\n')
      }
      if(all==TRUE) result<-rbind(vc1,result)
      
    }
    
    if(object$org.par$batch==TRUE){
      #object<-res21
      
        data<-object[[1]]
        
        #nn1<-nn2<-data.frame()
        NTrait<-names(data)
        NTrait<-gsub(' ','-',NTrait)
        NTrait<-sub('-$','',NTrait)
        
        res<-lapply(1:length(data), function(x){
          #x=1
          xx<-AFEchidna::pin33(fm=data[[x]],formula=formula)
          vc<-AFEchidna::evp.res(data[[x]]$evp)
          #names(vc)<-NTrait
          #vc1<-do.call(rbind,vc)
          vc1<-vc[,-1]
          names(vc1)[2]<-'Estimate'
          if(all==TRUE) xx<-rbind(vc1,xx)
          xx
        })
        names(res)<-NTrait
        result<-do.call(rbind,res)
        
      }
    result<-AFEchidna::fdata(result,faS=list(0,c(2:3)),FtN=TRUE)
    #result1<-result
    if(signif==TRUE) result$Siglevel<-AFEchidna::sig.level(result$Estimate,result$SE)
    
    AFEchidna::output(result,Rres=Rres,signif=signif,digit=digit) 
  }
}


#' @export
pin33 <- function(fm=NULL,vpc=NULL,vc=NULL,vpv.mat=NULL,
                  formula) {
  if(!is.null(fm)){
    vpc<-fm$vpc
    vc <- vpc[,'Vc']
    vpv.mat<-fm$vpv.mat
  }
 
  #formula<-h2~V2*4/(V2+V5)
  dd<-gsub('V','x',formula) # 'x'
  dd[2]<-as.character(formula[[2]])
  formula1<-stats::as.formula(paste(dd[2],dd[3],sep=' ~ '))
  
  transform<-formula1
  
  pframe <- as.list(vc)
  names(pframe) <- paste("x", seq(1, length(pframe)), sep = "") # 'x'
  tvalue<-eval(stats::deriv(transform[[length(transform)]], names(pframe)),pframe)
  tname <- if(length(transform)==3){transform[[2]]}else ""
  
  invAI <- vpv.mat
  se <- msm::deltamethod(transform,vc,invAI)
  
  result<-data.frame(row.names =tname, Estimate=tvalue, SE=se)
  result$Term<-rownames(result)
  result<-result[,c('Term','Estimate','SE')]
  rownames(result)<-NULL
  
  return(result)
}


#' @usage 
#'       pin22(...,mulp,signif=FALSE,digit=3)
#' @rdname  pin
#' @export  pin22
#' @export
pin22<-function(...,mulp,signif=FALSE,digit=3){
  args <- list(...)
  objs <- all.vars(match.call())
  
  for(i in 1:length(args)){
    cat("\npin results for: ", objs[i],'\n')

    AFEchidna::pin(args[[i]],mulp=mulp,signif=signif,digit=digit)

  }
  
}


# ------------------------------------
# sig.level functions
#' @export
sig.level <- function(tvalue,se,...){
  n <- length(se)
  siglevel <- 1:n
  for(i in 1:n){
    sig.se <- c(se[i]*1.645,se[i]*2.326,se[i]*3.090)
    # 1.450?

    if(abs(tvalue[i])>sig.se[3]) {siglevel[i] <- "***"}
    else if(abs(tvalue[i])>sig.se[2]) {siglevel[i] <- "**"}
    else if(abs(tvalue[i])>sig.se[1]) {siglevel[i] <- "*"}
    else {siglevel[i] <- "Not signif"}
  }
  return(siglevel)
}


#### output format
#' @export
output<-function(res,signif=TRUE,Rres=FALSE,digit=3){#
  if(Rres==FALSE){
    cat("pin results are as following:\n\n")
    if(signif==TRUE){
      if(is.data.frame(res)){
        #res$Signif<-AFfR::sig.level(res$Esmate,res$SE)
        print.data.frame(format(res, digits=digit,nsmall=digit))
      } else print(res)
      cat("---------------")
      cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")
    }else{
      if(is.data.frame(res)) print.data.frame(format(res, digits=digit,nsmall=digit))
      else print(res)
    }
    cat("\n")
  }  
  
  if(Rres==TRUE){
    if(is.data.frame(res)) return(format(res, digits=digit,nsmall=digit))
    else return(res)
  }
  
}

#### 
#' @title Model comparison for Echidna.
#' 
#' @description 
#' \code{model.comp} This function would compare models with different random structure 
#' under the same fixed factors.
#'  
#' @usage model.comp(...,LRT=FALSE,boundary = TRUE) 
#' 
#' @param ...	 A list with more than two Echidna-R results, such as "m1,m2,m3,m4".
#' @param LRT	 Value TRUE for Likelihood ratio test (LRT), default (FALSE) for no LRT. 
#' @param boundary	 boundary If \code{TRUE} (the default) hypothesized parameter
#' values being tested lie on the boundary of the parameter space.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' 
#' @examples
#' 
#' \dontrun{
#' library(AFEchidna)
#' 
#' es0.path="D:/teaching/"
#' 
#'               
#' #####   model comparison    #####
#' model.comp(m1,m2)
#' model.comp(m1,m2,LRT=TRUE)
#' }
#' 

#' @export
model.comp <- function(...,LRT=FALSE, boundary = TRUE)
{
  #args <- list(m1,m2)
  
    args <- list(...)
    n <- length(args)
    
    ## fixed models must be the same
    fixed.labels <- lapply(args, function(x) {
      
      if(class(x$org.par$fixed)=='formula'){
        tt<-as.character(x$org.par$fixed)[3]
        tt<-unlist(strsplit(tt,'\\+'))
        tt<-gsub('\\s+','',tt)
      } else tt<-x$org.par$fixed
      tt
    })
    
    batch.state <- sapply(args, function(x) {
      x$org.par$batch
    })
    
    if(stats::var(batch.state)==0&batch.state[1]==FALSE){
      
      ## check fixed
      trms <- length(unique(unlist(lapply(fixed.labels, function(x)x))))
      nt <- unlist(lapply(fixed.labels, function(x)length(x)))
      if(!all(nt == trms))
        stop("fixed models differ")
      
      
      pchisq.mixture <- function(x, n=2) {
        df <- 0:n
        mixprobs <- stats::dbinom(df, size=n, prob=0.5)
        p <- c()
        for (i in 1:length(x)){
          p[i] <- sum(mixprobs*pchisq(x[i], df))
        }
        p
      }
      
      objs <- all.vars(match.call())
      LL <- sapply(args, function(x)x$LogLikelihood)
      np <- sapply(args, function(x){
        x$ICparameter.Count})
      
      AIC<-sapply(args, function(x)x$AkaikeIC)
      BIC<-sapply(args, function(x)x$BayesianIC)
      
      tab0 <- matrix(nrow=n,ncol=5)
      tab0[,1] <- np
      tab0[,2] <- LL
      tab0[,3] <- AIC
      tab0[,4] <- BIC
      #tab0[,5] 
      tab0 <- data.frame(tab0)
      
      #tab0.ncol=5
      b1<- which.min(AIC)
      tab0[,5] <- ""
      tab0[b1,5] <- "better"
      b2 <- which.min(BIC)
      tab0[,6] <- ""
      tab0[b2,6] <- "better"
      
      attr(tab0, "names") <- c("parNO","LogL","AIC",'BIC','AIC.State','BIC.State')
      attr(tab0, "row.names") <- objs
      class(tab0) <- c("data.frame") #"anova",
      tab0<-tab0[base::order(tab0$parNO),]
      cat('\nModel comparison results as following:\n\n')
      print.data.frame(tab0)
      
      if(LRT==TRUE){
        cat("\n\n=====================================")
        cat("\nLikelihood ratio test (LRT) results:")
        cat('\nnote:left model before "/" is full model,right is reduced.\n\n')
        
        ## Increasing order of number or parameters
        idx <- base::order(np)
        D <- df <- prob <- numeric(n-1)
        models <- character(n-1)
        ## do successive pairs
        for(i in seq(1,n-1)) {
          ii <- idx[i]   # reduced
          jj <- idx[i+1] # full
          D[i] <- -2*(LL[ii]-LL[jj])
          df[i] <- np[jj]-np[ii]
          # constraint <- guzpfx(args[[jj]]$vparameters.con)
          # df[i] <- df[i] - sum(as.numeric(constraint == "F"))
          # df[i] <- df[i] - sum(as.numeric(constraint == "C"))
          # df[i] <- df[i] - sum(as.numeric(constraint == "S"))
          # constraint <- guzpfx(args[[ii]]$vparameters.con)
          # df[i] <- df[i] + sum(as.numeric(constraint == "F"))
          # df[i] <- df[i] + sum(as.numeric(constraint == "C"))
          # df[i] <- df[i] + sum(as.numeric(constraint == "S"))
          
          if(df[i] == 0)
            warning(ii," vs ",jj,": same number of parameters.")
          
          if(boundary) {
            if(df[i] == 1)
              prob[i] <- 0.5 * (1 - stats::pchisq(D[i],df[i]))
            else
              prob[i] <- 1 - pchisq.mixture(D[i],df[i])
          }
          else
            prob[i] <- 1 - stats::pchisq(D[i],df[i])
          
          models[i] <- paste(objs[jj],"/",objs[ii],sep="")
        }
        tab <- matrix(nrow=n-1,ncol=3)
        tab[,1] <- df
        tab[,2] <- D
        tab[,3] <- prob
        tab <- data.frame(tab)
        heading <- "Likelihood ratio test(s) assuming nested random models.\n"
        if(boundary)
          heading <- paste(heading,
                           "(See Self & Liang, 1987)\n",sep="")
        attr(tab, "heading") <- heading
        attr(tab, "names") <- c("df","LR-statistic","Pr(Chisq)")
        attr(tab, "row.names") <- models
        class(tab) <- c("anova","data.frame")
        return(tab)
        
      }

  } else{
    cat('not work for batch analysis!') ##!!!
  }
  #cat('\n')
}

#' @export
model.comp11<-function(mulM,LRT=FALSE,rdDF=FALSE,obL=23){
  # obL, each echdina results length
  
  #if(!require(dplyr,warn=F,quiet=T)) stop('Need package: dplyr.\n')
  #require(poorman,warn=F,quiet=T)
  
  # if(object$org.par$batch==TRUE){
  #   cat('not work for batch analysis!') ##!!!
  # }else{
    cat("Attension:\n")
    cat("Fixed factors should be the same!\n\n\n")
    
    Mnames <- vector()#<-NULL
    LogL <- parNo <- DF <- AIC<-BIC<-vector()
    Npm <- 0
    Nml<-mulM
    
    Nmls <- ceiling(length(Nml)/obL) 
    #LogL=Pm=Nedf=vector()
    for(i in 1:Nmls){
      DF[i] <- Nml[[6+(i-1)*obL]]  
      parNo[i] <- Nml[[9+(i-1)*obL]]
      LogL[i] <- Nml[[5+(i-1)*obL]] 
      AIC[i]  <- Nml[[7+(i-1)*obL]]
      BIC[i]  <- Nml[[8+(i-1)*obL]]
      
      Mnames <- all.vars(match.call())
    }
    #print(Mnames)
    
    df<- data.frame(DF=DF,parNO=parNo,LogL=LogL,AIC=AIC,BIC=BIC)
    
    #ifelse(mulM==TRUE,df$Model<-paste("m",1:Nmls,sep="")#, df$Model<-Mnames) 
    df$Model<-Mnames[1:Nmls]
    df<-df[,c(6,1:5)]
    df.ncol<-ncol(df)
    
    b1<- which.min(AIC)
    df$AIC.State <- ""
    df[b1,(df.ncol+1)] <- "better"
    b2 <- which.min(BIC)
    df$BIC.State <- ""
    df[b2,(df.ncol+2)] <- "better"
    df <- df[base::order(df$parNO),] #dplyr::arrange(df,parNo)
    
    #invisible(df)
    
    print.data.frame(df)
    cat("-----------------------------\n")
    cat("Lower AIC and BIC is better model.\n\n")
    
    A <- utils::combn(1:Nmls,2)
    B <- Nmls*(Nmls-1)/2
    
    if(LRT==TRUE){
      #cat("\n")
      cat("\nAttension: Please check every result's length is the same (default,23);\n")
      cat("if the length is less, put the object at the end of mulM.\n")
      cat("In the present, just allow one object's length less.")
      cat("\n=====================================")
      cat("\nLikelihood ratio test (LRT) results:\n\n")
      
      for(i in 1:B){
        if(B>1) df1 <- df[A[,i],1:4] else df1 <- df[1:2,1:4]
        df1 <- df1[base::order(df1$parNO),]
        #df1 <- dplyr::arrange(df1,parNo)
        DlogL <- df1$LogL[2]-df1$LogL[1]
        Ddf <- df1$parNO[2]-df1$parNO[1]
        
        pv<-ifelse(rdDF==TRUE,round(1-stats::pchisq(2*DlogL,Ddf-0.5),3),
                   round(1-stats::pchisq(2*DlogL,Ddf),3))
        
        # pv<-ifelse(rdDF==TRUE,.5*round(1-pchisq(2*DlogL,Ddf-0.5),3),
        #                .5*round(1-pchisq(2*DlogL,Ddf),3))
        
        df1$Ddf<-c(NA,Ddf)
        df1$DlogL<-c(NA,DlogL)
        df1$pv <- c(NA,pv)
        names(df1)[7] <- "Pr(Chisq)"
        
        #cat("\nModel compared between ",df1$Model[1],"--",df1$Model[2],":\n")
        # heading <-paste0("\nModel compared between ",df1$Model[1],"--",df1$Model[2],":\n")
        attr(df1, "row.names") <- df1$Model
        class(df1) <- c("anova","data.frame")
        # return(df1)
        cat('\n')
        print(df1[,-1])
        #cat("---------------")
        #cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
      }
      cat("=====================================")
      if(rdDF==TRUE){
        cat("\nAttension: Ddf=Ddf-0.5. \n")
        cat("When for corr model, against +/-1. \n\n")
      }else {
        cat("\nAttension: Ddf did not minus 0.5. \n")
        cat("When for corr model, against 0. \n\n")
      }
    }
  # }

}

## calculate accuracy for random effects
# ped is pedigree
# ran.eff is random effects
# Var is related variance

#' @usage raneff.acc(object,ran.eff,Var, ped=NULL)
#' @rdname  AF.Echidna
#' @export
raneff.acc <- function(object, ran.eff, Var, ped=NULL) {
  
  if(!require(dplyr,warn.conflicts=FALSE,quietly=TRUE)) 
    stop('Need package: dplyr.\n')
  if(!require(pedigree,warn.conflicts=FALSE,quietly=TRUE)) 
    stop('Need package: pedigree.\n')
  
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  
  #ran.eff<-ran.eff %>% filter(Term !='mv')
  
  if(object$org.par$batch==FALSE){
    if(is.null(ped)) ran.eff$Fi<- 0
    if(!is.null(ped)) {
      ped0<-ped
      names(ped)[1]<-'ID'
      ped1<-pedigree::add.Inds(ped)
      ped1$Fi<-pedigree::calcInbreeding(ped1)
      ped2<-ped1 %>% dplyr::filter(ID %in% ped$ID)
      ped0$Fi<-ped2$Fi
      
      ran.eff$Fi<- ped2 %>% 
        dplyr::filter(ID %in% ran.eff$Level) %>% 
        dplyr::select(Fi)
    }
    
    cat('Var is:',Var,'; Fi is inbreeding coefficient.')
    cat('\naccurancy formula: sqrt(1-SE^2/((1+Fi)*Var))\n') 
    ran.eff<- transform(ran.eff,accuracy=sqrt(1-SE^2/((1+Fi)*Var)))
    
    names(ran.eff)[ncol(ran.eff)]<-'accuracy'
    
    cat('The first six results:\n\n')
    print.data.frame(utils::head(ran.eff))
    cat('...\n\n')
    
    return(ran.eff)
  }
  

}

#=====================================
#' @export
predict <- function(object,...){
  UseMethod("predict",object)
}
#' @method  predict esR
#' @export  predict.esR
#' @rdname  AF.Echidna
#' @export

predict.esR <- function(object) {
  #object<-res11
  
  if(object$org.par$batch==FALSE){
    if(object$org.par$cycle==FALSE){
      
      preds<-AFEchidna::predict0(object)
       
    }else{
      
      trt<-names(object$esr.all)
      #preds<-list();length(preds)<-length(trt)
      preds <- vector("list", length(trt))
      
      pred<-object$pred
      
      #pred<-preds[[1]]
      predt<-unlist(strsplit(pred,'TITLE:'))
      #predt[1]
      predt<-predt[-1]# predt[predt!="  "]
      #length(predt)
      
      for(i in 1:length(trt)){
        #i=1
        readr::write_file(predt[i],'tmpf') #predt[i]
        #preds[[i]]<-mult.pred('tmpf')
        txt<-base::readLines('tmpf')
        
        a0<-which(grepl('(Predicted values\\s+.*)', txt))
        heads0<-txt[a0]
        
        aa<-which(grepl('(Prediction\\s+.*)', txt))
        bb<-which(grepl('(Average\\s+.*)', txt))
        #txt[bb]
        
        ased0<-as.numeric(unlist(regmatches(txt[bb],
                                            gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
                                                     txt[bb], perl=TRUE))))
        
        names(ased0)<-paste0('ased',1:length(bb))
        ased0<-as.list(ased0)
        
        #preds0<-list();length(preds0)<-length(aa)
        preds0 <- vector("list", length(aa))
        for(j in 1:length(aa)){
          preds0[[j]]<-utils::read.table(text=txt[aa[j]:(bb[j]-1)],header=T)
          preds0[[j]]<-list(pred=preds0[[j]],ased=ased0[[j]])
        }
        names(preds0)<-paste0('pred',1:length(aa))
        preds[[i]]<-list(heads=heads0,pred=preds0)
      }
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      names(preds)<-trt

    }
  }
  
  if(object$org.par$batch==TRUE){
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    #preds<-list();length(preds)<-length(trt)
    preds <- vector("list", length(trt))
    preds0<-preds
    
    #preds$pred<-lapply(object$res.all,function(x) x$pred)
    for(i in 1:length(trt)) {
      preds0[[i]]$pred<-object$res.all[[i]]$pred
      class(preds0[[i]])<-'esR'
    }
    #names(preds)<-trt
    
    preds<-lapply(preds0,predict0) ##???
    
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    names(preds)<-trt

  }
  return(preds)
}

#' @export
predict0<-function(object){
  if(is.null(object$pred)) stop('Please use predict statements.')
  
  txt<-object$pred
  
  head0<-which(grepl('(Predicted values\\s+.*)', txt))
  heads<-txt[head0]
  
  aa<-which(grepl('(Prediction\\s+.*)', txt))
  bb<-which(grepl('(Average\\s+.*)', txt))
  
  preds<-list()
  for(j in 1:length(aa)){
    preds[[j]]<-utils::read.table(text=txt[aa[j]:(bb[j]-1)],header=T)
  }
  names(preds)<-paste0('pred',1:length(aa))
  
  ased<-as.numeric(unlist(regmatches(txt[bb],
                                     gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
                                              txt[bb], perl=TRUE))))
  
  names(ased)<-paste0('ased',1:length(bb))
  ased<-as.list(ased)
  
  tt<-list(heads=heads,pred=preds,ased=ased)
  
  preds<-tt
  return(preds)
}


#' @export
coef <- function(object){
  UseMethod("coef",object)
}
#' @rdname  AF.Echidna
#' @method  coef esR
#' @export  coef.esR
#' @export

coef.esR <- function(object){
  #object<-HT
  
  if(class(object$org.par$random)=='formula'){
    random <- as.character(object$org.par$random)[2]
    if(grepl('\\*',random)) tt <- Var(object)$Term[Var(object)$Term!="Residual"]
      else tt <- random
    tt<-unlist(strsplit(tt,'\\+'))
    tt<-gsub('\\s+','',tt)
  } else tt <- object$org.par$random
  ranf <- tt
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$esr.all)){
      if(is.null(object$coef)) 
        stop('Please use qualifier !SLN.')
      
      trt <- object$Traits # AFEchidna::
      sol <- AFEchidna::coeff11(object$coef,ranf=ranf)
      
    }else{ # !cycle
      #object<-res13
      
      trt <- names(object$esr.all)#unlist(lapply(object$esr.all,function(x) x$Traits))
      #sol0 <- list();length(sol0) <- length(trt)
      sol0 <- vector("list", length(trt))
      
      coeft<-object$coef
      aa<-which(grepl('(Model_Term,\\s+.*)', coeft))
      
      for(i in 1:(length(aa)-1)){
        sol0[[i]]<-utils::read.table(text=coeft[aa[i]:(aa[i+1]-1)],
                              header=TRUE,sep=',')
      }
      sol0[[length(aa)]]<-read.table(text=coeft[aa[length(aa)]:length(coeft)],
                                    header=TRUE,sep=',')

      sol<-lapply(sol0,function(x) AFEchidna::coeff11(x,ranf=ranf))
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      names(sol)<-trt
    }
  }
  
  if(object$org.par$batch==TRUE){
    
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    trtN<-length(trt)
    #sol<-list();length(sol)<-trtN
    sol <- vector("list", trtN)
    sol0<-sol
    
    for(i in 1:trtN) {
      sol0[[i]]$coef<-object$res.all[[i]]$coef
      class(sol0[[i]])<-'esR'
    }

    sol<-lapply(1:trtN,function(x) AFEchidna::coeff11(sol0[[x]]$coef,ranf=ranf))
    
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    names(sol)<-trt

  }
 
  return(sol)
}


#' @export
coeff11 <- function(object,ranf) {
  
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  
  coeft<-object#$coef
  
  names(coeft)[1:4]<-c('Term','Level',"Effect",'SE')
  coeft$Term<-gsub('\\.',':',coeft$Term)
  coeft$Level<-gsub('\\t','',coeft$Level)
  coeft <-coeft %>% dplyr::filter(Term!='mv')
  
  ranf<-gsub('\\.',':',ranf)
  Term<-unique(coeft$Term)
  fixf<-base::setdiff(Term,ranf)
  
  fixeff<-AFEchidna::filterD1(coeft,Term %in% fixf) #AFEchidna::
  raneff<-AFEchidna::filterD1(coeft,Term %in% ranf)
  #head(raneff)
  
  attr(fixeff,'terms')<-'fixed'
  attr(raneff,'terms')<-'random'
  
  effects<-list(fixed=fixeff,random=raneff)
  
  return(effects)
}

#coef(res)$fixed
#coef(res)$random


#' @export
wald <- function(object,...){
  UseMethod("wald",object)
}
#' @method  wald esR
#' @export  wald.esR
#' @rdname  AF.Echidna
#' @export

wald.esR<-function(object) {
  
  #object<-mm
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$esr.all)){
      cat("\nWald tests for fixed effects.\n")
      if(!is.null(object$waldT)){
        cat(object$waldT)
       }
    }else{
      nn<-length(object$esr.all)
      trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
      for(i in 1:nn){
        cat("\nWald tests of fixed effects for trait: ",trt[i],'\n')
        cat(object$esr.all[[i]]$waldT,'\n')#names(object$esr.all[[1]])
        cat('\n')
      }
    }
  }
  
  if(object$org.par$batch==TRUE){
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    
    for(i in 1:length(trt)) {
      
      cat('\nWald tests of fixed effects for trait: ',trt[i],'\n')
      cat(object$res.all[[i]]$waldT,'\n')
      cat('\n')
    }
  }

}

##

#' @export
waldT <- function(object,...){
  UseMethod("waldT",object)
}
#' @method  waldT esR
#' @export  waldT.esR
#' @rdname  AF.Echidna
#' @export
waldT.esR<-function (object, term=NULL,ncol=NULL) 
{  
  if(is.null(term)){
    waldT2<-strsplit(object$waldT,'P-inc \r\n')[[1]][2]
    term<-unlist(regmatches(waldT2, 
                      gregexpr("[A-Za-z]+\\.?\\w+", 
                               waldT2, perl = TRUE)))
  }
  term<-gsub('\\.','\\:',term) # change . to : in R
  n <- length(term)
  
  waldTv <- as.numeric(unlist(regmatches(object$waldT, 
                                         gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", 
                                                  object$waldT, perl = TRUE))))
  if (object$org.par$mulT) { # TRUE
    if(is.null(ncol)) ncol <- 2
    wv <- matrix(waldTv, ncol = ncol, byrow=TRUE) # ncol=2
    wv <- data.frame(wv)
    
    denDF <- rep(object$Residual.DF,n)
    numDF <- as.numeric(as.character(wv[,1]))
    Fv <- as.numeric(as.character(wv[,2]))
  } else { #F: single trait
    if(is.null(ncol)) ncol <- 5
    wv <- matrix(waldTv, ncol = ncol, byrow=TRUE) # ncol=5
    wv <- data.frame(wv) 
    
    numDF <- as.numeric(as.character(wv[,1]))
    denDF <- as.numeric(as.character(wv[,2]))
    Fv <- as.numeric(as.character(wv[,3]))
  }
  
  pv <- NULL
  for (i in seq(1, n)) #pv[i] <- 1 - pchisq(Fv[i], numDF[i])
    pv[i] <- pf(Fv[i], numDF[i],denDF[i],lower.tail=FALSE)
  tab <- matrix(nrow = n, ncol = 3)
  tab[, 1] <- numDF[1:n]
  tab[, 2] <- Fv[1:n]
  tab[, 3] <- pv[1:n]
  tab <- data.frame(tab)
  tab[, 2] <- tab[, 1] * tab[, 2] 
  heading <- "Wald tests for fixed effects:\n"
  attr(tab, "heading") <- heading
  attr(tab, "names") <- c("DF", "Wald-F", 
                          "Pr(Chisq)")
  attr(tab, "row.names") <- term
  class(tab) <- c("anova", "data.frame")
  return(tab)
}


#' @export
IC <- function(object,...){
  UseMethod("IC",object)
}
#' @method  IC esR
#' @export  IC.esR
#' @rdname  AF.Echidna
#' @export

IC.esR <-function(object){
  #object<-res13
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$esr.all)){
      df<-get.IC(object)
      #print(df)
      
    }else{ #!cycle
      
      #nn<-length(object$esr.all)
      trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
      #preds<-list();length(preds)<-length(trt)
      preds<-vector("list", length(trt))
      
      for(i in 1:length(trt)) {
        preds[[i]]<-object$esr.all[[i]]
      }
      
      IC0<-lapply(preds,get.IC)   
      df<-do.call(rbind,IC0)
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      rownames(df)<-trt
      
      #print(df)
    }
  }
  
  if(object$org.par$batch==TRUE){
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    #preds<-list();length(preds)<-length(trt)
    preds<-vector("list", length(trt))
    
    for(i in 1:length(trt)) {
      preds[[i]]<-object$res.all[[i]]
    }
    #names(preds)<-trt
    
    IC0<-lapply(preds,get.IC)   
    df<-do.call(rbind,IC0)
    
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    row.names(df)<-trt
    #print(df)
  }
  return(df)
}

get.IC<-function(object){
  data.frame(DF=object$Residual.DF,parNO=object$ICparameter.Count,
             LogL=object$LogLikelihood,
             AIC=object$AkaikeIC,BIC=object$BayesianIC)
}


#' @export
trace <- function(object,...){
  UseMethod("trace",object)
}
#' @method  trace esR
#' @export  trace.esR
#' @rdname  AF.Echidna
#' @export
## iteration trace of runs
trace.esR<-function(object){
  #object<-spm.esr
  
  # if(object$org.par$family==" !poisson !log !disp 1")
  # cat(object$Iterations00)
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$esr.all)){
      #cat('\nEchinda version: ',object$Version,'\n')
      cat('\n',object$StartTime,'\n')
      print.data.frame(object$Iterations)
      cat(object$FinishAt,'\n\n')
    }else{
      nn<-length(object$esr.all)
      trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
      #cat('\nEchinda version: ',object$esr.all[[1]]$Version,'\n')
      
      for(i in 1:nn){
        cat('\n\nIteration procedure for trait: ',trt[i],'\n')
        cat('\n',object$esr.all[[i]]$StartTime,'\n')
        print.data.frame(object$esr.all[[i]]$Iterations)
        cat(object$esr.all[[i]]$FinishAt,'\n')
      }
    }
  }
  
  if(object$org.par$batch==TRUE){
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    
    #preds$pred<-lapply(object$res.all,function(x) x$pred)
    for(i in 1:length(trt)) {
      #preds[[i]]<-object$res.all[[i]]$Iterations
      cat('\n\nIteration procedure for trait: ',trt[i],'\n')
      cat('\n',object$res.all[[i]]$StartTime,'\n')
      print.data.frame(object$res.all[[i]]$Iterations)
      cat(object$res.all[[i]]$FinishAt,'\n')
    }
  }

}



#' @export
Var <- function(object,...){
  UseMethod("Var",object)
}
#' @method  Var esR
#' @export  Var.esR 
#' @rdname  Var
#' @export
Var.esR<-function(object){
  
  batch<-object$org.par$batch
  cycle<-object$org.par$cycle
  
  mulT<-object$org.par$mulT
  mulN<-object$org.par$mulN
  mulT<-object$org.par$mulT
  trace<-object$org.par$trace
  
  if(batch==FALSE){
    if(cycle==FALSE){
      #object<-r1.res
      
      if(is.null(object$evp)) 
        stop('Please use "W components" under "vpredict".')
      tt<-AFEchidna::evp.res(object$evp)
      #cat(mm$evp)
      for(i in 3:4) tt[,i]<-as.numeric(as.character(tt[,i]))
      tt$Z.ratio<-tt$Sigma/tt$SE
      
      tt$Term<-gsub('\\.',':',tt$Term)
      
      ## for MET
      var1<-tt[,-1]
      
      #return(var1)
    }
    if(cycle==TRUE){ # !cycle
      #object<-res13b
      
      nn<-length(object$esr.all)
      trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
      
      Converge<-sapply(object$esr.all,function(x) x$Converge)
      maxit<-sapply(object$esr.all,function(x) x$maxit)
      
      vpc0<-object$vpc.all[[1]]
      terms<-vpc0$Term # res,fam,...
      
      vpc<-do.call(rbind,object$evp.all)#[[idx]]
      
      varv<-vpc$Sigma
      varm<-matrix(varv,ncol=length(terms),byrow=TRUE)
      vardf<-as.data.frame(varm)
      names(vardf)<-paste0('V',1:ncol(vardf))
      
      sev<-vpc$SE
      sem<-matrix(sev,ncol=length(terms),byrow=TRUE)
      sedf<-as.data.frame(sem)
      names(sedf)<-paste0(names(vardf),'.se')
      
      var1<-cbind(vardf,sedf)
      var1$Converge<-Converge
      var1$maxit<-maxit
      
      trt<-gsub(' ','-',trt)
      trt<-sub('-$','',trt)
      row.names(var1)<-trt
      
      vcms<-terms
      vcms<-paste(paste0('V',1:length(vcms)),vcms,sep='-',collapse='; ')
      
      attr(var1, "heading") <- paste0('\n',vcms,'\n',
                                      'Converge: 1 means True; 0 means FALSE.\n')
      attr(var1, "row.names") <- trt
      class(var1) <- c("anova","data.frame")
      #return(var1)
    }
  }
  
  if(batch==TRUE){
    #object<-r2g#res21b
    
    trt<-names(object$res.all)
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)
    trt<-sub('^-','',trt)
    trtN<-length(trt)
    
    Converge<-sapply(object$res.all,function(x) x$Converge)
    maxit<-sapply(object$res.all,function(x) x$maxit)
    
    #var<-list();length(var)<-trtN
    var<-vector("list", trtN)
    
    var<-lapply(1:trtN, function(x){
      var0<-AFEchidna::evp.res(object$res.all[[x]]$evp)
      #terms<-var0[,'Term']
      mat.var<-matrix(c(var0[,'Sigma'],var0[,'SE']),nrow=1)
      mat.var<-as.data.frame(mat.var)
      names(mat.var)<-c(paste0('V',1:nrow(var0)),paste0('V',1:nrow(var0),'.se'))
      mat.var
    })
    
    var0<-AFEchidna::evp.res(object$res.all[[1]]$evp)
    terms<-var0[,'Term']
    
    var1<-do.call(rbind,var)
    row.names(var1)<-trt
    var1$Converge<-Converge
    var1$maxit<-maxit
    
    if(mulT==TRUE) terms<-AFEchidna::vcms.fun(terms,mulN)
    
    terms<-paste(paste0('V',1:length(terms)),terms,sep='-',collapse='; ')
    
    head0<- paste0('\n',terms,'\n',
                   'Converge: 1 means True; 0 means FALSE.\n')
    attr(var1, "heading") <-head0 
    attr(var1, "row.names") <- trt
    class(var1) <- c("anova","data.frame")
  }
  return(var1)
}


#' @export
Var.AR<-function(object,delN=4){
  #object<-m3b
  patt<-"[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?"
  numv<-as.numeric(unlist(regmatches(object$keyres,
                                     gregexpr(patt, object$keyres, perl=TRUE))))
  
  numm<-matrix(numv[-1:-delN],ncol=3,byrow=T)#
  
  varcomp<-as.data.frame(numm)
  names(varcomp)<-c('Gamma','Sigma','Zratio')
  varcomp$SE<-varcomp$Sigma/varcomp$Zratio
  
  varcomp <- round(varcomp,3)
  varcomp <- varcomp[,c('Sigma','SE','Zratio')] 
  varcomp$Siglevel<-sig.level(varcomp$Sigma,varcomp$SE)
  
  Term <- Var(object)$Term
  ssr <- which(grepl("^id(.*)", Term))
  if(length(ssr)!=0) Term <- Term[-ssr] 
  ss <- which(grepl("(ar1(.*))", Term))
  Term[ss] <- paste0(Term[ss],'.cor')
  varcomp$Term <- Term[c(2:length(Term),1)]
  varcomp <- varcomp[,c(length(varcomp),1:(length(varcomp)-1))]
  
  return(varcomp)
}


#' @title Summarize an esR object
#' 
#' @description
#' A \code{summary} method for objects inheriting from class
#' \code{esR}.
#' 
#' @param object
#'
#' An \code{esR} object. 
#' 
#' @return
#'
#' A list of class \code{summary.esR} with the following components:
#'
#' \describe{
#'
#' \item{org.res}{Original results from .esr file in Echidna.} 
#'   
#' \item{varcomp}{A dataframe summarising the random variance component.}
#' 
#' \item{IC}{nedf, loglik, Akaike information criterion and Bayesian information criterion.}
#' 
#' \item{coef.fixed}{A dataframe of coefficients and their standard errors
#' for fixed effects.}
#'
#' \item{coef.random}{A dataframe of coefficients and their standard errors
#' for random effects.}
#' 
#' }
#' @export
summary <- function(object,...){
   UseMethod("summary",object)
 }
#' 
#' 
#' @export  summary.esR
# @rdname  AF.Echidna
#' @method  summary esR
#' @aliases summary
#' @export
#'
summary.esR <- function(object){
  
  sum.object <- vector(mode="list")
  if(object$org.par$batch0==FALSE & is.null(object$esr.all)){
      keyres <- object$keyres
      sum.object$org.res <- keyres  # cat(keyres)
      sum.object$varcomp <- AFEchidna::Var(object)
      sum.object$IC <- AFEchidna::IC(object)
      sum.object$coef.fixed  <- AFEchidna::coef(object)$fixed
      sum.object$coef.random <- AFEchidna::coef(object)$random
    }else{  
      if(!is.null(object$esr.all)) xxxx <- object$esr.all ## !cycle
      if(!is.null(object$res.all)) xxxx <- object$res.all ## batch
      nn <- length(xxxx) #length(object$esr.all)
      trt <- unlist(lapply(xxxx,function(x) x$Traits))
     
      for(i in 1:nn){
        sum.object$org.res[[i]] <- xxxx[[i]]$keyres #object$esr.all[[i]]$keyres
        sum.object$coef.fixed[[i]]  <- AFEchidna::coef(object)[[i]]$fixed
        sum.object$coef.random[[i]] <- AFEchidna::coef(object)[[i]]$random
      }
      names(sum.object$coef.fixed) <-names(AFEchidna::coef(object))
      names(sum.object$coef.random)<-names(AFEchidna::coef(object))
      sum.object$IC <- AFEchidna::IC(object)
    }

  class(sum.object) <- "summary.esR"
  return(sum.object)
}

#' @export
converge <- function(object,...){
  UseMethod("converge",object)
}
#' @method  converge esR
#' @export  converge.esR
#' @rdname  AF.Echidna
#' @export
#'
converge.esR<-function(object){
  #object<-res
  
  if(object$org.par$batch==FALSE){
    if(is.null(object$esr.all)){
      cat(object$Converge)
    }else{
      nn<-length(object$esr.all)
      trt<-unlist(lapply(object$esr.all,function(x) x$Traits))
      for(i in 1:nn){ # i=1
        cat('\nConverge state for trait: ',trt[i],' ')
        cat(object$esr.all[[i]]$Converge,'\n')
        cat('\n')
      }
    }
  }
  
  if(object$org.par$batch==TRUE){
    trt<-unlist(lapply(object$res.all,function(x) x$Traits))
    trt<-gsub(' ','-',trt)
    trt<-sub('-$','',trt)

    for(i in 1:length(trt)) {
      cat('\nConverge state for trait: ',trt[i],'\n')
      cat('\n',object$res.all[[i]]$Converge,'\n')
      cat('\n')
    }
  }
}

#### original site codes connection with results

#' @title MET analysis
#' 
#' @description 
#' \code{met.plot} plots MET data.\code{met.corr} calculates var/cov/corr from echidna MET factor analytic 
#' results to further research the relation of trial sites.
#' \code{met.biplot} This function biplots MET factor analytic results from echidna 
#' to find the relation of trial sites and the best variety suitable to trial sites.  
#' 
#' @param data	 MET data.
#' @param plot.title	 MET plot title.
#' @param object	 echidna factor analytic results for MET, such as mm.
#  @param aimS  Specify the aim location parts of echidna object to count corr matrix.
#' @param rotate  Rotate the factor loadings, FALSE(default).
#' @param siteV	  trial site values, a vector or data frame.
#' @param kn	 Site cluster group numbers, 3(default).
#' @param horiz  output cluster site result format, horiz(default).
#' @param biplot	 output biplots, FALSE(default).
#  @param plg	 Adding labels before site, "S"(default). 
#  @param dmethod	 The distance measured method for site cluster, "manhattan"(default), more details see amap::hcluster.
#' @param dSco.u	 Least score of Variety breeding value. 
#' @param dLam.u	 Least distance from center.
#'
#' @rdname  met
#' @export met.plot
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' @examples 
#' \dontrun{
#' library(AFEchidna)
#' 
#' path="C:/Users/echi/exam" #home
#' setwd(path)
#' 
#' MET<-read.csv('MET.csv')
#' names(MET)
#' 
#' # example 1
#' # variable order: yield,genotype,site,row,col
#' MET2<-MET[,c(9,1,2,4:5)] 
#' str(MET2)
#' met.plot(MET2)
#' 
#' # example 2
#' MET3<-MET[,c(9,1,2,4:7)] # add variable order on MET2: Rep, Block
#' str(MET3)
#' met.plot(MET3,"My met trials")
#' 
#' ## running met analysis with FA model
#' mm<-echidna(es0.file="MET.es0",trait='yield',fixed='Loc',
#'       random='Genotype.xfa2(Loc)',
#'       residual='sat(Loc).units', #sat(Loc).ar1(Col).ar1(Row)
#'       #predict=c('Genotype'),
#'       vpredict=c('V Vmat Genotype.xfa1(Loc)','R cor 20:40'),
#'       qualifier='!maxit 50 !SLN',
#'       foldN='mm',
#'       met=T)
#'       
#' Var(mm)
#' 
#' siteV<-unique(MET['Loc']) # should be a data.frame or vector
#' 
#' met.corr(mm,siteV=siteV)
#' 
#' met.biplot(mm,siteV=siteV)
#' met.biplot(mm,siteV=siteV,biplot=T)
#' met.biplot(mm,siteV=siteV,biplot=T,dSco=1.0,dLam=0.8)
#' 
#' res2<-met.vmat(mm,siteV=siteV,VmatN='Vmat',corN='cor')
#' res2$res
#' res2$var
#' 
#' }
#' 

#' @usage met.plot(data, plot.title = NULL,...) 
#' @export
met.plot <-function(data,plot.title=NULL,...){

    if(!require(desplot))
      stop('Need package: desplot.\n')
  
    require(desplot,warn.conflicts=FALSE,quietly=TRUE)
    
    if(is.null(plot.title)) plot.title <- "MET data plot"
    
    dat <- data
    levels(dat[,3]) <- paste("S",1:nlevels(dat[,3]),sep="")
    names(dat)[1:5] <- c("yield","genotype","site","row","col")
    for(i in 4:5) dat[,i] <- as.numeric(dat[,i])
    
    #windows(10,8)
    # desplot(yield~ col*row|site, dat, main=plot.title)
    if(length(dat)==5){  
      desplot::desplot(yield~ col*row|site, dat, main=plot.title)
    }else{    
      names(dat)[6:7] <- c("Rep","Blk")  
      desplot::desplot(yield ~ col*row|site, dat, main=plot.title,
                       out1=Rep, out2=Blk,strip.cex=1.5,
                       out1.gpar=list(col="blue", lwd=4),
                       out2.gpar=list(col="red", lwd=1, lty=1),
                       par.settings = list(layout.heights=list(strip=2)))
    } 
}


#' @usage met.corr(object,siteV,kN=NULL,horiz=TRUE,rotate=FALSE) 
#' @rdname  met
#' @export
met.corr <- function(object,siteV,kN=NULL,horiz=TRUE,rotate=FALSE) { 
  
  if(!require(dplyr))
    stop('Need package: dplyr.\n')
  if(!require(amap))
    stop('Need package: amap.\n')
  
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  require(amap,warn.conflicts=FALSE,quietly=TRUE)
  
  #mm<-object
  
  if(is.null(kN)) kN <- 3
  
  if(is.data.frame(siteV)) {
    siteV<-factor(siteV[,1])
    siteN <- n<- nlevels(siteV) 
  } else siteN <- n<- length(siteV)
  
  
  varcomp <- AFEchidna::Var(object)[c('Term','Sigma')]
  varcomp <- varcomp %>% dplyr::filter(grepl('xfa', Term)) %>% dplyr::select(Sigma)
  
  vect1 <- varcomp[1:n,]
  w.var <- diag(vect1)
  vect2 <- varcomp[(n+1):nrow(varcomp),]
  t.var <- matrix(vect2,nrow=siteN)
  #crossprod(t.var)
  
  if(rotate==TRUE){
    cat('Using rotating factor loadings.\n\n')
    
    L <- t.var
    svd.L <- svd(L)
    L.star <- L %*% svd.L$v
    t.var <- L.star
  }
  
  wt.var <- t.var%*%t(t.var)+w.var
  df <- wt.var
  
  df2<-stats::cov2cor(wt.var)
  rownames(df2)<-colnames(df2)<-paste0('S-',siteV)
  
  df[upper.tri(df)]<-df2[upper.tri(df2)]
  df<-as.data.frame(df)
  
  rownames(df)<-colnames(df)<-paste0('S-',siteV)
  
  cat("\nVar\\Cov\\corr matrix:\n")
  print.data.frame(round(df,3))
  cat("---------------")
  cat("\ndiag is Variance, lower is covariance, upper is correlation.\n")
  
  
  df2<-stats::na.omit(df2)
  chcluster <- amap::hclusterpar(df2, method="manhattan")
  graphics::plot(chcluster,main='Cluster of different sites', 
              hang=-1,ylab='',xlab='')  
  stats::rect.hclust(chcluster, k=kN)
  cat("\nSite cluster results:\n")
  id <- stats::cutree(chcluster,k=kN)
  cl.res<- data.frame(cl.No=id,row.names=rownames(df))
  if(horiz) print(t(cl.res)) else print.data.frame(cl.res)
  cat('\n')
  
  invisible(df)
}

#' @usage met.biplot(object,siteV,biplot=FALSE, dSco.u=NULL,dLam.u=NULL)
#' @rdname  met
#' @export
met.biplot <- function(object,siteV,biplot=FALSE,dSco.u=NULL,dLam.u=NULL) {
  #object<-mm
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  #is.data.frame(Var(mm))
  
  component<-AFEchidna::Var(object)[c('Term','Sigma')] 
  arr<-component %>% dplyr::filter(grepl('xfa', Term)) %>% dplyr::select(Sigma)
  
  if(is.data.frame(siteV)) {
    siteV<-factor(siteV[,1])
    siteN <- n<- nlevels(siteV) 
  } else siteN <- length(siteV)
  
  sname<-paste("S-",siteV,sep="")
  
  Xfam<-matrix(arr[,1],nrow=siteN)#,(1+faN))
  faN<-ncol(Xfam)-1
  
  fa.name<-paste("FA",1:faN,sep="")
  dimnames(Xfam)<-list(sname,c("Psi",fa.name))
  #windows(8,8)
  graphics::pairs(Xfam,main="Fig.1-- pairs of Psi with FAs")
  
  ss<-svd(Xfam[,-1])
  Lam<-Xfam[,-1] %*% ss$v
  colnames(Lam)<-c(paste("FA",1:faN,sep="")) # c("Fa1","Fa2")
  Gvar<-Lam %*% t(Lam)+diag(Xfam[,1])
  cLam<-diag(1/sqrt(diag(Gvar))) %*% Lam  ##??
  varp<-round(mean(diag(Lam %*% t(Lam))/diag(Gvar))*100,2) # %variance explained
  cat("\nFA number is:",faN,",\t%Var.explained is: ",varp,".\n")
  
  
  # get each factor for each site
  dt.Lam<-as.data.frame(Lam)
  row.names(dt.Lam)<-sname
  for(i in 1:faN) dt.Lam[(faN+i)]<-dt.Lam[i]^2
  colnames(dt.Lam)[(faN+1):(2*faN)]<-c(paste("sq.FA",1:faN,sep=""))
  dt.Lam$Ps.Var<-Xfam[,1]
  dt.Lam$T.Var<-rowSums(dt.Lam[(faN+1):(2*faN+1)],na.rm=T)
  nnn<-ncol(dt.Lam)
  for(i in 1:faN) dt.Lam[(nnn+i)]<-100*dt.Lam[(faN+i)]/dt.Lam$T.Var
  names(dt.Lam)[(nnn+1):(nnn+faN)]<-c(paste("per.FA",1:faN,sep=""))
  #nnn1<-ncol(dt.Lam)
  dt.Lam[(nnn+faN+1)]<-rowSums(dt.Lam[(nnn+1):(nnn+faN)],na.rm=T)
  names(dt.Lam)[(nnn+faN+1)]<-"tper.FA"
  mmm<-nrow(dt.Lam)
  dt.Lam[mmm+1,]<-colMeans(dt.Lam)
  row.names(dt.Lam)<-c(row.names(dt.Lam)[1:mmm],'Mean')
  
  #print(format(dt.Lam[(nnn+1):(nnn+faN+1)],digits=3,nsmall=3))
  print.data.frame(format(dt.Lam,digits=3,nsmall=3))
  
  if(biplot){ ### 
    if(!require(tidyr,warn.conflicts=FALSE,quietly=TRUE)) 
      stop('Need package: tidyr.\n')
    require(tidyr,warn.conflicts=FALSE,quietly=TRUE)
    
    if(faN==1){cat("\nAttension: biplot worked when more than 2 FAs!\n\n")}

    if(faN>1){

      #object<-m7
      bv<-AFEchidna::coef(object)$random # here would be complexed!!
      #head(bv)

      Xfasln<-bv %>% dplyr::filter(grepl('\\.F', Level)) #%>% select(Effect)
      #head(Xfasln1)
      Xfasln$Level<-gsub('\t','',Xfasln$Level)
      
      # here careful!!
      Xfasln1<-tidyr::separate(data = Xfasln, col = Level,
                               into = c("Genotype", "Loc"), sep = "\\.")

      #VarietyN=70
      VarietyN<-nrow(Xfasln1)/faN
      scores<-matrix(Xfasln1$Effect,nrow=VarietyN)
      dimnames(scores)<-list(unique(Xfasln1$Genotype),paste("Fa",1:faN,sep=""))

      acb<-utils::combn(1:faN,2)
      bl<-faN*(faN-1)/2

      mLam<-rep(1/siteN,siteN) %*% Lam # get loading means
      sLam<-Lam-rep(mLam,rep(siteN,faN)) # center loadings
      dLam<-sqrt((sLam*sLam) %*% rep(1,faN)) # distance from center
      dSco<-sqrt((scores*scores) %*% rep(1,faN))

      dSco.a<-0.65*max(dSco,rm=TRUE)
      dLam.a<-max(dLam,rm=TRUE)

      if(is.null(dSco.u)) dSco.u<-round(dSco.a,1) # 2
      if(is.null(dLam.u)) dLam.u<-round(dLam.a,1) #0.1

      if(faN>2){
        for(i in 1:bl){
          #windows(18,8)
          #par(mfrow=c(1,2))
          stats::biplot(scores[,acb[,i]],Lam[,acb[,i]],cex=0.75,
                 main=paste("Fig 2-",i, " biplot with all variety",sep=""))
          graphics::abline(h=0,lty=3)
          graphics::abline(v=0,lty=3)
          if(nrow(Lam[dLam>dLam.u,1:2])>1){
            stats::biplot(scores[dSco>dSco.u,acb[,i]],Lam[dLam>dLam.u,acb[,i]],cex=0.75,
                   main=paste("Fig 3-",i, " biplot when dSco>",dSco.u,sep="")) # dSco>2
            graphics::abline(h=0,lty=3)
            graphics::abline(v=0,lty=3)
          }else {
            cat("\ndSco is:\n")
            print(tail(sort(dSco),6))
            cat("\ndLam is:\n")
            print(round(dLam,3))
            cat('\nthe second figure failure, we should set up dLam.\n')
          }
        }
      }else {
        #windows(18,8)
        #par(mfrow=c(1,2))
        stats::biplot(scores[,1:2],Lam[,1:2],cex=0.75,
               main="Fig 2 biplot with all variety")
        graphics::abline(h=0,lty=3)
        graphics::abline(v=0,lty=3)

        if(nrow(Lam[dLam>dLam.u,1:2])>1){
          stats::biplot(scores[dSco>dSco.u,1:2],Lam[dLam>dLam.u,1:2],cex=0.75,
                 main=paste("Fig 3 biplot when dSco>",dSco.u,' and dLam>',dLam.u,sep="")) # dSco>2
          graphics::abline(h=0,lty=3)
          graphics::abline(v=0,lty=3)
        }else {
          cat("\ndSco is:\n")
          print(utils::tail(sort(dSco),6))
          cat("\ndLam is:\n")
          print(round(dLam,3))
          cat('\nthe second figure failure, we should set up dLam.\n')
        }

      }

      dscores<-data.frame(scores[dSco>dSco.u,],Scores=dSco[dSco>dSco.u]) #2
      ddLam<-data.frame(Lam[dLam>dLam.u,],distFC=dLam[dLam>dLam.u]) # 0.1

      cat("\nScores.u is:",dSco.u,"\n")
      print.data.frame(round(dscores,3))
      cat("\ndistFC.u is:",dLam.u,"\n")
      print.data.frame(round(ddLam,3))
      cat("\n")
    }
  }
  
}

#' @usage met.vmat(object,siteV,VmatN,corN)
#' @rdname  met
#' @export
met.vmat <- function(object,siteV,VmatN='Vmat',corN='cor') {
  
  if(!require(reshape,warn.conflicts=FALSE,quietly=TRUE)) 
    stop('Need package: reshape.\n')
  if(!require(dplyr,warn.conflicts=FALSE,quietly=TRUE)) 
    stop('Need package: dplyr.\n')
  
  require(reshape,warn.conflicts=FALSE,quietly=TRUE)
  require(dplyr,warn.conflicts=FALSE,quietly=TRUE)
  
  odd <- function(x) x %% 2 != 0
  
  if(is.data.frame(siteV)) {
    siteV<-factor(siteV[,1])
    siteN <- n<- nlevels(siteV) 
  } else siteN <- length(siteV)
  
  sname<-paste("S-",siteV,sep="")
  
  #path1<-paste(path,foldN,sep='/')
  #evpf<-paste(path1,'temp.evp',sep='/')
  res.evp<-object$evp0 #base::readLines(evpf)
  
  patt1<-paste0(VmatN,' 2')
  vmat.str<-res.evp[grep(patt1,res.evp)]
  #print(vmat.str)
  vmat.v<-as.numeric(unlist(regmatches(vmat.str,
                                       gregexpr("[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?",
                                                vmat.str, perl=TRUE))))
  vn<-1:length(vmat.v)
  
  v.mat<-vmat(siteN=siteN, vec=vmat.v[odd(vn)])
  se.mat<-vmat(siteN=siteN, vec=vmat.v[!odd(vn)])
  
  
  rownames(v.mat)<-colnames(v.mat)<-sname
  rownames(se.mat)<-colnames(se.mat)<-sname
  
  patt2<-paste0(corN,' 2')
  cor.str<-res.evp[grep(patt2,res.evp)]
  #print(cor.str)
  cor.v<-as.numeric(unlist(regmatches(cor.str,
                                      gregexpr("[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?",
                                               cor.str, perl=TRUE))))
  
  
  corn<-1:length(cor.v)
  cor.mat<-AFEchidna::vmat(siteN=siteN, vec=cor.v[odd(corn)],cor=TRUE)
  corse.mat<-AFEchidna::vmat(siteN=siteN, vec=cor.v[!odd(corn)],cor=TRUE)
  
  #cat('Var\\cov\\corr matrix:\n')
  #corn<-1:length(cor.v)
  v.mat1<-v.mat
  v.mat1[upper.tri(v.mat1,diag=F)]<-cor.v[odd(corn)]
  print(v.mat1)
  
  #cat('\nThe se for Var\\cov\\corr matrix:\n')
  se.mat1<-se.mat
  se.mat1[upper.tri(se.mat1,diag=F)]<-cor.v[!odd(corn)]
  print(se.mat1)
  
  #cat('\nThe sig.level for Var\\cov\\corr matrix:\n')
  sig.l1<-AFEchidna::siglevel(vmat.v[odd(vn)],vmat.v[!odd(vn)])
  sig.mat<-AFEchidna::vmat(siteN=siteN, vec=sig.l1)
  
  sig.l2<-AFEchidna::siglevel(cor.v[odd(corn)],cor.v[!odd(corn)])
  sig.mat[upper.tri(sig.mat,diag=F)]<-sig.l2
  
  rownames(sig.mat)<-colnames(sig.mat)<-sname
  print(noquote(sig.mat))
  
  cat('\nThe se for corr matrix:\n')
  corse.mat[upper.tri(corse.mat,diag=F)]<-cor.v[odd(corn)]
  rownames(corse.mat)<-colnames(corse.mat)<-sname
  print(corse.mat)
  
  cat('\nThe sig.level for corr matrix:\n')
  corsig.mat<-cor.mat
  corsig.mat[upper.tri(corsig.mat,diag=F)]<-sig.l2
  rownames(corsig.mat)<-colnames(corsig.mat)<-sname
  print(noquote(t(corsig.mat)))
  
  cat("=================\n")
  cat("upper is corr and lower is error (or sig.level) for corr matrix.\n")
  cat("Sig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
  
  df<-list(vmat=v.mat1,se.vmat=se.mat1,sig.vmat=sig.mat,
           cor.se.mat=corse.mat,cor.sig.mat=corsig.mat)
  
  vmat0<-v.mat1
  rownames(v.mat1)<-colnames(v.mat1)<-1:nrow(v.mat1)
  rownames(se.mat1)<-colnames(se.mat1)<-1:nrow(se.mat1)
  rownames(sig.mat)<-colnames(sig.mat)<-1:nrow(sig.mat)
  
  rr0<-reshape::melt(vmat0)
  rr1<-reshape::melt(v.mat1)
  rr2<-reshape::melt(se.mat1)
  rr3<-reshape::melt(sig.mat)
  
  rr4<-merge(rr1,rr2,by=c('X1','X2'),sort=F)
  rr5<-merge(rr4,rr3,by=c('X1','X2'),sort=F)
  
  rr5$Type<-ifelse(rr5$X1<rr5$X2,'Cor','Var')
  
  rr5$X1<-rr0$X1
  rr5$X2<-rr0$X2
  
  rr5<-dplyr::arrange(rr5,dplyr::desc(Type),X1,X2)
  names(rr5)[1:5]<-c('site1','site2','estimate','se','Sig.level')
  
  df<-list(res=df,var=rr5)
  
  return(df)
  
}

#' @export
vmat <- function(siteN, vec,cor=FALSE) {
  vmat<-diag(siteN) # siteN
  if(cor==TRUE) vmat[upper.tri(vmat,diag=F)]<-vec
  else vmat[upper.tri(vmat,diag=T)]<-vec
  a<-vmat
  a[lower.tri(a)]<-t(a)[lower.tri(a)]
  
  return(a)
}

## for batch
#' @export
vm2r <- function(object,corN=NULL) {
  if(!is.null(object$res.all)){
    tt<-names(object$res.all)
    ss<-lapply(1:length(tt),function(x) AFEchidna::vm2r0(object,idx=x,corN=corN))
    names(ss)<-tt
  }else ss<-AFEchidna::vm2r0(object,corN=corN)
  
  return(ss)
}

#' @export
vm2r0 <- function(object,idx=1,corN=NULL) {
  
  #object<-res22
  if(!is.null(object$res.all))
    evp.str<-strsplit(object$res.all[[idx]]$evp,'\r\n')[[1]] # !batch
  
  if(!is.null(object$evp))
    evp.str<-strsplit(object$evp,'\r\n')[[1]] 
  
  if(!is.null(corN)){
    #corN<-'cor'
    patt2<-paste0(corN,' 2')
    cor.str<-evp.str[grep(patt2,evp.str)]
    num.patt<-"[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?"
    cor.v<-as.numeric(unlist(regmatches(cor.str,
                                        gregexpr(num.patt,
                                                 cor.str, perl=TRUE))))
    
    rdf<-as.data.frame.matrix(matrix(cor.v,ncol=2,byrow=T)) # corr
    rdf$Type<-'cor'
    
    cor.str1<-cor.str[grep('cor 2\\s+.*=',cor.str)]
    
    
    cor.str1<-unlist(regmatches(cor.str1,
                                gregexpr('(?<=cor 2\\s)\\s+\\d.*=\\s+us',
                                         cor.str1, perl=TRUE)))
    cor.str1<-gsub('\\s+','.t',cor.str1)
    cor.str1<-gsub('.t=.tus','',cor.str1)
    row.names(rdf)<-paste0('r',cor.str1)
    
    
    var.str<-evp.str[evp.str!=cor.str]
  } else var.str<-evp.str
  
  var.v<-as.numeric(unlist(regmatches(var.str,
                                      gregexpr("[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?",
                                               var.str, perl=TRUE))))
  vardf<-as.data.frame.matrix(matrix(var.v,ncol=2,byrow=TRUE)) # var
  vardf$Type<-'var'
  #print(cor.str)
  
  if(!is.null(object$res.all)){
    terms<-attr(object$varcomp,'heading')
    terms<-strsplit(terms,'\n')[[1]][2]
    terms<-gsub('V\\d+-','',terms)
    terms<-unlist(strsplit(terms,'; '))
    row.names(vardf)<-terms
  }

  if(!is.null(corN)) resdf<-rbind(vardf,rdf)
  else resdf<-vardf
  
  names(resdf)[1:2]<-c('Estimate','SE')
  
  return(resdf)

}


#======================================================
#' @title Generate genomic relationship matrix.
#' 
#' @description 
#' \code{GenomicRel} This function generates 5 genomic relationship matrixs.
#' 
#' \tabular{ll}{
#' \strong{option} \tab \strong{Description} \cr
#' 1    \tab observed allele frequencies (GOF, VanRaden, 2008). \cr
#' 2    \tab weighted markers by recipricals of expected variance (GD, Forni et al., 2011). \cr
#' 3    \tab allele frequencies fixed at 0.5 (G05, Forni et al., 2011). \cr
#' 4    \tab allele frequencies fixed at mean for each locus (GMF, Forni et al., 2011).\cr
#' 5    \tab regression of MM' on A sort (Greg, VanRaden, 2008).
#' }  
#'  
#' @usage GenomicRel(marker,option,ped=NULL,
#'                    Infv=1000,Gm=NULL,G.adj=FALSE,Gres=TRUE) 
#' 
#' @param marker	 markers data.
#' @param option	 option (1~5) for different G matrixs.
#' @param ped    ped data.
#' @param Infv   A value for Inf in G matrix. 
#' @param Gm     G matrix from marker.
#' @param G.adj  Adjust G matrix with A matrix from ped data. 
#' @param Gres   return G matrix directly(True, default) or in data frame(FALSE).  
#' @author Isik Fikret
#' @references
#' Isik Fikret. Genetic data analysis for plant and animal breeding. 2017 
#' @examples 
#' \dontrun{
#' library(AFEchidna)
#' 
#' read.example(package = "AFEchidna", setpath = TRUE)
#' Markers<-read.file(file="sim_markers.txt",sep=' ' )
#' ped<-read.table( "sim_pedigree.txt", sep=' ')
#' 
#' 
#'  GOF1=GenomicRel( Markers,1)
#'  GD1=GenomicRel(  Markers,2)
#'  G051=GenomicRel( Markers,3)
#'  GMF1=GenomicRel( Markers,4)
#'  
#'  # the same result but adjust G matrix with ped data:
#'  GOF2=GenomicRel(  Markers,1, ped,G.adj=T)
#'  GD2=GenomicRel(   Markers,2, ped,G.adj=T)
#'  G052=GenomicRel(  Markers,3, ped,G.adj=T)
#'  GMF2=GenomicRel(  Markers,4, ped,G.adj=T)
#'  Greg=GenomicRel(  Markers,5, ped,G.adj=T)
#' }
#' 
#' @export .GenomicRel
#' @export
GenomicRel <- function(marker,option=NULL,ped=NULL,Infv=10000,
                       Gm=NULL,G.adj=FALSE,Gres=TRUE){
  return(.GenomicRel(marker,option,ped,Infv,Gm,G.adj,Gres))
}
## functions to generate G matrix from SNP markers.

.GenomicRel <- function(marker, option=NULL,ped=NULL,Infv=10000,
                       Gm=NULL,G.adj=FALSE, Gres=TRUE){ # ,invG=FALSE
  
  
  #data
  #	column 1 - id
  #	column 2:m markers 
  #	markers must be 0, 1, 2 for homozygous, heterozygous and other homozygous
  
  #pedigree
  #	only needed for option 5
  #	ID, Sire, Dam in columns 1,2 and 3, respectively
  #	must be sorted with animal in first column before they are in 2nd or 3rd column
  
  #NO HEADERS on input tables
  #Returns G matrix in the following form: col1 = row, col2 = col, col3=Genomic relationship, col4=pedigree relationship
  
  if(!require(GeneticsPed)){stop('Need package: GeneticsPed.\n')}
  
  require(GeneticsPed,warn.conflicts=FALSE,quietly=TRUE)
  
  cat('Generating G matrix is under going.\n')
  
  options(warn=-1)
  data<-marker
  
  if(!is.null(ped)){
    names(ped)<-c("id","father","mother")
    ped<-GeneticsPed::as.Pedigree(ped)
    fullA<-GeneticsPed::relationshipAdditive(ped)
    rowName<-as.numeric(rownames(fullA))
    A<-fullA[match(data[,1],rowName),match(data[,1],rowName)]
  }
  
  #library(MASS)
  #library(GeneticsPed)
  if(!is.null(Gm)){
    G<-Gm
  }else{
    
    M1<-data
    
    if(option!=5){
      # M matrix
      if(option %in% c(1:2)) M<- M1[,2:ncol(M1)]-1 
        else M<- M1[,2:ncol(M1)]
      
      # p-value
      if(option==3){
        p1<-array(0.5,ncol(M))
        p<-p1
      }else{ # option=1,2,4
        
        if(option==4) p1<-round((apply(M,2,mean)),3) 
        else p1<-round((apply(M,2,sum)+nrow(M))/(nrow(M)*2),3)
        
        p<-2*(p1-.5)
      }
      
      P <- matrix(p,byrow=T,nrow=nrow(M),ncol=ncol(M))
      Z <- as.matrix(M-P)
      
      if(option==2){
        D<-1/(ncol(M)*(2*p1*(1-p1)))
        D[!is.finite(D)]<-Infv
        
        G <- Z%*%(D*t(Z))
      }else{ # option=1,2,4
        b<-1-p1
        c<-p1*b
        d<-2*(sum(c))
        
        ZZt <- Z %*% t(Z)
        G <- (ZZt/d)
      }
    }else{ # option=5
      M<- M1[,2:ncol(M1)]-1
      
      M<-as.matrix(M)
      MtM<-M%*%t(M)
      
      n<-(nrow(A))^2
      sumA<-sum(A)
      SqA<-A*A
      sumSqA<-sum(SqA)
      
      rhs1<-sum(MtM)
      rhs2<-sum(MtM*A)
      
      lhs<-cbind(rbind(n,sumA),rbind(sumA,sumSqA))
      rhs<-rbind(rhs1,rhs2)
      g<-solve(lhs,rhs)
      
      one<-matrix(1,nrow=nrow(data))
      
      G<-(MtM-(g[1,1]*(one%*%t(one))))/g[2,1]
    }
  }    
  
  
  if(G.adj==TRUE){
    GW<-round(0.95*G+(1-0.95)*A,3)
    #if(invG==TRUE) A=solve(A)
  }else GW<-G
  
  col1<-col2<-col3<-col4<-NA
  # col2<-NA
  # col3<-NA
  # col4<-NA
  for (i in 1:nrow(GW)){
    for (j in 1:i){
      
      col1<-cbind(col1,i)
      col2<-cbind(col2,j)
      col3<-cbind(col3,GW[i,j])
      if(!is.null(ped))	col4<-cbind(col4,round(A[i,j],3))
    }	
    
  }
  Gmat<-cbind(t(col1),t(col2),t(col3))
  row.names(Gmat)<-c(0:(nrow(Gmat)-1))
  
  if(!is.null(ped))	{
    Amat<-cbind(t(col1),t(col2),t(col4))
    row.names(Amat)<-c(0:(nrow(Amat)-1))
  }
  
  #write.table(Ginv[-1,],"Ginv.giv",sep=" ", row.names=F, col.names=F,quote = F)
  if(!is.null(ped)){
    df<-data.frame("col"=Gmat[-1,1],"row"=Gmat[-1,2],"G"=Gmat[-1,3],"A"=Amat[-1,3])
  } else df<-data.frame("col"=Gmat[-1,1],"row"=Gmat[-1,2],"G"=Gmat[-1,3])
  
  if(Gres==TRUE) return(GW) else return(df)
  
}

#======================================================
#' @title Generate H-inverse matrix for SS-GBLUP.
#' 
#' @description 
#' \code{AGH.inv} This function calculate genomic relationship matrix(G),
#' full additative matrix(A) and blended relationship matrix(H) from
#' genotyped marker, genotyped pedigree and ungenotyped pedigree.
#'  
#' @usage AGH.inv(gmarker,option=1, ugped, gped) 
#' 
#' @param option	 option (1~5) for different G matrixs.
#' @param ugped	 ungenotyped pedigree, or total pedigree.
#' @param gped	 genotyped pedigree.
#' @param gmarker	 genotyped marker,column 1 should be sample ID.
#' @param asrV	 asreml version, 3(default) or 4.
#' @details 
#'   This function would return a list containing 3 elements. The types of
#' option (1~5) as following:
#' 
#' \tabular{ll}{
#' \strong{option} \tab \strong{Description} \cr
#' 1    \tab observed allele frequencies (GOF, VanRaden, 2008). \cr
#' 2    \tab weighted markers by recipricals of expected variance (GD, Forni et al., 2011). \cr
#' 3    \tab allele frequencies fixed at 0.5 (G05, Forni et al., 2011). \cr
#' 4    \tab allele frequencies fixed at mean for each locus (GMF, Forni et al., 2011).\cr
#' 5    \tab regression of MM' on A sort (Greg, VanRaden, 2008).
#' } 
#' @return 
#' \describe{ 
#' \item{Ainv}{inverse of full additative matrix(A).}
#' \item{Ginv}{inverse of genomic relationship matrix(G).}	
#' \item{Hinv}{inverse of blended relationship matrix(H).}	
#' }
#' 
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016    
# AFfR website:https://github.com/yzhlinscau/AFfR
#' @examples 
#' \dontrun{
#' library(AFEchidna)
#' 
#' data("ugped")
#' data("gped")
#' data("gmarker")
#' 
#' # get A-matrix, G-matrix and H-matrix
#' AGH1<-AGH.inv(option=1,ugped,gped,gmarker)
#' 
#'   
#' data(MET)
#' MET$yield<-0.01*MET$yield
#' levels(MET$Genotype)<-gped$ID
#' MET1<-filterD1(MET, Loc %in% c(3))
#' 
#' ## for ASReml-R V3.0 
#' library(asreml)
#' 
#' # base model
#' sm1.asr<-asreml(yield~Rep, random=~ Genotype+units, 
#'                 rcov=~ ar1(Col):ar1(Row), 
#'                 data=MET1, maxiter=50)
#' 
#' Var(sm1.asr)
#'
#' # A-BLUP
#' Ainv <- AGH1$Ainv
#' sm2.asr<-update(sm1.asr, random=~ ped(Genotype)+units, 
#'                 ginverse=list(Genotype=Ainv))
#' 
#' Var(sm2.asr)
#' 
#' # G-BLUP
#' Ginv <- AGH1$Ginv
#' sm3.asr<-update(sm1.asr, random=~ ped(Genotype)+units, 
#'                 ginverse=list(Genotype=Ginv))
#' 
#' Var(sm3.asr)
#' 
#' # H-BLUP
#' Hinv <- AGH1$Hinv
#' sm4.asr<-update(sm1.asr, random=~ ped(Genotype)+units, 
#'                 ginverse=list(Genotype=Hinv))
#' 
#' Var(sm4.asr)
#' 
#' 
#' ## for ASReml-R V4 
#' library(asreml)
#' 
#' 
#' sm1.asr<-asreml(yield~Rep, random=~ Genotype+units, 
#'                 residual=~ ar1(Col):ar1(Row), 
#'                 data=MET1, maxiter=50)
#' 
#' Var(sm1.asr)
#'
#' # A-BLUP
#' Ainv <- AGH1$Ainv
#' sm2.asr<-update(sm1.asr, 
#'              random=~ vm(Genotype,Ainv)+units)
#' 
#' Var(sm2.asr)
#' 
#' # G-BLUP
#' Ginv <- AGH1$Ginv
#' sm3.asr<-update(sm1.asr, 
#'              random=~ vm(Genotype,Ginv)+units)
#' 
#' Var(sm3.asr)
#' 
#' # H-BLUP
#' Hinv <- AGH1$Hinv
#' sm4.asr<-update(sm1.asr, 
#'              random=~ vm(Genotype,Hinv)+units)
#' 
#' Var(sm4.asr)
#' 
#' ########## if any other genotyped id without ped
#' ########## we can put their parent code to 0 or NA
#' ########## to make pedigree, then use H-matrix.
#' ## gmarker2 without pedigree
#' #
#' ## make their pedigree
#' # gid2<-gmarker2[,1]
#' # gped2<-data.frame(ID=gid2,Female=0,Male=0)
#' #
#' ## combine genotyped id's pedigree
#' # gped1<-rbind(gped,gped2)
#' #
#' ## combine all genotyped marker data
#' # gmarker1<-rbind(gmarker,gmarker2)
#' #
#' # AGH1a<-AGH.inv(option=1,tped1,gped1,gmarker1)
#' #
#' }
#' @export .AGH.inv
#' @export

AGH.inv <- function(option=1,ugped,gped,gmarker,asrV=3,tidn=NULL,gidn=NULL){
  
  return(.AGH.inv(option,ugped,gped,gmarker,asrV,tidn,gidn))
}

.AGH.inv <- function(option=1,ugped,gped,gmarker,asrV=3,
                     tidn=NULL,gidn=NULL){
  # tidn: vector of total id number
  # gidn: vector of genotyped id number
  
  # names(ped)<- c("id","father","mother")
  #asrV<-getASRemlVersionLoaded(Rsver=TRUE)  
  
  if(!require(nadiv)){stop('Need package: nadiv.\n')}
  #if(!require(synbreed)){stop('Need package: synbreed.\n')}
  if(!require(GeneticsPed)){stop('Need package: GeneticsPed.\n')}
  
  # genotyped ped
  gped<-gped[!duplicated(gped),]
  gid<-as.character(gped[,1])
  
  # total ped
  tped<-rbind(ugped,gped)
  #row.names(tped)<-tped[,1]
  tped<-tped[!duplicated(tped),]
  
  #tped1<-tped
  tped1<-nadiv::prepPed(tped)
  fullA<-as.matrix(nadiv::makeA(tped1))
  tid1<-rownames(fullA)#<-colnames(fullA)
  
  ugid<-base::setdiff(tid1,gid)
  tid1<-c(ugid,gid)
  
  rowName<-rownames(fullA)
  fullA<-fullA[match(tid1,rowName),match(tid1,rowName)]
  
  gNO<-length(gid)
  ugNO<-length(ugid)
  
  A11<-fullA[1:ugNO,1:ugNO] # ungenotyped A
  A22<-fullA[(1+ugNO):(ugNO+gNO),(1+ugNO):(ugNO+gNO)] # genotyped A
  A12<-fullA[1:ugNO,(1+ugNO):(ugNO+gNO)]
  A21<-t(A12)
  #row.names(A22)
  
  G<-AFEchidna::GenomicRel( gmarker, option, Gres=TRUE)
  #G<-AFEchidna::GenomicRel( gmarker, option,gped,G.adj=T, Gres=TRUE)
  
  H11<-A11 + A12 %*% solve(A22) %*% (G-A22) %*% solve(A22) %*% A21
  H12<-G %*% solve(A22) %*% A21
  H21<-t(H12)
  H22<-G
  
  H1<-rbind(H11,H12)
  H2<-rbind(H21,H22)
  
  H<-cbind(H1,H2)
  
  if(!is.null(tidn)){
    class(fullA)<-c("relationshipMatrix", "matrix")
    rowName<-as.numeric(rownames(fullA))
    fullA<-fullA[match(tidn,rowName),match(tidn,rowName)]
    
    class(H)<-c("relationshipMatrix", "matrix")
    rowName<-as.numeric(rownames(H))
    H<-H[match(tidn,rowName),match(tidn,rowName)]
  }
  if(!is.null(gidn)){
    class(G)<-c("relationshipMatrix", "matrix")
    rowName<-as.numeric(rownames(G))
    G<-G[match(gidn,rowName),match(gidn,rowName)]
  }
  
  ## H-inverse
  
  class(H)<-c("relationshipMatrix", "matrix")
  #summary(eigen(H)$values)
  
  Hinv <- AFEchidna::write.relationshipMatrix(H, 
                                              file =NULL,
                                              sorting=c("ASReml"), 
                                              type=c("inv"), digits=10)
  
  #head(attr(Hinv, "rowNames"),10)
  names(Hinv) <- c("row", "column", "coefficient")
  
  if(asrV=='4'){
    Hinv1<-as.matrix(Hinv)
    attr(Hinv1, "rowNames")<-attr(Hinv, "rowNames")
    class(Hinv1)<-c("matrix","ginv")
    Hinv<-Hinv1
  }
  
  ## A-inverse
  A<-fullA
  class(A)<-c("relationshipMatrix", "matrix")
  #summary(eigen(A)$values)
  
  Ainv <- AFEchidna::write.relationshipMatrix(A, 
                                              file =NULL,
                                              sorting=c("ASReml"), 
                                              type=c("inv"), digits=10)
  
  #head(attr(Ainv, "rowNames"),10)
  names(Ainv) <- c("row", "column", "coefficient")
  
  if(asrV=='4'){
    Ainv1<-as.matrix(Ainv)
    attr(Ainv1, "rowNames")<-attr(Ainv, "rowNames")
    class(Ainv1)<-c("matrix","ginv")
    Ainv<-Ainv1
  }
  
  ## G-inverse
  class(G)<-c("relationshipMatrix", "matrix")
  #summary(eigen(A)$values)
  
  Ginv <- AFEchidna::write.relationshipMatrix(G, 
                                              file =NULL,
                                              sorting=c("ASReml"), 
                                              type=c("inv"), digits=10)
  
  #head(attr(Ainv, "rowNames"),10)
  names(Ginv) <- c("row", "column", "coefficient")
  
  if(asrV=='4'){
    Ginv1<-as.matrix(Ginv)
    attr(Ginv1, "rowNames")<-attr(Ginv, "rowNames")
    class(Ginv1)<-c("matrix","ginv")
    Ginv<-Ginv1
  }
  
  return(list(Ainv=Ainv,Ginv=Ginv,Hinv=Hinv))
}


## functions to output G matrix to giv or grm file for 
## further running in ASReml or Echidna.
#' @export
write.relationshipMatrix <- function(x,file=NULL,sorting=c("WOMBAT","ASReml"),
                                     type=c("ginv","inv","none"),digits=10){
  
  type <- match.arg(type)
  sorting <- match.arg(sorting)
  
  if(sorting=="WOMBAT" & type!="ginv") stop("'type' must be 'ginv' for WOMBAT")
  
  # pass (inverse) relationship matrix
  if(type=="ginv") rMinv <- MASS::ginv(x)
  if(type=="inv")  rMinv <- solve(x)
  if(type=="none") rMinv <- x
  
  rMinv <- round(rMinv,digits)
  
  # add rownames and colnames
  res <- data.frame(Row = rep(1:nrow(rMinv), nrow(rMinv)),
                    Column = rep(1:nrow(rMinv), each = nrow(rMinv)),
                    coeff = as.numeric(rMinv),
                    lower = as.logical(lower.tri(rMinv, diag = TRUE)))
  rm(rMinv)
  
  
  
  # only use lower triangle
  res <- res[res$lower == TRUE, c("Row", "Column", "coeff")]
  
  if (sorting=="ASReml"){    
    res <-  res[order( res$Row, res$Column), ] 
  }
  if (sorting=="WOMBAT"){
    res <- res[, c(2,1,3)]
    res <-  res[order(res$Column, res$Row), ]  
  }
  res <- res[res$coeff != 0, ]
  
  # write to given file
  if (!is.null(file)){
    write.table(res, file, sep = " ", quote = FALSE, 
                col.names = FALSE, row.names = FALSE)
    rm(x)
  } else {
    attr(res, "rowNames") <- rownames(x)
    rm(x)
    return(res)
  }
  
}


#########################
# function to generate specific correlation with another variable
#' @usage rho.par(rho,x0,a=1,b=0)
#' @rdname  AF.base
#' @export
rho.par <- function(rho,x0,a=1,b=0) {
  x0[is.na(x0)]<-0.001
  n     <- length(x0)#20         # length of vector
  rho   <- rho                   # desired correlation = cos(angle)
  theta <- acos(rho)             # corresponding angle
  x1    <- x0#rnorm(n, 1, 1)     # fixed given data
  x2    <- stats::rnorm(n, 2, 0.5)      # new random data
  X     <- cbind(x1, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
  
  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[, 1, drop=FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[, 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[, 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
  
  x <- Y[, 2] + (1 / tan(theta)) * Y[, 1]     # final new vector
  
  x<-x*a+b # make values positive 
  
  return(x)
}

##========== weights ==============



##========== family ==============
#' @export
esr_gaussian <- function(link = "identity", dispersion=NA)
{
  ## gaussian family
  
  # link <- as.character(substitute(link))
  # misnames <- c("inverse", "log", "identity", "reciprocal", "1/mu",
  #               "Inverse", "Reciprocal", "Log", "Identity")
  # corresp <- c(1, 2, 3, 1, 1, 1, 1, 2, 3)
  # lmatch <- pmatch(link, misnames, FALSE)
  # if(!lmatch)
  #   stop('Gaussian links are "log", "inverse" or "identity"\n')
  # link <- misnames[corresp[lmatch]]
  # fam <- esr_makeFamily("gaussian",link=link)
  # fam$dispersion <- dispersion
  
  if(!is.na(dispersion)) fam <- paste0(' !disp ',dispersion)
   else fam <- ' '
  
   return(fam)
}


#' @export
esr_binomial <- function(link = "probit", dispersion=1, total=NULL)
{
  if(link=="logit") fam<-paste0(' !binomial !logit !disp ',dispersion)#,'!totals ')
  if(link=="probit") fam<-paste0(' !binomial !probit ')
  if(link=="comploglog") fam<-paste0(' !binomial !comploglog ')
  if(link=="marginal") fam<-paste0(' !binomial !MARG ')
  return(fam)
}

#' @export
esr_poisson <- function(link = "log", dispersion=1.0)
{
  if(link=="log") fam<-paste0(' !poisson !log !disp ',dispersion)
  if(link=="sqrt") fam<-paste0(' !poisson !sqrt !disp ',dispersion)
  #if(link=="identity") fam=paste0(' !poisson !identity !disp ',dispersion)
  return(fam)
}  


#======================================================
# update: 2020-03-13
#' @title Model Qualifiers for Echidna
#'
#' @details
#'   The Qualifiers control various aspects of the model fitting process and reporting of results:
#'
#' \cr
#' \tabular{ll}{
#' \strong{jobqualf } \tab \strong{Description} \cr
#' \code{!WORKSPACE n}       \tab where n is an integer between 1 and 32 gigabytes. \cr
#' \code{!CONTINUE [f] or !FINAL [f] }       \tab instructs Echidna to retrieve variance parameters from an .esv file. \cr
#' \code{!VIEW }    \tab  displays any Winteracter graphics directly to the screen, such as '!view !PNG' \cr
#' \code{!DEBUG}          \tab requests additional debugging output to be written. \cr
#' \code{!LOGFILE}          \tab receive the DEBUG information. \cr
#' \code{!EQN [q]}          \tab sets the !EQN qualifier which selects the equation ordering option, q could be 1~7 or 77. \cr
#' \code{others}           \tab !RENAME,!ARGS,!OUTFOLDER, no needs in R. 
#' }
#' 
#' \cr
#' \tabular{ll}{
#' \strong{qualifier} \tab \strong{Description} \cr
# \code{!MAXIT m}     \tab sets the maximum number of iterations to m,13(default). \cr
#' \code{!EXTRA n}     \tab forces n more iterations after convergence . \cr
#' \code{!SINGLE}      \tab forces Echidna not to use Parallel Processing. \cr
#' \code{!SLOW}      \tab reduces stepsize when updating variance parameters so convergence is slower but
#' possible more reliable. \cr
#' \code{!SKIP i}      \tab indicating the file contains i heading lines to be ignored. \cr
#' \code{!READ f}      \tab specifying that f data fields are to be read. \cr
# \code{!YHT}         \tab requests the residuals and fitted values. \cr
#' \code{!FILTER Variable !SELECT value !EXCLUDE value}         \tab subset the data sets. \cr
#' \code{!MVINCLUDE}   \tab design variables which are missing are assumed to be zero.\cr
#' \code{!MVREMOVE}    \tab data records are ignored if any design variables have missing values.
#' }
#' 
#' \cr
#' \tabular{ll}{
#' \strong{GRM qualifiers } \tab \strong{Description} \cr
#' \code{!SKIP i}       \tab to skip the first i lines of the grm/giv file. \cr
#' \code{!LDET d}       \tab lets you set the logDet value for the supplied giv matrix. \cr
#' \code{!ADD value}    \tab adds value to the diagonal of a GRM matrix before inversion.value can only be 1, 2 or 3. \cr
#' \code{!PSD}          \tab allows a GRM matrix to be positive semidefinite. \cr
#' \code{!NSD}          \tab allows a GRM matrix to be negative semidefinite. \cr
#' \code{!ND}           \tab allows a GRM matrix to be negative definite. 
#' }
#' 
#' \cr Data fields: if VARIABLE is a CLASS variable, the NAME must be followed by a qualifier indicating it
#' is a class variable.
#'
#' \tabular{ll}{
#' \strong{class qualifier} \tab \strong{Description} \cr
#' \code{*}               \tab variable is coded 1:n. \cr
#' \code{!I n}            \tab variable is coded with INTEGER labels, not 1:n. \cr
#' \code{!A n}            \tab variable is coded with ALPHANUMERIC labels. \cr
#' \code{!L <labels>}     \tab data is coded 1:n and <labels> is the list of n class names. \cr
#' \code{!AS <factor> }   \tab when two or more alphanumeric variables have a common list of class names. \cr
#' \code{!LL c }          \tab reset the maximum length to c characters. \cr
#' \code{!PRUNE }         \tab reset the number of levels (n) in a factor to the real maximum levels. 
#' }
#'
#' \cr PEDIGREE FILE: ifile contains 3 or 4 fields being the identity (tag, id, name) of
#'        an individual, its Sire and its Dam..
#'
#' \tabular{ll}{
#' \strong{qualifier} \tab \strong{Description} \cr
#' \code{!SKIP i}        \tab the file contains i heading lines to be ignored. \cr
#' \code{!CSV}           \tab file is strictly COMMA delimited, without .csv suffix. \cr
#' \code{!GROUPS g }     \tab first g lines are genetic groups. . \cr
#' \code{!MGS}           \tab  the third field is a maternal grandsire (not DAM). \cr
#' \code{!SAVE f }       \tab requests the inverse be written as a .giv (f=1, ascii) or .bgiv (f=3, real binary) file. 
#' }
#'
#' 
#' \cr VPREDICT statements have the syntax as: <Key Letter> <Label> <arguments>.
#'    <Key Letter> is one of the letters F, H, R, V, etc.
#'
#' \tabular{ll}{
#' \strong{Key Letter } \tab \strong{Description} \cr
#' \code{F}       \tab specifies a linear function like animal + units. \cr
#' \code{H}       \tab specifies a ratio (heritability) like animal / Total. \cr
#' \code{R}       \tab specifies correlations from a matrix or formula. \cr
#' \code{V}       \tab converts a FA structure to a US structure. \cr 
#' \code{X}       \tab is a multiply function. \cr
#' \code{S}       \tab is a square root function. \cr
#' \code{K}       \tab sets short vectors for constants (e.g. Legendre coefficients) to be used for M. \cr
#' \code{M}       \tab uses the K coefficients to convert US (us(leg(dim,3))) matrix for a specific dim. \cr
#' \code{C}       \tab moves components back e.g. C label I[:II]=J[:JJ] where I < J. \cr
#' \code{D}       \tab discards components eg D from 393. \cr
#' \code{W}       \tab writes components to f.vpc and their variance to f.vpv.
#' }
#' 
#' \cr Variance structures
#'
#' \tabular{ll}{
#' \strong{function} \tab \strong{Description} \cr
#' \code{id(),idv()}      \tab identity or scaled identity. \cr
#' \code{ar1(), ar1v()}   \tab autoregressive correlation or covariance matrix. \cr
#' \code{coru(), coruv(), coruh()}   \tab uniform correlation matrix, v for common variance,h for heterogeneous variance. \cr
#' \code{diag()}          \tab diagonal variance matrix. \cr
#' \code{grmk()}          \tab the kth GRM matrix. \cr
#' \code{mrmk()}          \tab multiple relationship matrices. \cr
#' \code{us()}            \tab unstructured variance matrix.\cr 
#' \code{facvk()}         \tab factor analytic (basic form). \cr
#' \code{xfak()}          \tab factor analytic (eXtended form). \cr
#' \code{rrk()}           \tab factor analytic: No specific variance. 
#' }
#' 
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016

#' @name Echidna.options

NULL
