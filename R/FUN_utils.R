## version: public

##################################################################################################

#Startup function

#this function is executed once the library is loaded

.onAttach = function(library, pkg)
  
{
  options(warn=-1)
  
  Rv = R.Version()
  
  if(!exists("getRversion", baseenv()) || (getRversion() < "4.0"))
    
    stop("This package requires R 4.0 or later")
  
  assign(".AFEchidna.home", file.path(library, pkg),
         
         pos=match("package:AFEchidna", search()))
  
  AFEchidna.version = "1.68 (2023-04-03)" # usually 2 months before it expires
  

  
  
  assign(".AFEchidna.version", AFEchidna.version, pos=match("package:AFEchidna", search()))
  
  if(interactive())
    
  {
    packageStartupMessage("AFEchidna builds on the Echidna software.",appendLF=TRUE)
    packageStartupMessage("AFEchidna is for non-commercial academic use.",appendLF=TRUE)
    packageStartupMessage("Please do not share AFEchidna with anyone without my permission.",appendLF=TRUE)
    
    #ssd <- Sys.Date()
    # getRid()
    
    message('\nWelcome to the world of AFEchidna.\n
          --------- Yuanzhen Lin (SCAU)')
  }
  
  #Echsf<-AFEchidna::loadsoft()
  #assign("Echsf", Echsf)#, envir = .GlobalEnv


 # invisible()
  
}


# @export  .getRid
#' @export
#getRid<-function(){return(.getRid())}

getRid<-function(){
  
  # usrp <- .libPaths()
  # #usrp <- usrp[1]
  # #usrp<-'C:/Users/HP/Miniconda3'
  # usrp1 <- usrp[grep('C:/Users',usrp)]# change here?
  # usrn <- strsplit(usrp1,'/')[[1]][3]
  # #usrn <- 'yzhlinscau2'
  
  usrn <- AFEchidna::id0()
  
  # change here
  if(usrn!='yzhlinscau'){
    if(usrn!='guobin') {
      stop('\nyou do not have the permission to use AFEchidna.\nPlease contact me(yzhlinscau@163.com).\n')
      #quit('yes')
      #break
    }else message('\nWelcome to the world of AFEchidna.\n--------- Yuanzhen Lin (SCAU)')
  } else message('\nWelcome to the world of AFEchidna.\n--------- Yuanzhen Lin (SCAU)')
  
 # invisible()
}

#' @export
id0<-function(){
  #usrp <-"D:/R-4.0.2/library"
  usrp <- .libPaths() 
  if(any(grepl('C:/Users',usrp))){
    usrp1 <- usrp[grep('C:/Users',usrp)]# change here?
    usrn <- strsplit(usrp1,'/')[[1]][3]
  }else usrn <- usrp

  return(usrn)
}


#' @export
loadsoft <- function(update=FALSE, soft.path=NULL){
  
  org.path <- getwd()
  
    path0 <- NULL
    softf <- NULL

    #path0 <-ifelse(.Platform$OS.type == "windows",  
    #               'C:/ProgramData/Echidna.bin', 
    #               '~/Echidna.bin')
    path0 <- 'C:/ProgramData/Echidna.bin'
    softf <- paste0(path0,'/Echidna.exe')
    #if(.Platform$OS.type != "windows" ) softf  <- paste('wine',softf,sep=' ')
    
    if(update==TRUE|!dir.exists(path0)){
      
      if(update==TRUE){
        setwd(path0)
        file.remove(dir())
      } else dir.create(path0)
      
      if(is.null(soft.path)) 
         soft.path <- system.file("extdata/bin", package = "AFEchidna")
         
      setwd(soft.path)
      file.copy(from=dir(),to=path0, overwrite=TRUE)
      
      setwd(path0)
      vfile<-dir(pattern='^[Vv]1.*')
      
      if(update==TRUE)
       cat('Echidna software has been updated to the latest version:',vfile,'.\n')
      
    }
  setwd(org.path) 
  
  return(softf)
}

##file.exists('C:/ProgramData/Echidna.bin/Echidna.exe')

# loadsoft(update=T)

#' @export
nfile.copy <- function(version=153, path0=NULL, path1=NULL) {
  
  if(is.null(path0))
   path0<-'D:\\myworks\\myRpackages\\public\\AFEchidna\\inst\\extdata\\bin'
  old.file<-dir(path0)
  setwd(path0)
  file.remove(old.file)
  if(is.null(path1))
   path1<-paste0('D:\\softs\\ASReml\\Echidna\\Echidna',version,'\\BIN')
  setwd(path1)
  all.file<-dir()
  for(i in 1:length(all.file))
    file.copy(from=all.file[i],to=path0, overwrite=TRUE)
  
  print(dir(path0))
  cat('file copy done!\n')
  
}

#' @export
############### set up softp for working in linux

linux.softp <- function() {
  path0 <- system.file("extdata/bin", package = "AFEchidna")
  softf <- paste0(path0, "/Echidna.exe")
  softp0 <- paste('wine',softf, sep=' ')
  return(softp0)
}

#linux.softp()


#' @export
subF1 <- function(fixed,random,residual,
                  es0.file,
                  trace=T,met=T,
                  subV.org,subV.Lv=NULL,
                  mulN=NULL,res.no=NULL,
                  run.purrr=F,
                  dat.file=NULL,...) {
  
  
  # subF function
  #if (as.numeric(mode)==4) {
    batch0 <- TRUE
    
    # get data file, maybe problem here!!!
    es0.txt <- base::readLines(es0.file)
    datL <- es0.txt[grep('\\!SKIP',es0.txt)] # >=1
    if(grepl('\\#',datL)) datL <- datL[-grep('\\#',datL)]
    lth <- length(datL)
    dat.file0 <- sub('\\s+\\!SKIP.*','',datL[lth])
    if(is.null(dat.file)) dat.file <- dat.file0 else dat.file <- dat.file 
    
    # data file
    dat <- read.csv(file=dat.file)
    
    # copy original data file and rename to an old.file
    org.datf <- paste0('old.',dat.file)
    write.csv(dat,file=org.datf,row.names=FALSE)
    dat <- read.csv(file=org.datf)
    
    dat$nSite <- dat[,subV.org]
    
    if(is.null(subV.Lv)){
      cat('Starting analysis.\n')
      
      cc <- unique(dat$nSite)
      
      if(is.null(mulN)) mulN <- 2 
      else mulN <- mulN
      
      bb <- utils::combn(cc,mulN)
      if(is.null(res.no)) bbn <- ncol(bb) 
      else bbn <-res.no
      
      run.fun4 <- function(x){
        #cc<-paste0('Site-',bb[1,x],':',bb[2,x])
        cat('\nAnalysing---- ',paste(append(paste0('Site-'),bb[,x]), collapse = ":"))
        
        dat22 <- dat %>% filter(.,nSite %in% bb[,x])
        #temp.datf<-paste0('new.',dat.file)
        write.csv(dat22,file=dat.file,row.names=FALSE)
        
        #AFEchidna::subsetcc<-paste(append(paste0('!subset ',subV.new,' ', subV.org),bb[,x]), collapse = " ")
        mm <- AFEchidna::run.mod(fixed=fixed,
                                 random=random,
                                 residual=residual,
                                 #qualifier = subsetcc,
                                 trace=trace,met=met,
                                 es0.file = es0.file)
      }
      
      ss <- vector("list", bbn)
      if(!run.purrr) ss <- lapply(1:bbn, run.fun4 ) 
      else ss <- 1:bbn %>% purrr::map(run.fun4 )
      
      names(ss) <- lapply(1:bbn, function(x) paste(append(paste0('Site-'),bb[,x]), collapse = ":"))
      cat('works done.\n')
      
    }else{
      dat22 <- dat %>% filter(.,nSite %in% subV.Lv)
      write.csv(dat22,file=dat.file,row.names=FALSE)
      
      ss <- AFEchidna::run.mod(fixed=fixed,
                               random=random,
                               residual=residual,
                               #qualifier = subsetcc,
                               trace=trace,met=met,
                               es0.file = es0.file)
    }
    
    dat$nSite <- NULL
    write.csv(dat,file=dat.file,row.names=FALSE)
    file.remove(org.datf)      
    #cat('works done.\n')
    
    tt <- NULL      
    if(is.null(subV.Lv)) tt$res.all <- ss else tt <- ss      

  class(tt) <- c('esR') 
  
}
