## version: public

# update: 2020-03-13

#' @title Summary of some R basic functions
#'
#' @details
#'   This package would supply some functions for R. Details as following:
#'
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Description} \cr
#' \code{\link{datatable2}}    \tab Create an HTML table widget. \cr
#' \code{\link{mc.run}}    \tab multi-core run in R. \cr
# \code{\link{heatmap1}}    \tab Create a Heatmap. \cr
#' \code{\link{spd.plot}}    \tab Plot spatial data or Variogram.\cr
#' \code{\link{Var}}    \tab Output Variance components for R packages.
#' }
#'
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
# AFfR website:https://github.com/yzhlinscau/AFfR
#' @name AF.base
NULL


#======================================================
# Simple basic statistics
# @export
# bstat<-function(x){
#   N=length(x)
#   M=mean(x,na.rm=T)
#   SD=sd(x,na.rm=T)
#   CV=SD/M
#   R=range(x,na.rm=T)
#   res1=c(N,M,SD,CV,R)
#   names(res1)=c('No','mean','sd','cv','Max','Min')
#   return(res1)
# }

#======================================================
# update: 2020-03-13
#' @title multi-core run in R
#'
#' @details
#'   This page would show how to run with multi-core in R.
#'
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
# AFfR website:https://github.com/yzhlinscau/AFfR
#' @name mc.run
#' @examples
#' \dontrun{
#' 
#' code<-c('600519.ss','601398.ss')
#'
#' # stock.get is the main function
#' # using lapply and stock.get
#' mm<-lapply(code, stock.get)
#'
#' # using  stock.get with multi-core
#' library(doParallel)
#' cl <- makePSOCKcluster(detectCores()-1)
#' registerDoParallel(cl)
#'
#' ## multi-core run here
#' mm1<-foreach(code=code) %dopar% stock.get(code)
#'
#' stopCluster(cl)
#' }
NULL



#======================================================
#' @title  read file function
#'
#' @description
#' \code{read.file} This function read file similar to asreml.read.table().
#'
#' @details
#' Count error for h2 and corr value, also outputs significent level.
#' @aliases read.file
#' @param file	 File name.
#' @param header	 Whether file has header for Varialbes, TRUE(default).
#' @param sep	 Field separator character, ','(default).
#' @param dec	  Decimal points'  character, '.'(default).
#' @param ...	 Further arguments to be passed to read.table.
#' @return this returned a data.frame.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AFfR website:https://github.com/yzhlinscau/AFfR
#' @seealso See Also as \code{\link{read.example}}, \code{\link{fdata}}
#' @examples
#' library(AFEchidna)
#' AfEchidna::read.example(package = "AFfR", setpath = TRUE)
#' df<-AfEchidna::read.file(file="fm.csv", header=TRUE)
#' names(df)
#'
#' @export
read.file<-function(file,header=TRUE,sep=',',dec='.',...){
  df<-utils::read.table(file=file,header=header,sep=sep,dec=dec,...)
  aa<-names(df)

  sn<-grep('^[A-Z]{1}',aa)
  for(i in sn) df[,i]<-factor(df[,i])

  return(df)
}


#' @title read file list
#'
#' @description
#' \code{read.example} This function read file list under one package, or
#' sets working directory under package.
#'
#' @aliases read.example
#' @param package	 package name.
#' @param setpath	 Whether set working directory under package, FALSE(default).
#' @return this returned a path or file list.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AFfR website:https://github.com/yzhlinscau/AFfR
#' @seealso See Also as \code{\link{read.file}}, \code{\link{fdata}}
#' @examples
#' library(AFEchidna)
#' # read file list under a package
#' AFEchidna::read.example(package = "AFEchidna")
#'
#' # set working directory under a package
#' AFEchidna::read.example(package = "AFEchidna", setpath = TRUE)
#' getwd()
#'
#' @export
read.example <- function(package,setpath = FALSE) {
  
  path<-system.file("extdata", package = package)
  
  if (setpath== FALSE)  dir(path)
   else setwd(path)
  
  invisible(path)
}


#' @title format dataset
#'
#' @description
#' \code{fdata} This function will format dataset for Varialbes to factors.
#'
#' @details
#' This function formats dataset Varialbes between factor and numeric type.
#' When using for factor to numeric, \code{faS} should be a list of sites for
#' characteric and numeric type factor respectively, i.e., faS=list(c(2:4),c(1,5:8)),
#' with site 2:4 (character factors) and site c(1,5:8) (numeric factors), one of
#' them can be 0.
#' @aliases fdata
#' @param data	 dataset.
#' @param faS	 The column location of factor Variables.
#' @param FtN  Change factoric indexs to numeric indexs, default (F).
#' @param ped  Change factoric pedigree to numeric type, default (F).
#' @return this returned a formated dataset.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AFfR website:https://github.com/yzhlinscau/AFfR
#' @seealso See Also as \code{\link{read.file}}, \code{\link{read.example}}
#' @examples
#' library(AFEchidna)
#' names(mtcars)
#'
#' mtcars1<-fdata(mtcars,faS=c(2,8,9))
#'
#' data(sp,package='RSTAT2D')
#' sp1<-fdata(sp,faS=list(c(2:4),c(1,5:8)),FtN=TRUE)
#' str(sp1) # or head(sp1)
#' # list reformed levels
#' sp1$rf$Mum # or sp1$rf[[1]]
#'
#' # only change numeric-factor to numeric index
#' sp1a<-fdata(sp,faS=list(0,c(1,5:8)),FtN=TRUE)
#' # only change characteric-factor to numeric index
#' sp1b<-fdata(sp,faS=list(c(2:4),0),FtN=TRUE)
#'
#' sp2<-fdata(sp[,1:3],ped=TRUE)
#' sp3<-fdata(sp2,faS=1:3)
#'
#' @export
fdata<-function(data,faS=NULL,FtN=FALSE,ped=FALSE){
  data<-as.data.frame(data)
  if(!ped){
    if(!FtN){ # items changed into factors
      if(is.null(faS)){
        aa<-names(data)
        sn<-grep('^[A-Z]{1}',aa)
      } else sn<-faS

      for(i in sn) data[,i]<-factor(data[,i])
    }else{ # factors changed into numeric idexes
      if(!is.null(faS)) {
        nc=ncol(data)

        sn1<-unlist(faS[[1]]) # characteric factor
        nsn1=length(sn1)
        if(nsn1>0&min(sn1)!=0){
          data[,(nc+1):(nc+nsn1)]=data[,sn1]
          for(i in sn1){
            fln<-nlevels(data[,i])
            levels(data[,i])<-1:fln
          }
        }

        sn2<-unlist(faS[[2]]) # characteric factor
        sn<-c(sn1,sn2)
        sn<-sn[sn!=0]
        for(i in sn) data[,i]<-as.numeric(paste(data[,i]))

        if(nsn1>0&min(sn1)!=0){
          rf=list()
          for(i in 1:nsn1) rf[[i]]=unique(data[, c(sn1[i],(nc+i)) ])
          names(rf)<-names(data)[sn1]
          data=list(data=data,rf=rf)
        }
      }
    }
  }else{  # for pedigree files
    j=0
    data[,4:6]<-data[,1:3]
    for(i in c(2:3,1)){
      data[,i]<-as.factor(data[,i])
      fln<-nlevels(data[,i])
      levels(data[,i])<-1:fln+j
      data[,i]<-as.numeric(paste(data[,i]))
      j=j+fln
    }
  }
  return(data)
}

#' @usage filterD1(x, ..., except = NULL)
#' @rdname  AF.base
#' @export
# filter data functions
filterD1<-function (x, ..., except = NULL) {

  if(!require(dplyr)){stop('Need package: dplyr.\n')}

  res <- dplyr::filter(x, ...)
  res <- droplevels(res, except)
  if (nrow(res) == 0)
    warning("The resultant data.frame has 0 rows. Try str() on the result.\n")
  res
}

#======================================================
#' @title Plot spatial data or Variogram.
#'
#' @description
#' \code{spd.plot} This function plots spatial data or Variogram.
#'
#' @usage spd.plot(object,type="data",p.lbls=NULL,key.unit=NULL,
#'                 x.unit=NULL,y.unit=NULL,na=NULL,
#'                 color.p=NULL,mtitle=NULL)
#'
#' @param object	 Aim dataset.
#' @param type	 Type of dataset, default value is "data", when "Variogram" for Variogram.plot in spatial analysis in ASReml-R.
#' @param p.lbls	 Extra labels in figure title.
#' @param key.unit	 The unit of key, default value is 1.
#' @param x.unit	 Axis x least unit, default value is 1.
#' @param y.unit	 Axis y least unit, default value is 1.
#' @param na	 Transform NA to 0(na=0) or keep NA (default).
#' @param color.p	 Parameters of the colors for figures, default value is terrain.colors, it could be rainbow, heat.colors, cm.colors and topo.colors.
#' @export spd.plot
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AFfR website:https://github.com/yzhlinscau/AFfR
#' @examples
#' \dontrun{
#' library(AFEchidna)
#'
#' ######## example 1 plot regular spatial data
#' data(barley)
#'
#' aim.trait<-subset(barley,select=c(Row,Column,yield))
#' spd.plot(aim.trait)
#' spd.plot(aim.trait,color.p=topo.colors)
#' spd.plot(aim.trait,key.unit="Kg")
#' spd.plot(aim.trait,p.lbls="barley",x.unit=2,y.unit=1)
#'
#' 
#' #AR1XAR1--asreml V3.0
#' barley1.asr<-asreml(yield~Variety, rcov =~ ar1(Row):ar1(Column), data=barley)
#'
#' summary(barley1.asr)$Varcomp
#' plot(Variogram(barley1.asr),main="M1")
#'
#' aa=Variogram(barley1.asr)
#' spd.plot(aa,type="Variogram",color.p=topo.colors)
#'
#' ######## example 2 plot spatial data with NA's
#' data(ir.sp)
#'
#' ir.sp2<-ir.sp[,5:16] # order: Row,Col,h05,cw05,...
#' #ir.sp2<-subset(ir.sp,select=c(Row,Col,h05,cw05))
#'
#' sp1<-ir2r.sp(ir.sp2,row.max=10,col.max=20)
#'
#' aim.trait=subset(sp1,select=c(Row,Col,d10))
#' spd.plot(aim.trait,key.unit="cm")
#' spd.plot(aim.trait,color.p=topo.colors,na=0)
#' spd.plot(aim.trait,na=0,x.unit=3)
#' }
#'
#'


spd.plot<-
  function(object,type="data",p.lbls=NULL,key.unit=NULL,
           x.unit=NULL,y.unit=NULL,na=NULL,
           color.p=NULL,mtitle=NULL){
    #require(plyr)

    graphics::par(mar=c(4,4,2,2), cex.main=1)
    if(is.null(color.p)) color.p <- terrain.colors

    if(type=="data"){
      for(i in 1:2){
        object[,i] <- as.numeric(object[,i])}
      #object<-arrange(object,object[2],object[1])
      object <- object[order(object[2],object[1]),]
    }
    if(type=="Variogram"){
      object <- object[,-4]
      for(i in 1:2) object[,i] <- object[,i]+1
    }

    ncol <- max(object[2])
    lbls <- names(object[3])
    lbls2 <- paste(lbls,key.unit,sep="\n(")
    lbls2 <- paste(lbls2,"",sep=")")
    object.1 <- object[,3]
    df <- matrix(object.1,nrow=ncol,byrow=TRUE)
    if(is.null(na)) na <- 1
    if(na==0)  df[is.na(df)] <- 0.0001  # reduce effects of NA value in data

    x = 1 : nrow(df)
    y = 1 : ncol(df)


    if(is.null(x.unit)) x.unit <- 1
    if(is.null(y.unit)) y.unit <- 1
    x.axis <- seq(x.unit,max(x),by=x.unit)
    y.axis <- seq(y.unit,max(y),by=y.unit)

    if(is.null(p.lbls)) p.lbls <- ""
    else p.lbls <- paste(": ",p.lbls)

    if(is.null(key.unit)) lbls2 <- lbls
    else lbls2 <- lbls2

    if(is.null(mtitle)) mtitle <- paste("The Topography of ",lbls, p.lbls)
    else mtitle <- mtitle

    #windows(10,8)
    graphics::filled.contour(x, y, df, color.palette=color.p,
                   plot.title = title( main=mtitle,
                                       xlab="Col", ylab="Row"),
                   plot.axes = {
                     axis(1,x.axis)
                     axis(2,y.axis)
                   },
                   key.title = title(main=lbls2, cex.main=1.0)
    )
    #abline(v=0, h=seq(1, max(y), by=1),lty=2,col="grey75")
    #abline(h=0, v=seq(1, max(x), by=1),lty=2,col="gray75")

  }

#======================================================
#' @title Output Variance components for R packages
#'
#' @description
#' \code{Var} This function output Variance components for R packages.
#'
#' @details
#' Output Variance component for mixed model results from R packages.
#'
#' @param object  an object of mixed model results from R packages.
#' @param mulT  multi-trait model(default, FALSE).
#'
#' @export
Var <- function(object,...) {
  UseMethod("Var")
}
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' AFfR website:https://github.com/yzhlinscau/AFfR
#' @name  Var
#' @examples
#' library(AFEchidna)
#'
#' ## 00 data
#' data(butron.maize,package='agridat')
#' df<-butron.maize
#'
#' ## 01 nlme package
#' library(nlme) # V3.1-131
#'
#' nlm<-lme(yield~1+env,random=~1|male/female,
#'                na.action='na.omit',
#'                data=df)
#'
#' Var(nlm)
#'
#' ## 02 lme4 package
#' library(lme4) # V1.1-17
#'
#' lme<-lmer(yield~1+env+(1|male)+(1|female),
#'                data=df)
#'
#' Var(lme)
#'
#'  \dontrun{
#' ## 03 breedR package
#' library(breedR) # V0.12-1
#'
#' bdR <- remlf90(fixed = yield~1+env,
#'                       random = ~ male+female,
#'                       data=df)
#'
#' Var(bdR)
#'
#'
#' ## 04 asreml package
#' library(asreml) #V3.0
#'
#' asr <- asreml(fixed = yield~1+env,
#'                       random = ~ male+female,
#'                       na.method.X='include',
#'                       data=df)
#'
#' Var(asr)
#'
#' ##### special for breedR
#' library(breedR)
#'
#' data(globulus)
#' res.animal <- remlf90(fixed = phe_X ~ 1,
#'                       random = ~ gg,
#'                       genetic = list(model = 'add_animal',
#'                       pedigree = globulus[, 1:3],
#'                       id = 'self'),
#'                       data = globulus)
#' Var(res.animal)
#' }
#'
#'


#' @method Var lme
#' @rdname Var
#' @export

Var.lme <- function (object) {

  if(!require(msm)){stop('Need package: msm.\n')}

  df<-NULL
  #object<-nlm

  Var<-nlme::VarCorr(object)
  suppressWarnings(storage.mode(Var) <- "numeric")
  vc<-Var[!is.na(Var)]
  Var1<-matrix(vc,nrow=2,byrow=T)

  # change sd to se
  ncM<-ncol(Var1)
  pVar <-object$apVar
  par1<-attr(pVar, "Pars")

  #library(msm)
  SE<-NULL
  for(i in 1:ncM) SE[i]<-msm::deltamethod (
    stats::as.formula(paste('~ exp(x',i,')^2',sep='')),
    par1, pVar)

  Var1[2,]<-SE

  RFN<-rownames(Var)
  if(length(RFN)>2) RFN<-RFN[RFN!="(Intercept)"]
  if(length(RFN)==2) {
    #a<-as.character(~1|Fam)
    #a1<-gsub('1 /| ','',a[2])
    #a2<-gsub('[1|]','',a1)
    RFN<-c('Rfs',"Residual")
  }

  RFN1<-gsub(" =",'',RFN)

  Var1<-t(Var1)
  Var1<-as.data.frame(Var1)
  Var1$z.ratio<-Var1[,1]/Var1[,2]

  names(Var1)[1:2]<-c('componet','se')
  row.names(Var1)<-RFN1

  df<-Var1
  return(df)
}

#' @method Var lmerMod
#' @rdname Var
#' @export
Var.lmerMod <- function (object) {
  df<-NULL

  Var<-as.data.frame(summary(object)$Varcor)[,-2:-3]
  row.names(Var)<-Var[,1]
  Var<-Var[,-1]
  Var<-as.data.frame(Var)
  names(Var)<-c('componet','sd')

  df<-Var
  return(df)
}

#' @method Var asreml
#' @rdname Var
#' @export .Var.asreml
#' @export
#'
Var.asreml <- function (object) {
  return(.Var.asreml(object))
}

.Var.asreml <- function (object) {
  df<-NULL
  df<-as.data.frame(summary(object)$Varcomp)
  return(df)
}


#' @method Var remlf90
#' @rdname Var
#' @export
Var.remlf90 <- function (object,mulT=FALSE) {

  df<-NULL

  df<-as.data.frame(summary(object)$Var)

  df$gamma<-df[,1]/df[nrow(df),1]
  if(mulT==TRUE) df$gamma<-df[,1]

  df$z.ratio<-df[,1]/df[,2]

  const<-function(x){
    cons.v<-1:length(x)
    for(i in 1:length(x)){
      #if(abs(x[i])!=x[length(x)]) cons.v[i]='Positive'
      if(abs(x[i])<=1e-6) cons.v[i]='Boundary'
      else cons.v[i]='Positive'
    }
    #cons.v[length(x)]='Positive'
    cons.v
  }

  df$constraint<-const(df$gamma)

  df<-df[,c(3,1:2,4:5)]
  colnames(df)[2:3]<-c('component','std.error')

  #df
  return(df)
}



#======================================================
#' @title Create an HTML table widget for dataset
#'
#' @description
#' \code{datatable2} This function  creates an HTML widget to display
#' rectangular data (a matrix or data frame) using the JavaScript
#' library DataTables.
#'
# @details
# Output Variance component for mixed model results from R packages.
#'
#' @aliases datatable2
#' @param data  a data object (either a matrix or a data frame).
# @param mulT  multi-trait model(default, FALSE).
#'
#' @examples
#'
#' library(AFEchidna)
#' data("iris")
#' datatable2(iris)
#'
#' @export
datatable2<-function(data){
  if(!require(DT)){stop('Need package: DT.\n')}
  #data("iris")

  DT::datatable(data,
                rownames = FALSE, # remove row numbers
                filter = "top", # add filter on top of columns
                extensions = "Buttons", # add download buttons
                options = list(
                  autoWidth = TRUE,
                  dom = "Blfrtip", # location of the download buttons
                  buttons = c("copy", "csv", "excel", "pdf", "print"), # download buttons
                  pageLength = 5, # show first 5 entries, default is 10
                  order = list(0, "asc") # order the title column by ascending order
                )
  )
}

#======================================================
#' @export
siglevel <- function(tvalue,se,...){
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
  invisible(siglevel)
}

#======================================================
#' @title output PCs for AMMI analysis
#'
#' @description
#' \code{PC.res} This function outputs PCs for AMMI analysis.
#'
# @details
# Output Variance component for mixed model results from R packages.
#'
#' @aliases PC.res
#' @param model  an AMMI object.
#' @param pcN  keeping pc number (default, 2).
#'
#' @examples
#'
#' library(AFEchidna)
#' data(plrv,package='agricolae')
#' names(plrv) #str(plrv)
#'
#' library(agricolae)
#' model<- with(plrv,AMMI(ENV=Locality,GEN=Genotype,REP=Rep,
#'                        Y=Yield,console=F,PC=T))
#'
#' model$ANOVA # for aov
#' model$analysis # for pc
#'
#' PCs.aov<-PC.res(model,2) # for GEI aov
#' PCs.aov
#'
#' @export
## only keep the first two PCs
PC.res <- function(model,pcN=2) {
  maov<-model$ANOVA # for aov
  df.res<-maov['Residuals','Df'] # residual df
  MS.res<-maov['Residuals','Mean Sq']

  cat('GEI aov results:\n')
  print(maov['ENV:GEN',])
  cat('\n\n')

  GEI<-model$analysis # for pc
  row.names(GEI)
  nr<-nrow(GEI)
  for(i in 1:7) GEI[nr+1,i]<-sum(GEI[(pcN+1):nr,i])
  row.names(GEI)[nr+1]<-'PCres'

  GEI['PCres','acum']=GEI[nr,'acum']
  GEI['PCres','Mean.Sq']=round(GEI['PCres','Sum.Sq']/GEI['PCres','Df'],4)
  GEI['PCres','F.value']=round(GEI['PCres','Mean.Sq']/MS.res,4)
  GEI['PCres','Pr.F']=round(stats::pf(GEI['PCres','F.value'], GEI['PCres','Df'], df.res,lower.tail=F),4)
  GEI$GEIp<-round(100*GEI$Sum.Sq/sum(GEI$Sum.Sq[1:nr]),4)


  for(i in c(1,3:4,8)) GEI[nr+2,i]<-sum(GEI[1:nr,i])
  row.names(GEI)[nr+2]<-'PCtl'
  GEI['PCtl','Mean.Sq']=round(GEI['PCtl','Sum.Sq']/GEI['PCtl','Df'],4)
  GEI['PCtl','F.value']=round(GEI['PCtl','Mean.Sq']/MS.res,4)

  nr1<-nrow(GEI)

  return(GEI[c(1:pcN,nr1-1,nr1),])
}

## batch analysis for aov
#' @usage aov.batch(df,mod,nf=NULL,alpha=.05)
#' @rdname  AF.base
#' @export
aov.batch <- function(df,mod,nf=NULL,alpha=.05) {
  #library(agricolae)
  if(!require(agricolae)){stop('Need package: agricolae.\n')}

  res<-list()
  fit<-stats::aov(mod,data=df)
  aov.res<-summary(fit) # list

  res1<-list()
  if(is.null(nf)) {
    tt<-as.character(mod[[3]])
    fn<-tt[tt!="+"]
    nf<-length(fn)
  } else{
    fn<-names(df)[nf]
    nf<-length(fn)
  }

  for(j in 1:nf){
    res1[[j]]<-agricolae::duncan.test(fit,fn[j],alpha=.05)$groups
  }
  names(res1)<-fn

  res<-list(aov.res,res1)
  names(res)<-c('aov','comp1')
  res
}

#' @export
aovr.print<-function(res,idx){
  print(res[idx])
}


#' @export
checkPack<-function( choosemirror1 = FALSE,choosemirror2 = FALSE) {
  
  pkgs1<-c('agricolae','amap','desplot','dplyr','DT',
           'ggplot2','knitr',
           'msm','nadiv','pedigree','purrr',
           'readr','reshape','stringr','tidyr') # 
  pack<-pkgs1
  InsPack0(pack,"CRAN",choosemirror1=choosemirror1)

  
  pkgs2<-c('GeneticsPed')
  pack<-pkgs2
  
  if (getRversion() < "3.6") {
    source("https://bioconductor.org/biocLite.R")
    InsPack0(pack,"Bio1",choosemirror2 =choosemirror2)
  }
  if (getRversion() >= "3.6") {
    if (!"BiocManager" %in% utils::installed.packages()) utils::install.packages("BiocManager")
    InsPack0(pack,"Bio2",choosemirror2 =choosemirror2)
  }

}



#' @export
InsPack0<-function(pack,mode=c("CRAN", "Bio1", "Bio2" ),
                   choosemirror1 = FALSE,choosemirror2 = FALSE){
  
  mode <- switch(mode, "CRAN" = 1, 
                 "Bio1" = 2, "Bio2" = 3)
  
  if (as.numeric(mode)==1) {
    if (choosemirror1 == TRUE) 
      utils::chooseCRANmirror()
    for (i in 1:length(pack)) {
      if (!pack[i] %in% utils::installed.packages()) {
        print(paste("installing ", pack[i]))
        utils::install.packages(pack[i], dependencies = TRUE)
      }
      else print(paste(pack[i], ": already installed"))
    }
  }
  
  if (as.numeric(mode)==2) {
    if (choosemirror2 == TRUE) 
      utils::chooseBioCmirror()
    
    for (i in 1:length(pack)) {
      if (!pack[i] %in% utils::installed.packages()) {
        print(paste("installing ", pack[i]))
        BiocInstaller::biocLite(pack[i])
      }
      else print(paste(pack[i], ": already installed"))
    }
  }
  
  if (as.numeric(mode)==3) {
    if (choosemirror2 == TRUE) 
      utils::chooseBioCmirror()
    
    for (i in 1:length(pack)) {
      if (!pack[i] %in% utils::installed.packages()) {
        print(paste("installing ", pack[i]))
        BiocManager::install(pack[i])
      }
      else print(paste(pack[i], ": already installed"))
    }
  }

}
