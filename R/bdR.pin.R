#' @title Count error for h2 in BreedR
#' 
#' @description 
#' \code{bdR.pin} This function counts standard error(se) for heritability(h2) and corr value 
#' and also outputs significent level for corr value in BreedR package.
#' 
#' @details 
#' counts standard error(se) for heritability(h2)
#' 
#' @param object an object of BreedR result
#' @param formula formula for h2 (or corr, not now)
#' @param signif	Index to output signif levels, F(default) for non-signif.
#' @param digit a number to control decimal point,5(default).
#' @export
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references 
#' bdRPlus website:https://github.com/yzhlinscau/AAfun
#' @examples 
#' library(breedR)
#' res.animal <- remlf90(fixed = phe_X ~ 1,
#'                       random = ~ gg,
#'                       genetic = list(model = 'add_animal',
#'                       pedigree = globulus[, 1:3],
#'                       id = 'self'),
#'                       data = globulus)
#' bdR.pin(res.animal, h2~V2/(V1+V2+V3))

bdR.pin <- function(object,formula,signif=FALSE,digit=5) {
  #require(msm, quietly = TRUE)
  #require(AAfun)
  
  dd<-gsub('V','x',formula)
  formula<-as.formula(paste(dd[2],dd[3],sep=' ~ '))
  
  transform<-formula
  aa1<-bdR.var(object)
  aa<-aa1[,"Estimated variances"]
  pframe <- as.list(aa)
  names(pframe) <- paste("x", seq(1, length(pframe)), sep = "")
  tvalue<-eval(deriv(transform[[length(transform)]], names(pframe)),pframe)
  tname <- if(length(transform)==3){transform[[2]]}else ""
  
  invAI <- object$reml$invAI
  se <- deltamethod(transform,aa,invAI)
  
  result<-data.frame(row.names=tname, Estimate=tvalue, SE=se)
  result1<-result
  result1$sig.level<-sig.level(tvalue,se)
  
  cat("\n")
  options(digits=digit)
  if(signif==TRUE){ 
    print(result1)
    cat("---------------")
    cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")    
  }else print(result)
  cat("\n")
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# sig.level functions

sig.level<-function(tvalue,se,...){
  n<-length(se)
  siglevel<-1:n
  for(i in 1:n){    
    sig.se<-c(se[i]*1.450,se[i]*2.326,se[i]*3.090)  
    
    if(abs(tvalue[i])>sig.se[3]) {siglevel[i]<-"***"}
    else if(abs(tvalue[i])>sig.se[2]) {siglevel[i]<-"**"}
    else if(abs(tvalue[i])>sig.se[1]) {siglevel[i]<-"*"}
    else {siglevel[i]<-"Not signif"}
  }
  siglevel
}

##------------------------------
bdR.var <- function (object) {
  df<-as.data.frame(summary(object)$var)
  df$z.ratio<-df[,1]/df[,2]
  return(df)
}

##------------------------------
# test trait's norm
bdR.plot <- function (object) {
  par(mfrow=c(2,2))
  hist(residuals(object),main='',xlab='Residuals',col='blue')
  qqnorm(residuals(object),main='',col='blue',ylab='Residuals')
  plot(fitted(object),residuals(object),xlab='Fitted',ylab='Residuals',col='blue')
  abline(h=0)
  plot(1:length(fitted(object)),residuals(object),xlab='Unit Number',ylab='Residuals',col='blue')
  abline(h=0)
  par(mfrow=c(1,1))
}

