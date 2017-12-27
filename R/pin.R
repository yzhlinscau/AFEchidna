pin <-
function(object, formula=NULL,signif=NULL, corN=NULL,Rdf=NULL,digits=3){
  if(is.null(signif)) signif=FALSE
  if(is.null(Rdf)) Rdf=FALSE
  
  sig.level<-function(tvalue,se,...){
    n<-length(se)
    siglevel<-1:n
    for(i in 1:n){    
      sig.se<-c(se[i]*1.450,se[i]*2.326,se[i]*3.090)  
      if(abs(tvalue[i])>sig.se[1]){siglevel[i]<-"*"}else{siglevel[i]<-"Not signif"}
      if(abs(tvalue[i])>sig.se[2]){siglevel[i]<-"**"}
      if(abs(tvalue[i])>sig.se[3]){siglevel[i]<-"***"}
    }
    siglevel
  }
  
  if(!is.null(formula)){
    transform<-formula
    #if(is.null(N)) N<-0
    pframe <- as.list(object$gammas)
    names(pframe) <- paste("Vc", seq(1, length(pframe)), sep = "")
    tvalue<-eval(deriv(transform[[length(transform)]], names(pframe)),pframe)
    X <- as.vector(attr(tvalue, "gradient"))
    X[object$gammas.type == 1] <- 0
    tname <- if(length(transform)==3){transform[[2]]}else ""
    n <- length(pframe)
    i <- rep(1:n, 1:n)
    j <- sequence(1:n)
    k <- 1 + (i > j)
    Vmat <- object$ai
    se <- sqrt(sum(Vmat * X[i] * X[j] * k))
    
    vv<-NULL
    vv[1]=round(tvalue,digits);vv[2]=round(se,digits)
    
    result<-data.frame(row.names=tname, Estimate=tvalue, SE=se)
    result1<-result
    result1$sig.level<-sig.level(tvalue,se)
  
    cat("\n")
    #options(digits=digits)
    if(signif==TRUE){ 
      print(result1)
      cat("---------------")
      cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")    
    }else{
      if(Rdf==TRUE) print(vv) else print(result)
      }
    cat("\n")
  }
  
  if(is.null(formula)){
    
    if(is.null(corN)){cat("\nAttension: since no N value, here just show fisrt one corr!!\n\n")
                    corN<-1} 
    n<-corN
    df<-summary(object)$varcomp
    tvalue<-as.vector(df[1:n,2])
    se<-as.vector(df[1:n,3])
    tname<-rownames(summary(object)$varcomp)[1:n]    
    siglevel<-sig.level(tvalue,se)
    
    options(digits=digits) 
    print(data.frame(row.names=tname,Estimate=tvalue, SE=se, sig.level=siglevel))
    cat("---------------")
    cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")    
  }
}

#=================================

panel.hist <- function(x, ...) {             #指定对角图形为直方图
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=F)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.cor <- function(x, y, digits = 2, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par( usr = c(0, 1, 0, 1))
  r <- cor(x, y,  use = "complete.obs" )
  if(r>0) color=4 else color=2
  ra <- cor.test(x, y, use = "complete.obs")$p.value
  txt <- round(r, digits); prefix = "";sig=1
  if(ra <= 0.1) {prefix <- ".";sig=1}
  if(ra <= 0.05) {prefix <- "*";sig=1}
  if(ra <= 0.01) {prefix <- "**";sig=1}
  if(ra <= 0.001) {prefix <- "***";sig=1}
  txt <- paste(txt, prefix, sep="\n")
  text(0.5, 0.5, txt, cex =sig, col=color)
}
