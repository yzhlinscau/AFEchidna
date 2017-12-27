\name{model.comp}
\alias{model.comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model comparison for asreml-R objection.
}
\description{
This function would compare models with different random structure under the same fixed factors.
}
\usage{
model.comp(m1=NULL,m2=NULL,Nml=NULL,mulM=NULL,LRT=NULL,rdDF=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{
Model 1 asreml results.
}
  \item{m2}{
Model 2 asreml results.
}
  \item{Nml}{
A vector with more than 2 asreml results.
}
  \item{mulM}{
mulM is "TRUE" for a vector's comparison with multi-models, default (FALSE).
}
  \item{LRT}{
Value TRUE for Likelihood ratio test (LRT), default (FALSE) for no LRT.
}
  \item{rdDF}{
Value TRUE to control Ddf minus 0.5, default FALSE for LRT with Ddf. If TRUE, Ddf would minus 0.5, ie, Ddf=Ddf-0.5 .
}
}

\value{
%%  If it is a LIST, use
\item{m1 }{An asreml result,such as "m1.asr"}
\item{Nml }{A vector, such as "c(m1,m2,m3,m4)"}
%% ...
}
\author{
Yuanzhen Lin <yzhlinscau@163.com>
}
\references{
Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
}
\seealso{
Website for instant update: yzhlin-asreml.ys168.com
}

\examples{

library(asreml)
library(AAfun0s)

df<-PrSpa
fm<-asreml(h5~ 1+Rep,random=~ Fam, 
            subset=Spacing=='3',data=df,maxit=40)
            
fm1a<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
            random=~ us(trait):Fam, rcov=~units:us(trait),
            subset=Spacing=='3',data=df,maxit=40)
fm1b<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
            random=~ diag(trait):Fam, rcov=~units:us(trait),
            subset=Spacing=='3',data=df,maxit=40)

fm3a<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
            random=~ diag(trait):Fam, rcov=~units:us(trait),
            subset=Spacing=='3',data=df,maxit=40)
            
fm3b<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
            random=~ diag(trait):Fam, rcov=~units:us(trait),
            subset=Spacing=='3',data=df,maxit=40)

#####   model comparison    #####
model.comp(m1=fm1a,m2=fm1b)
model.comp(m1=fm1a,m2=fm1b,LRT=TRUE)
model.comp(m1=fm1a,m2=fm1b,LRT=TRUE,rdDF=TRUE)

model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE)
model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE,LRT=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model.comp }
\keyword{ ASReml.comp }% __ONLY ONE__ keyword per line
