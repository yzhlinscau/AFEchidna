\name{asreml.batch}
\alias{asreml.batch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ASReml-R batch analysis
}
\description{
This function carries out batch analysis for mult-trait with same model and also output heritability etc. in ASReml-R package.
}
\usage{
asreml.batch(data,factorN,traitN,FMod=NULL,RMod=NULL, EMod=NULL,
                           mulT=NULL, mulN=NULL,mulR=NULL,
                           corM=NULL,corMout=FALSE,gformula=NULL,
                           pformula=NULL,pformula1=NULL,pformula2=NULL,
                           pformula3=NULL,pformula4=NULL,maxit=NULL,
                           ped=NULL,pedinv=NULL,ginverse=NULL,digits=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
aim dataset
}
  \item{factorN}{
A vector with sites of all factors.
}
  \item{traitN}{
A vector with sites of all traits.
}
  \item{FMod}{
Fixed mode.
}
  \item{RMod}{
Randomed variance structure.
}
  \item{EMod}{
Error variance structure for multi-trait model.
}
  \item{mulT}{
Value "T" or "TRUE" for multi-trait model, "F"(default).
}
  \item{mulN}{
Number of trait for one model in multi-trait analysis,2(default).
}
  \item{mulR}{
Value "T" or "TRUE" to count corr/error matrix, only works for bitrait, "F"(default).
}
  \item{corM}{
Value "T" or "TRUE" for corr model, "F"(default).
}
  \item{corMout}{
Value "T" or "TRUE" to output corr matrix, "F"(default).
}
  \item{gformula}{
Formula for corr.
}
  \item{pformula}{
Formula for h2 or corr.
}
  \item{pformula1}{
Formula for h2 or corr.
}
  \item{pformula2}{
Formula for h2 or corr.
}
  \item{pformula3}{
Formula for h2 or corr.
}
  \item{pformula4}{
Formula for h2 or corr.
}
  \item{maxit}{
Maximum number of iterations.
}
  \item{ped}{
Value "T" or "TRUE" for animal model with pedigree, "F"(default)..
}
  \item{pedinv}{
A G-inverse matrix for pedigree from ASReml.Ainverse(). 
}
  \item{ginverse}{
a named list with each component identifying a G-inverse matrix.
}
  \item{digits}{
Index to control digit of decimal point, 3(default).
}
}
\value{
%%  If it is a LIST, use
\item{factorN }{A vector contains column site of factors in dataset, such as c(1:3,5)}
\item{traitN }{A vector contains column site of aim trait in dataset, such as c(10:15)}
\item{FMod }{Fixed mode,must start in "y~1+fixed.factor" (single trait) or "cbind(y1,y2)~trait+trait:fixed.factor" (multi-trait, cbind(y1,y2,y3,...))}
\item{RMod }{Random variance structure, such as "~Fam" or "~us(trait):Fam". }
\item{EMod }{Error variance structure, such as "~units:us(trait)". }
\item{pformula}{formula for h2 or corr, such as "h2 ~ 4 * V1/(V1+V2)".}
\item{maxit}{maximum number of iterations, default is 20.}
%% ...
}
\author{
Yuanzhen Lin <yzhlinscau@163.com>
}
\references{
Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2017
}
\seealso{
Website for instant update: yzhlin-asreml.ys168.com
}

\examples{

library(asreml)
library(AAfun0s)

##### example I for dataset without pedigree
data(PrSpa)
df<-PrSpa

# exmaple 1.1 for sigle trait model
df1=subset(df,Spacing==3)
asreml.batch(data=df1,factorN=1:5,traitN=c(9:13),
             FMod=y~1+Rep+Plot,RMod=~Fam,
             pformula=h2 ~ 4 * Vc1/(Vc1+Vc2))

# exmaple 1.2 for us model 
asreml.batch(data=df1,factorN=1:5,traitN=c(10:13),
             FMod=cbind(y1,y2)~trait+trait:Rep,
             RMod=~us(trait):Fam,
             EMod=~units:us(trait),
             mulT=TRUE,mulN=2,mulR=TRUE,corMout=TRUE,
             pformula=r.g ~ Vc2/sqrt(Vc1*Vc3),
             pformula1=h2.A ~ 4*Vc1/(Vc1+Vc5),
             pformula2=h2.B ~ 4*Vc3/sqrt(Vc3+Vc7))


# exmaple 1.3 for corr model
asreml.batch(data=df1,factorN=1:5,traitN=c(10:13),
             FMod=cbind(y1,y2,y3)~trait+trait:Rep,
             RMod=~corgh(trait):Fam,
             EMod=~units:us(trait), maxit=30,
             mulT=TRUE,mulN=3,mulR=TRUE,corM=TRUE)
}