\name{pin}
\alias{pin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count error for h2 and corr
}
\description{
This function counts standard error(se) for heritability(h2) and corr value and also outputs significent level for corr value in ASReml-R package.
}
\usage{
pin(object, formula=NULL, signif=NULL, corN = NULL, Rdf=NULL,digits=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
asreml results
}
  \item{formula}{
formula for h2 or corr
}
  \item{signif}{
Index to output signif levels, F(default) for non-signif.
}
  \item{corN}{
Number of corr, 1(default).
}
  \item{Rdf}{
Index to output results to vector, F(default) for non-vector output.
}
  \item{digits}{
Index to control digit of decimal point, 3(default).
}
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

data(PrSpa)
df<-PrSpa

# exmaple 1 for sigle trait model
fm<-asreml(h5~1+Rep, random=~Fam, subset=Spacing=='3',data=df)
summary(fm)$varcomp[,1:3]
pin(fm, h2 ~4*Vc1/(Vc1+Vc2))
pin(fm, h2 ~4*Vc1/(Vc1+Vc2),Rdf=TRUE)

# exmaple 2 for us model
fm2<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
              random=~ us(trait):Fam, rcov=~units:us(trait),
              subset=Spacing=='3',data=df,maxit=40)

summary(fm2)$varcomp[,1:3]

pin(fm2, h2_A ~ 4 * Vc1/(Vc1+Vc5)) # heritability for trait A
pin(fm2, h2_B ~ 4 * Vc3/(Vc3+Vc7)) # heritability for trait B

pin(fm2, gCORR ~ Vc2/sqrt(Vc1*Vc3),signif=TRUE) # genetic corr
pin(fm2, pCORR ~ (Vc2+Vc6)/sqrt((Vc1+Vc5)*(Vc3+Vc7)),signif=TRUE) # phenotype corr

# exmaple 3 for corr model
fm3<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
              random=~ corgh(trait):Fam, rcov=~units:us(trait),
              subset=Spacing=='3',data=df,maxit=40)

summary(fm3)$varcomp[,1:3]
pin(fm3,corN=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ asreml.pin }
\keyword{ corr.signif }% __ONLY ONE__ keyword per line
