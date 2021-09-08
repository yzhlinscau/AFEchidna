[![DOI](https://zenodo.org/badge/115507374.svg)](https://zenodo.org/badge/latestdoi/115507374)

# AFEchidna
Added functions for Echidna in R

## About AFEchidna

 This package AFEchidna adds some R functions for Echidna v-154. AFEchidna builds on the Echidna software. AFEchidna is for non-commercial academic use. Echidna is targeted for use in animal and plant breeding contexts by Arthur Gilmour. The primary software of Echidna could be downloaded from website (https://www.echidnamms.org/). Echidna is free and a powerful tool for mixed models. AFEchidna is developed to run Echidna in R and similar to asreml at some extent.

## Install AFEchidna package
``` r
remotes::install_github('yzhlinscau/AFEchidna')

AFEchidna::checkPack()  # check depended R packages
``` 

## AFEchidna function

  - echidna() to run mixed model or batch analysis;
  - pin() to calculate heritability or corr with se;
  - model.comp() to run model comparisons;
  - met.corr() to get cov/var/corr matrix for FA models;
  - met.plot() to plot MET data;
  - met.biplot() to run biplot for MET factor analytic results;
  - etc...

## DEMO functions
``` r
library(AFEchidna)

demo('run.echidna')

```

## AFEchidna workflow
- (1) generate temple .es0 file;
- (2) specify the mixed model;
- (3) run program and check the results.


## more examples

### single trait
``` r
# generate .es0 file
# get.es0.file(dat.file='fm.csv') # .es file
# get.es0.file(es.file='fm.es') # .es0 file
# file.edit('fm.es') # check and edit .es0 file

res11<-echidna(h3~1+Rep,
               random=~Fam,
               residual=NULL,
               es0.file="fm.es0")

# variance componets
Var(res11)

# only one parameter
pin11(res11,h2~V2*4/(V1+V2))

# model diagnosis
plot(res11)

## get solutions for fixed and random effects
fix.eff<-coef(res11)$fixed
head(fix.eff)

ran.eff<-coef(res11)$random
head(ran.eff)
tail(ran.eff)

## get predictions 
res11p<-update(res11,predict='Fam')
pred<-predict(res11p)

pred$heads
head(pred$pred$pred1)
pred$ased
```

### single trait--batch analysis
``` r
res21<-echidna(trait=~h3+h4+h5,
               fixed=~1+Rep,
               random=~Fam,
               residual=~units,
               batch=TRUE,#run.purrr=T,
               es0.file='fm.es0')

names(res21)

res21b<-b2s(res21)
lapply(res21b,Var)

```

More examples will be updated in the future....
