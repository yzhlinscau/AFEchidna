[![DOI](https://zenodo.org/badge/115507374.svg)](https://zenodo.org/badge/latestdoi/115507374)

# AFEchidna
Added functions for Echidna in R

## About AFEchidna

 This package AFEchidna adds some R functions for Echidna v-154. AFEchidna builds on the Echidna software. AFEchidna is for non-commercial academic use. 

## INSTALL package
``` r
remotes::install_github('yzhlinscau/AFEchidna')

AFEchidna::checkPack()  # check depended R packages
``` 

## AAfun function

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


