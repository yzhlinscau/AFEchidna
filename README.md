[![DOI](https://zenodo.org/badge/115507374.svg)](https://zenodo.org/badge/latestdoi/115507374)

# AFEchidna
Added functions for Echidna in R

## About AFEchidna

 This package AFEchidna adds some R functions for Echidna v-154. AFEchidna builds on the Echidna software. AFEchidna is for non-commercial academic use. Echidna is targeted for use in animal and plant breeding contexts by Arthur Gilmour. The primary software of Echidna could be downloaded from website (https://www.echidnamms.org/). Echidna is free and a powerful tool for mixed models. AFEchidna is developed to run Echidna in R and similar to asreml at some extent.

## INSTALL AFEchidna package
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

## more examples

Will be updated in the future....
