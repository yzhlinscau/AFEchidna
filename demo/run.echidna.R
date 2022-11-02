##
## demo file for echidna et al. 
##
## library(AFEchidna)

 AFEchidna::read.example(package='AFEchidna', setpath=TRUE)
 # dir()

##  generate .es0 file
# get.es0.file(dat.file='fm.csv') # .es file
# get.es0.file(es.file='fm.es') # .es0 file

## running model
res11<-echidna(h3~1+Rep,
               #weights='h1',
               #family=esr_gaussian(),
               random=~Fam,
               residual=NULL,
               #delf=F,foldN='res11',
               es0.file="fm.es0")
 
 # output variance components
 Var(res11)

 # test trait's norm 
 plot(res11)

 # calculate heritability 
 pin11(res11,h2~V2*4/(V1+V2))
 
