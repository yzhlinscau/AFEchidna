
## AFEchidna manual
## version: 1.54
## update: 2021-08-25

library(AFEchidna)

citation('AFEchidna')

## update new version of Echidna 
AFEchidna::loadsoft(update=TRUE)

## 00 install AFEchidna from github

remotes::install_github('yzhlinscau/AFEchidna')

## 00 install AFEchidna from local files

## 01 install depended libraries

AFEchidna::checkPack()


## 02 show help messages
help(package=AFEchidna)

library(AFEchidna)
ls("package:AFEchidna")

##### manual examples
#path=r'(D:\Rtraining)'
#setwd(path)

## get examples from AFEchidna
AFEchidna::read.example(package='AFEchidna', setpath=TRUE)
dir()

## 10 generate .es0 file
# get.es0.file(dat.file='fm.csv') # .es file
# get.es0.file(es.file='fm.es') # .es0 file
# file.edit('fm.es') # check and edit .es0 file

#### 11 single-traits

dir(pattern='.es0')

file.edit("fm.es0")
## run echidna directly in R
res11<-echidna(h3~1+Rep,
               weights='h1',
               #family=esr_gaussian(),
               random=~Fam,
               residual=NULL,
               #delf=F,foldN='res11',
               es0.file="fm.es0")


# show result class
class(res11)
names(res11) # show element details


# Iteration procedure
trace(res11) 

# variance componets
Var(res11)

# wald table for fixed effects
wald(res11)
waldT(res11,term=c('mu','Rep'))

# model diagnosis
plot(res11)

# summary of key results
summary(res11)

## run pin functions
pin(res11) # show variance components sign

pin11(res11,h2~V2*4/(V1+V2)) # only one parameter
pin11(res11,h2~V2*4/(V1+V2),signif=T,all=T)
# output results into a data frame
h2res<-pin11(res11,h2~V2/(V2+V1),signif=T,all=T,Rres=T)

# for more than 2 parameters 
pin(res11,mulp=c(h2~V2*4/(V2+V1),
                     Vp~V2+V1),signif=T)

# output results into a data frame
ap.res <- pin(res11,mulp=c(h2~V2/(V2+V1),
                             h2f~V2/(V2+V1/4)),
            signif=T,Rres=T)

## get solutions for fixed and random effects
fix.eff<-coef(res11)$fixed
head(fix.eff)

ran.eff<-coef(res11)$random
head(ran.eff)
tail(ran.eff)

#ran.eff %>% filter(Level<5)

## get predictions 
res11p<-update(res11,predict='Fam')
pred<-predict(res11p)

pred$heads
head(pred$pred$pred1)
pred$ased


# get other results
IC(res11)  # for AIC and BIC


res11$Converge # iteration converge state
converge(res11)

#### 12 multi-traits

## run echidna directly in R
res12<-echidna(cbind(h3,h4)~Trait+Trait:Rep,
               random=~us(Trait):Fam,
               residual=~units.us(Trait),
               predict='Fam',mulT=T,
               qualifier = '!filter Spacing !select 1',
               es0.file="fm.es0")

# show result class
class(res12)
names(res12) # show element details

# variance componets
Var(res12)

# wald table for fixed effects
wald(res12)

# summary of key results
summary(res12)

# model diagnosis
plot(res12,mulT=T)

## run pin functions
pin(res12) # show variance components sign

pin11(res12,gcor~V3/sqrt(V2*V4)) # only one parameter
pin11(res12,gcor~V3/sqrt(V2*V4),signif=T,all=T)
# output results into a data frame
gcorr<-pin11(res12,gcor~V3/sqrt(V2*V4),signif=T,all=T,Rres=T)

# for more than 2 parameters  
pin(res12,mulp=c(gcor~V3/sqrt(V2*V4),
                     ecor~V6/sqrt(V5*V7),
                     h2A~V2*4/(V2+V5),
                     VpA~V2+V5),signif=T)

# output results into a data frame
ap.res<-pin(res12,mulp=c(gcor~V3/sqrt(V2*V4),
                     ecor~V6/sqrt(V5*V7),
                     h2A~V2*4/(V2+V5),
                     VpA~V2+V5),
            signif=T,Rres=T)


## get solutions for fixed and random effects
fix.eff<-coef.esR(res12)$fixed
head(fix.eff)


fix.eff %>% filter(Level>=2.0)

# fix.eff$logv<-c(T,F)
# fix.eff %>% filter(logv ==T) 

ran.eff<-coef(res12)$random
head(ran.eff)
tail(ran.eff)

## get predicitons 

pred<-predict(res12)

pred$heads
head(pred$pred$pred1)
pred$ased


# get other results
IC(res12)  # for AIC and BIC

trace(res12) # Iteration procedure  

res12$Converge # iteration converge state
converge(res12)


#### 13 single-trait by !cycle

res13<-echidna(trait=~h3+h4+h5,
               fixed=~1+Rep,
               random=~Fam,
               residual=~units,
               es0.file="fm.es0",
               predict='Fam',cycle=T)

# show result class
class(res13)
names(res13) # show element details

# variance componets
Var(res13)

# wald table for fixed effects
wald(res13)

# model diagnosis
plot(res13)  # for all traits

plot(res13,idx=1) # for 1th trait

# summary of key results
summary(res13)

## run pin functions
pin(res13) # show variance components sign

pin11(res13,Vp~V1+V2,idx=2) # for 2th trait
pin11(res13,Vp~V1+V2,idx=2,signif=T,all=T)
# output results into a data frame
Vpres<-pin11(res13,Vp~V1+V2,idx=2,signif=T,all=T,Rres=T)

# for more than 2 parameters
pin(res13,mulp=c(h2~V2/(V2+V1),
               h2f~V2/(V2+V1/4)),signif=T)

# output results into a data frame
ap.res<-pin(res13,mulp=c(h2~V2/(V2+V1),
                       h2f~V2/(V2+V1/4)),
            signif=T,Rres=T)


## get solutions for fixed and random effects
sol.res<-coef(res13)
names(sol.res)

# trait order
idx<-1 

## fixed effects
sol.res[[idx]]$fixed

## random effects
ran.eff<-sol.res[[idx]]$random
head(ran.eff)
tail(ran.eff)


## get predicitons  

pred.res<-predict(res13)

names(pred.res)

idx<-1 # trait order

names(pred.res[[idx]])

pred.res[[idx]]$heads

## show all prediction names
names(pred.res[[idx]]$pred)

# 1th prediction
head(pred.res[[idx]]$pred$pred1$pred)
pred.res[[idx]]$pred$pred1$ased

# # 2nd prediction
# pred.res[[idx]]$pred$pred2

# get other results
IC(res13)  # for AIC and BIC

trace(res13) # Iteration procedure

converge(res13) # iteration converge state



## 14 spatial analysis 

#dir(pattern='.es0')

file.edit("barley.es0")
m1<-echidna(yield~1+variety,
               random=~Rep,
               residual=~units,
               predict=c('variety'),
               es0.file="barley.es0")

Var(m1)

plot(m1)


m2a<-echidna(yield~1,
             random=~Variety+units,
            residual=~ar1(Row):ar1(Column),
            predict=c('Variety'),
            es0.file="barley.es0")

Var(m2a)
plot(m2a)

m2b<-update(m2a,random=~Variety) 

model.comp(m2a,m2b,LRT=T)



df<-read.csv(file="barley.csv",header=T)
str(df)

df$resid1<-m1$yht$Residual
df$resid2<-m2a$yht$Residual

resid1<-subset(df,select=c(Row,Column,resid1))

spd.plot(resid1)

resid2<-subset(df,select=c(Row,Column,resid2))

spd.plot(resid2)

## 15 gblup analysis 


file.edit("G.data.es0")


ablup<-echidna(t1~1+Site,random=~nrm(ID),
            residual=~units,
            predict=c('ID'),
            es0.file="G.data.es0")

Var(ablup)
wald(ablup)

ebv<-coef(ablup)$random
head(ebv)

gblup<-echidna(t1~1+Site,
               random=~giv(ID),
               residual=~units,
               predict=c('ID'),
               es0.file="G.data.es0")

Var(gblup)

gebv<-coef(gblup)$random
head(gebv)

base::plot(ebv$Effect,gebv$Effect)
cor(ebv$Effect,gebv$Effect)


##batch--Gblup
# Gblup.mG<-update(ablup, random=c(G1~grm1(ID),
#                                  G2~grm2(ID),
#                                  G3~grm3(ID),
#                                  G4~grm4(ID),
#                                  G5~grm5(ID)),
#                  batch.G=T)
# 
# Gblup.mG2<-b2s(Gblup.mG)
# lapply(Gblup.mG2, Var)

# data(MET,package='RSTAT2D')

file.edit("MET.es0")

mm<-echidna(trait='yield',fixed='Loc',
            random='Genotype.xfa1(Loc)',
               residual='sat(Loc).units', #ar1(Col).ar1(Row)
               #predict=c('Genotype'),
               vpredict=c('V Vmat Genotype.xfa1(Loc)','R cor 20:40'),
               qualifier='!maxit 50',met=T,
               es0.file="MET.es0")

wald(mm)
waldT(mm,term=c('mu','Loc'))

summary(mm)

Var(mm)

names(mm)

cat(mm$evp)

df<-read.csv('MET.csv')

siteV<-unique(df['Loc']) # should be a data.frame or vector

met.corr(mm,siteV=siteV)
met.biplot(mm,siteV=siteV,biplot=F,dSco=1.0,dLam=0.8)


## using vpredict statement

vvm<-met.vmat(mm,siteV=siteV,VmatN='Vmat',corN='cor')

vvm$var

vvm$res$vmat




#### 2  batch analysis

## 2.1 single-trait 


res21<-echidna(trait=~h3+h4+h5,
               fixed=~1+Rep,
               random=~Fam,
               residual=~units,
               batch=TRUE,#run.purrr=T,
               es0.file='fm.es0')

names(res21)

res21b<-b2s(res21)
lapply(res21b,Var)

h3.res<-res21b$h3
Var(h3.res)
pin(h3.res)
pin(h3.res,mulp=c(h2~V2*4/(V1+V2)))

fix.eff<-coef(h3.res)$fixed
head(fix.eff)

ran.eff<-coef(h3.res)$random
head(ran.eff)

IC(h3.res)
converge(h3.res)


#### 2.2 multi-trait 


res22<-echidna(trait=~h2+h3+h4+h5,fixed=~Trait+Trait:Rep,
                   random=~us(Trait):Fam,
                   residual=~units:us(Trait),
                   predict='Fam',
                   batch=TRUE,mulT=TRUE,
                   #run.purrr=TRUE,
                   es0.file='fm.es0')

names(res22)

res22b<-b2s(res22)
lapply(res22b,Var)

h3h4.res<-res22b[[1]]
Var(h3h4.res)
pin(h3h4.res)
pin(h3h4.res,mulp=c(h2.h3~V2*4/(V2+V5),
                    h2.h4~V4*4/(V4+V7),
                    gcorr~V2/sqrt(V3*V4)),signif=T)

fix.eff<-coef(h3h4.res)$fixed
head(fix.eff)

fix.eff %>% filter(Level<2.0)

ran.eff<-coef(h3h4.res)$random
head(ran.eff)

ran.eff %>% filter(Level<2.0) %>% head()

IC(h3h4.res)
converge(h3h4.res)

pred.h3h4<-predict(h3h4.res)

names(pred.h3h4)

pred.h3h4$heads

head(pred.h3h4$pred$pred1)

pred.h3h4$pred$pred1 %>% filter(Trait=='h3') %>% head()

pred.h3h4$ased

##### 2.3 multi-G

res23<-echidna(es0.file="fm.es0",
             fixed=h5~1+Rep,
             random=c(G1~Fam,G2~Fam+Plot),
             residual=~units,
             batch.G=T,#run.purrr=T,
             trace=T)


res23b<-b2s(res23)

lapply(res23b, Var)

##### 2.4 multi-R
dir(pattern=('MET.es0'))

res24<-echidna(es0.file="MET.es0", 
             fixed=yield~1+Loc,
             random=~Genotype:Loc,
             residual=c(R1~sat(Loc):ar1(Col):ar1(Row),
                        R2~sat(Loc):units), 
             batch.R=T, #run.purrr=T,
             met=T)

res24b<-b2s(res24)

lapply(res24b, Var)


########## 3.1 binomial trait

#data(dfm2,package='RSTAT2D')

#write.csv(dfm2,file='dfm2.csv',quote=F,row.names = F)

get.es0.file(dat.file='dfm2.csv')
get.es0.file(es.file='dfm2.es')

file.edit('dfm2.es0')

# parent model
bp.esr<-echidna(lt ~ 1, random =~ Mum, 
                family = esr_binomial(), 
                es0.file = 'dfm2.es0' )

Var(bp.esr)
pin(bp.esr)
pin11(bp.esr,h2~4*V2/(V1+V2)) 
plot(bp.esr)

bp2.esr<-echidna(cbind(lt,dis) ~ Trait, 
                 random =~ us(Trait):Mum,
                 residual=~ units:us(Trait),
                family = c(esr_binomial(),esr_binomial()), 
                mulT=TRUE,
                es0.file = 'dfm2.es0' )

Var(bp2.esr)

bp3.esr<-echidna(cbind(h5,lt) ~ Trait, 
                 random =~ us(Trait):Mum,
                 residual=~ units:us(Trait),
                 family = c(esr_gaussian(),esr_binomial()), 
                 mulT=TRUE,
                 es0.file = 'dfm2.es0' )

Var(bp3.esr)

# tree model
bt.esr<-echidna(lt~1, random =~ nrm(TreeID), 
                 family = esr_binomial(),
                 es0.file = 'dfm2.es0' )

Var(bt.esr)
pin11(bt.esr,h2~V2/(V1+V2)) 


bt2.esr<-echidna(cbind(lt,dis) ~ Trait, 
                 random =~ us(Trait):nrm(TreeID),
                 residual=~ units:us(Trait),
                 family = c(esr_binomial(),esr_binomial()), 
                 mulT=TRUE,
                 es0.file = 'dfm2.es0' )

Var(bt2.esr)

bt3.esr<-echidna(cbind(h5,lt) ~ Trait, 
                 random =~ us(Trait):nrm(TreeID),
                 residual=~ units:us(Trait),
                 family = c(esr_gaussian(),esr_binomial()), 
                 mulT=TRUE,
                 es0.file = 'dfm2.es0' )

Var(bt3.esr)


########## 3.2 poisson trait

# parent model
spm.esr<-echidna(str ~ 1, random =~ Mum, 
                 family = esr_poisson(),
                 es0.file = 'dfm2.es0' )
cat(spm.esr$Iterations00)

Var(spm.esr)
pin(spm.esr)
pin11(spm.esr,h2~4*V2/(V1+V3)) 
plot(spm.esr)


# spm2.esr<-echidna(cbind(h5,str) ~ Trait, 
#                  random =~ us(Trait):Mum,
#                  residual=~ units:us(Trait),
#                  family = c(esr_gaussian(),esr_poisson()), 
#                  mulT=TRUE,
#                  es0.file = 'dfm2.es0' )

# tree model
stm.esr<-echidna(str~1, random =~ nrm(TreeID), 
                 family = esr_poisson(), 
                 es0.file = 'dfm2.es0' )

Var(stm.esr)
pin(stm.esr)
pin11(stm.esr,h2~V2/(V1+V2)) 



### complex model

get.es0.file(dat.file='pig_data.txt')

get.es0.file(es.file='pig_data.es',faS=1:6,pedS=1)
file.edit('pig_data.es0')


pm1<-echidna(weanwt~year+sex+weanage,
             random=~nrm(pig),
             es0.file='pig_data.es0')

Var(pm1)

pin(pm1)
pin(pm1,mulp=c(Va~V2,
               VP~V1+V2,
               h2i~V2/(V2+V1)) ) 

pm2<-echidna(weanwt~year+sex+weanage,
             random=~str(nrm(pig)+nrm(dam),~us(2):nrm(pig)),
             es0.file='pig_data.es0')

Var(pm2)


# at(Trait,1):weanage, problem in .ess file
pm3<-echidna(fixed = cbind(weanwt,weight)~Trait:(year+sex+weanage+pen),            
             random=~str(~Trait:nrm(pig)+Trait:dam,~us(4):nrm(pig)),
             residual=~idv(units):us(Trait),
             mulT = T,
             es0.file='pig_data.es0')

Var(pm3)


####### CODES END #######

