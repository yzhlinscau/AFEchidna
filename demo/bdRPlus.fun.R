##
## demo file for bdR.pin et al. 
##
 library(bdRPlus)
 library(breedR)
 res.animal <- remlf90(fixed = phe_X ~ 1,
                       random = ~ gg,
                       genetic = list(model = 'add_animal',
                       pedigree = globulus[, 1:3],
                       id = 'self'),
                       data = globulus)
 
 # output variance components
 bdR.var(res.animal)

 # test trait's norm 
 bdR.plot(res.animal)

 # calculate heritability 
 bdR.pin(res.animal, h2~V2/(V1+V2+V3))
 
