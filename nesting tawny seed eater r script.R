#pacotes

require(bbmle)
require(glmulti)
require(vegan)
require(caret)


#dados
dados=read.table(pipe("pbpaste"), sep="\t", header=T);dados
dados[sapply(dados, is.character)] <- lapply(dados[sapply(dados, is.character)],  as.factor)
 str(dados)                                      
attach(dados)




#multimodel inference  and variable importance
mod=glmulti(OCCUPIED...1~.,data=dados,family="binomial",crit=aicc,method="h",confsetsize = 130 ,level=1, maxsize = 3)
coeficientes=coef.glmulti(mod,2);coeficientes
  summary.glmulti(mod,2)
weightable(mod)

#best models
top <- weightable(mod)
top <- top[top$aicc <= min(top$aicc) + 2,]


 



