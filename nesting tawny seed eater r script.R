#pacotes
require(MuMIn)
require(lme4)
require(bbmle)
require(glmulti)
require(vegan)
require(caret)


#dados
dados=read.table(pipe("pbpaste"), sep="\t", header=T);dados
attach(dados)

#Dados padronizados
pred=decostand(dados,"standardize");pred

#testando maodelos completos(misto com ano,sem ano, linear com ano)
##modelo com ano como fator aleatorio
mod.glmm=glmer(OCCUPIED...1~BARE.GROUND+HEIGHT.LOWER.STRATUM+HEIGHT.HIGHER.STRATUM+SHRUB.COVER+fogo+SLOPE+(1|ano),family="binomial", data=dados)
#modelo sem ano
mod.atemporal=glm(OCCUPIED...1~HABITAT.TYPE+BARE.GROUND+SHRUB.COVER+HEIGHT.LOWER.STRATUM+HEIGHT.HIGHER.STRATUM+BURNING+VEGETATION.DENSITY+SLOPE                
,family="binomial", data=dados)
#modelo com ano como fator fixo
mod.ano=glm(OCCUPIED...1~HABITAT.TYPE+BARE.GROUND+SHRUB.COVER+HEIGHT.LOWER.STRATUM+HEIGHT.HIGHER.STRATUM+BURNING+VEGETATION.DENSITY+SLOPE+YEAR,family="binomial", data=dados)

AICctab(mod.atemporal,mod.ano,nobs=52,weights = TRUE, delta = TRUE, base = TRUE)

###model average se fosse usar estes modelos
mod.test<-model.sel (mod.glmm,mod.atemporal,mod.ano,rank=AICc);mod.test
av1=model.avg (get.models (mod.test, subset = delta < 2,beta ="none",rank=AIC,fit=T))
coefTable(av1)
varImp(mod.full, scale = FALSE)


#multimodel inference  and variable importance
dados2=dados[,-dados$YEAR]
dados2=cbind(OCCUPIED...1,dados2)
mod=glmulti(OCCUPIED...1~.,data=dados,family="binomial",crit=aicc,method="h", confsetsize=256,level=1)
coeficientes=coef.glmulti(mod,2);coeficientes
summary.glmulti(mod,2)
weightable(mod)
plot(mod,type="s")

top <- weightable(mod)
top <- top[top$aicc <= min(top$aicc) + 2,]


 
###Barplot  RVI
##importa os valores das RVI
teste=read.table(pipe("pbpaste"), sep="\t",header=T);teste
##especifica as margens do graf
par(mar=c(5,10,1,1))
###barplot
barplot(teste$RI, main="", horiz=TRUE,
        names.arg=teste$X,xlab="Relative Varibale Importance",col="black",las=1,xlim=c(0,1))


