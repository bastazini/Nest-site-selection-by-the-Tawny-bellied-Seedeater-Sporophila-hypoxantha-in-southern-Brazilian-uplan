require(caret)
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
pred=(decostand(cbind(cob.solo,cob.estrat.med,alt.estrat.med,alt.estrat.alt,cob.lat,decliv),"standardize"));pred
ano=as.factor (ano)
fogo=as.factor (fogo)
amb=as.factor (amb)
dados1=cbind(ninho,ano,fogo,amb,pred);dados1
dados1=as.data.frame(dados1)
names(dados1)
#testando maodelos completos(misto com ano,sem ano, linear com ano)
##modelo com ano como fator aleatorio
mod.glmm=glmer(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv+(1|ano),family="binomial", data=dados1)
#modelo sem ano
mod.atemporal=glm(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv,family="binomial", data=dados1)
#modelo com ano como fator fixo
mod.ano=glm(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv+ano,family="binomial", data=dados1)

AICctab(mod.glmm,mod.atemporal,mod.ano,nobs=52,weights = TRUE, delta = TRUE, base = TRUE)

mod.test<-model.sel (mod.glmm,mod.atemporal,mod.ano,rank=AICc);mod.test
av1=model.avg (get.models (mod.test, subset = delta < 2,beta ="none",rank=AIC,fit=T))
coefTable(av1)
varImp(mod.full, scale = FALSE)


#multimodel
dados2=dados1[,-dados1$ano]
mod=glmulti(ninho~.,data=dados2,family="binomial",crit=aicc,method="h", confsetsize=256,level=1)
coeficientes=coef.glmulti(mod);coeficientes
summary.glmulti(mod)
weightable(mod)
plot(mod,type="s")

"Burning"= 0.2207641
"Vegetation density"= 0.3514052
"Height (higher stratum)"= 0.2326585
"Bare ground (%)" = 0.9952342
"Habitat type" = 0.2215828
"Slope" = 0.2968510
"Height (lower stratum)" = 0.9796466
"Shrub cover (%)" = 0.9998258 
 
teste=dados=read.table(pipe("pbpaste"), sep="\t", header=T);teste
par(mar=c(5,7,1,1))
barplot(teste$RI, main="", horiz=TRUE,
        names.arg=teste$var,xlab="Relative Varibale Importance",col="black",las=1)


