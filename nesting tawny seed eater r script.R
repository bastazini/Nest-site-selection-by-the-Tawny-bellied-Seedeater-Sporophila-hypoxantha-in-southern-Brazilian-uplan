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
pred2=as.factor (cbind(ano,fogo,amb))
dados1=cbind(ninho,pred,pred2);dados1
dados1=as.data.frame(dados1)
#testando maodelos completos(misto com ano,sem ano, linear com ano)
mod.full=glmer(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv+(1|ano),family="binomial", data=dados1)
mod.glm=glm(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv,family="binomial", data=dados1)
mod.ano=glm(ninho~cob.solo+cob.estrat.med+alt.estrat.med+alt.estrat.alt+fogo+cob.lat+decliv+ano,family="binomial", data=dados1)

AICctab(mod.full,mod.glm,mod.ano,nobs=52,weights = TRUE, delta = TRUE, base = TRUE)
mod.test<-model.sel (mod.full,mod.lm,mod.ano,rank=AIC);mod.test
av1=model.avg (get.models (mod.test, subset = delta < 2,beta ="none",rank=AIC,fit=T))
coefTable(av1)
varImp(mod.full, scale = FALSE)

#
#data dredging


mod=glmulti(ninho~.,data=dados1,family="binomial",crit=aicc,method="h", level=1)
coeficientes=coef.glmulti(mod);coeficientes
summary.glmulti(mod)
weightable(mod)
plot(mod,type="s")

