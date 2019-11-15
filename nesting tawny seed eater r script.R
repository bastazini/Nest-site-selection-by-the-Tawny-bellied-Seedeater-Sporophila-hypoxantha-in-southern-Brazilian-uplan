require(caret)
#pacotes
require(MuMIn)
require(lme4)
require(bbmle)
require(glmulti)
require(vegan)

#dados
dados=read.table(pipe("pbpaste"), sep="\t", header=T);dados
attach(dados)
#Dados padronizados
pred=(decostand(tabela[,c(-1,-2)],"standardize"));pred
ano1=as.factor (tabela[,2]);ano1
dados=cbind(densidade,pred)
dados

#testando maodelos completos(misto com ano,sem ano, linear com ano)
mod.full=lmer(densidade~precipitacao+temperatura+vento+umidade+(1|ano1),data=tabela)
mod.lm=lm(densidade~precipitacao+temperatura+vento,data=tabela)
mod.ano=lm(densidade~precipitacao+temperatura+ano1,data=tabela)

AICctab(mod.full,mod.lm,mod.ano,nobs=14,weights = TRUE, delta = TRUE, base = TRUE)
mod.test<-model.sel (mod.full,mod.lm,mod.ano,rank=AIC);mod.test
av1<-model.avg (get.models (mod.test, subset = delta < 2,beta ="none",rank=AIC,fit=T))
coefTable(av1)
varImp(mod.full, scale = FALSE)

#
#data dredging


mod=glmulti(densidade~.,data=dados,family="gaussian",crit=aicc,method="h", level=1)
coeficientes=coef.glmulti(mod);coeficientes
summary.glmulti(mod)
weightable(mod)
plot(mod,type="s")

