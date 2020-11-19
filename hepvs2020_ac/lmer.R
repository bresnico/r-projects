library(lme4)
library(lmerTest)
library(ggplot2)
library(reshape2)
library(languageR)
library(psych)
library(effects)
#library(mclogit)
names(d)
d[, 1:31]
d_long = melt(d[, 1:31], id.vars=c("id","age", "sex", "grp", "clas", "tps"))

names(d_long)[7] = "item"
names(d_long)[8] = "score"



#codage des contrastes 
options(contrasts=c("contr.sum", "contr.poly")) #-1/1 
options(contrasts=c("contr.treatment", "contr.poly")) #0/1
# pour specifier qu'un seul facteur est 1/-1: 
#contrasts(d_long$tps)<-contr.sum(levels(d_long$tps))
# pour specifier un seul facteur 01
#contrasts(d_long$tps)<-contr.treatment(levels(d_long$tps))

#descriptives 

hist(d_long$score)
describe(d_long)

table(d_long$clas, d_long$grp)


#mettre les pr?dicteurs en facteurs 
d_long$grp<- as.factor(d_long$grp)
levels(d_long$grp)
d_long$tps<-as.factor(d_long$tps)

# 1. random intercepts (empty models)#####


ModelEmpty1a<- glmer(score ~ 1 + (1|id) + (1 |item) +(1|clas), data=d_long, family = "poisson")
ModelEmpty1b<- glmer(score ~ 1 + (1|id) + (1 |item) , data=d_long, family = "poisson")
ModelEmpty1c<- glmer(score ~ 1 + (1|id) +(1|clas), data=d_long, family = "poisson")
ModelEmpty1d<- glmer(score ~ 1 + (1 |item) +(1|clas), data=d_long, family = "poisson")

summary(ModelEmpty1a)
summary(ModelEmpty1b)
anova(ModelEmpty1a, ModelEmpty1b) # class est pertinent en random 
anova(ModelEmpty1a, ModelEmpty1c) # class et item sont pertinents en random 
anova(ModelEmpty1a, ModelEmpty1d) # class id et item sont pertinents en random 


# 2. intégrer les prédicteurs  ####
modele_dlong<- glmer(score ~ grp*tps + (1|id) + (1 |item) +(1|clas), data=d_long, family = "poisson")
summary(modele_dlong)
confint(modele_dlong, level = 0.95)

hist(resid(modele_dlong)) #pas forcement pertinent pour modèles poisson
plot(resid(modele_dlong), fitted(modele_dlong))

plot(Effect(c("grp", "tps" ), modele_dlong), 'tps', multiline=T, ylab="score", xlab="temps"
     , main="Interaction plot", trace.label="groupe" , colors=c("aquamarine4", "darkorchid4"))

# fit du modele 
library  (MuMIn)
r.squaredGLMM(modele_dlong)

     