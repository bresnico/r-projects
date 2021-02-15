# Intervention sur le livret des Forces, mars 2019 (uniquement t1) 

setwd("~/HEP/Recherche/Bressoud_VIA_Kidscreen/Orsiere")

#ls()
#rm(list = ls())


## library

#library(Rcmdr)
#library(psy)
#library(MASS)
library(psych)

#source(file="C:/Work/Lib/RFct/sumScoreDf.fct.R")
#source(file="C:/Work/Lib/RFct/convScoreDf.fct.R")
#source(file="C:/Work/Lib/RFct/alphaDf.fct.R")
source(file="C:/Work/Lib/RFct/corTestDf.fct.R")
source(file="C:/Work/Lib/RFct/formCor.fct.R")

#load(file="C:/Work/Lib/RShare/sepOrsParam.df")
#load(file="C:/Work/Lib/RShare/beOrsParam.df")
#load(file="C:/Work/Lib/RShare/engOrsParam.df")


#### load all ans sum #### 
read.csv2(file="NBR_Ors_t1.csv", row.names=1, dec=".") -> all.df
names(all.df)
head(all.df)
summary(all.df)


summary(all.df$groupe)	
all.df$groupe <- as.factor(all.df$groupe)
summary(all.df$groupe)	
summary(all.df$classe.eleve)


read.csv2(file="NBR_Ors_t4.csv", row.names=1, dec=".") -> VD_t4.df
names(VD_t4.df)



### code load t1 ### 

# load Humeur et Vitalité t1
cbind(all.df[2], all.df[33], all.df[59], all.df[77:80]) -> VD_t1.df
names(VD_t1.df)
summary(VD_t1.df)
describeBy(VD_t1.df, group = VD_t1.df$groupe)

read.csv2(file="NBR_Ors_t4.csv", row.names=1, dec=".") -> VD_3t.df
 VD_3t.df <- VD_3t.df[36:38]
 



#### corrélations
corTestDf.fct(VD_t1.df[-1], VD_t1.df[-1]) -> tmp.li
formCor.fct(tmp.li)
plot(VD_t1.df[-1], VD_t1.df[-1])




#### régressions

fit <- lm(t0.bienetre.score.moyenne ~ t0.eng.score.moyenne + t0.sep.score.moyenne, data=VD_t1.df)
summary(fit)
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

## compare models
#fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
#fit2 <- lm(y ~ x1 + x2)
#anova(fit1, fit2) 
#https://www.statmethods.net/stats/regression.html


fit <- lm(t0.bienetre.score.moyenne ~ t0.eng.score.moyenne * t0.sep.score.moyenne, data=VD_t1.df)
summary(fit)


fit <- lm(t0.eng.score.moyenne ~ t0.bienetre.score.moyenne + t0.sep.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.eng.score.moyenne ~ t0.bienetre.score.moyenne * t0.sep.score.moyenne, data=VD_t1.df)
summary(fit)


fit <- lm(t0.sep.score.moyenne ~ t0.bienetre.score.moyenne + t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.sep.score.moyenne ~ t0.bienetre.score.moyenne * t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)


fit <- lm(t0.sep.score.général ~ t0.bienetre.score.moyenne + t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.sep.score.général ~ t0.bienetre.score.moyenne * t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)

fit <- lm(t0.sep.score.math ~ t0.bienetre.score.moyenne + t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.sep.score.math ~ t0.bienetre.score.moyenne * t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)

fit <- lm(t0.sep.score.francais ~ t0.bienetre.score.moyenne + t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.sep.score.francais ~ t0.bienetre.score.moyenne * t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)




fit <- lm(t0.bienetre.score.moyenne ~ t0.sep.score.math +  t0.sep.score.francais + t0.sep.score.général, data=VD_t1.df)
summary(fit)
fit <- lm(t0.eng.score.moyenne ~ t0.sep.score.math +  t0.sep.score.francais + t0.sep.score.général, data=VD_t1.df)
summary(fit)
fit <- lm(t0.bienetre.score.moyenne ~ t0.sep.score.math +  t0.sep.score.francais + t0.sep.score.général + t0.eng.score.moyenne, data=VD_t1.df)
summary(fit)
fit <- lm(t0.eng.score.moyenne ~ t0.sep.score.math +  t0.sep.score.francais + t0.sep.score.général + t0.bienetre.score.moyenne, data=VD_t1.df)
summary(fit)

## mediation LAVAAN

library(QuantPsyc)
library(lavaan)
library(semPlot)

attach(VD_t1.df)

# matrice de corr
table.cor = lav_matrix_lower2full(c(1.00,
                                    0.30, 1.00,
                                    0.52, 0.59, 1.00))

colnames(table.cor) = rownames(table.cor) = c("BE", "ENG", "SEP")


# ENG -> SEP -> BE
mediation.model = '
BE ~ a*ENG + c*SEP
SEP ~ b*ENG
ind:=b*c
'
mediation.fit = sem(mediation.model, 
                    sample.cov=table.cor,
                    sample.nobs=118)
summary(mediation.fit, rsquare=T, standardized=T)
semPaths(mediation.fit, whatLabels = "par", layout = "spring")



# SEP -> BE -> ENG
mediation.model = '
ENG ~ a*BE + c*SEP
BE ~ b*SEP
ind:=b*c
'
mediation.fit = sem(mediation.model, 
                    sample.cov=table.cor,
                    sample.nobs=118)
summary(mediation.fit, rsquare=T, standardized=T)
semPaths(mediation.fit, whatLabels = "par", layout = "spring")



# BE -> SEP -> ENG
mediation.model = '
ENG ~ a*SEP + c*BE
SEP ~ b*BE
ind:=b*c
'
mediation.fit = sem(mediation.model, 
                    sample.cov=table.cor,
                    sample.nobs=118)
summary(mediation.fit, rsquare=T, standardized=T)
semPaths(mediation.fit, whatLabels = "par", layout = "spring")


# ENG -> BE -> SEP
mediation.model = '
SEP ~ a*BE + c*ENG
BE ~ b*ENG
ind:=b*c
'
mediation.fit = sem(mediation.model, 
                    sample.cov=table.cor,
                    sample.nobs=118)
summary(mediation.fit, rsquare=T, standardized=T)
semPaths(mediation.fit, whatLabels = "par", layout = "spring")


# BE -> ENG -> SEP
mediation.model = '
SEP ~ a*ENG + c*BE
ENG ~ b*BE
ind:=b*c
'
mediation.fit = sem(mediation.model, 
                    sample.cov=table.cor,
                    sample.nobs=118)
summary(mediation.fit, rsquare=T, standardized=T)
semPaths(mediation.fit, whatLabels = "par", layout = "spring")









names(VD_t1.df)
# ANOVA
fit <- aov(t0.bienetre.score.moyenne ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)

fit <- aov(t0.eng.score.moyenne ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)
TukeyHSD(fit) 

fit <- aov(t0.sep.score.moyenne ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)

fit <- aov(t0.sep.score.général ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)

fit <- aov(t0.sep.score.francais ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)

fit <- aov(t0.sep.score.math ~ groupe, data=VD_t1.df)
summary(fit) # display Type I ANOVA table
plot(fit)










# comparaisons entre t0 et t1

read.csv2(file="wb_pga.csv", row.names=1, dec=".") -> wb_pga.df
str(wb_pga.df)

wb_pga.df$category <- as.factor(wb_pga.df$category)
#wb_pga.df$WB_score_mean <- as.numeric_version(wb_pga.df$WB_score_mean)


summary(wb_pga.df)

fit <- aov(WB_score_mean ~ category*tps, data=wb_pga.df) 
summary(fit)

model1 <- aov(WB_score_mean ~ (category*tps) + Error(Subj/(category*tps)), contrasts = contr.sum, data=wb_pga.df)
summary(model1)   # Standard repeated measures anova  

model1 <- aov(WB_score_mean ~ (category*tps) + Error(Subj), contrasts = contr.sum, data=wb_pga.df)
summary(model1)   # Standard repeated measures anova  

model1 <- aov(WB_score_mean ~ (category+tps+category*tps) + Error(Subj/(category*tps)), contrasts = contr.sum, data=wb_pga.df)
summary(model1)   # Standard repeated measures anova  
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(model1) # diagnostic plots
drop1(model1,~.,test="F") # type III SS and F Tests 
TukeyHSD(model1)
TukeyHSD(fit) 

library(ggplot2)

ggplot(wb_pga.df, aes(x=category, y=WB_score_mean, colour=tps, fill=tps))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) +
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+
  ylab("WB")+
  theme_classic()



t.test(VD_3t.df[1], VD_3t.df[2])
t.test(VD_3t.df[1], VD_3t.df[3])
t.test(VD_3t.df[2], VD_3t.df[3])


boxplot(VD_3t.df[1], VD_3t.df[2], VD_3t.df[3])


