# lancement des packages
library(readxl)
library(tidyverse)
library(broom) #pour les sorties du test ANOVA en tidy data.
library(knitr) #pour bidouiller nos tableaux de sortie
library(kableExtra) #pour bidouiller nos tableaux de sortie
library(sjstats) #pour eta_sq()

# importation des données brutes présentées sous la forme d'un tableau de deux temps avec un groupe expé et un groupe contrôle.
# les données concernent une expérimentation pédagogique en classe sur le groupe expé. On mesure l'inhibition des élèves avant/après.

d <- read_excel("hepvs2020_ac_raw.xlsx")

# Création de la moyenne "sco" pour chaque participant (On ne s'occupe pas de la variable "total")

d <- d %>% 
  mutate(sco = rowMeans(d[,7:31],na.rm =T))

# Préparation de la variable de temps en facteur

d$tps <- factor(d$tps)
d$grp <- factor(d$grp)

##################################
# Résumés avant appairage strict #
##################################

# N, âge, score : avant / après par classe et sexe
d_sum <- d %>% 
  mutate(sex=ifelse(sex=="M", "garçons","filles")) %>% 
  group_by(clas, sex, tps) %>% 
  summarise(n=n(),
            av_age=round(mean(age), digits=2),
            sco_mean=mean(sco)
            )

# N, score : avant / après par classe
d_sum2 <- d %>% 
  group_by(clas, tps) %>% 
  summarise(n=n(),
            mean_sco=mean(sco))

# N, score : avant / après par groupe
d_sum3 <- d %>% 
  group_by(tps, grp) %>% 
  summarise(n=n(),
            mean_sco=mean(sco))

######################################
# Appairage strict temps 1 - temps 2 #
######################################

#suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d %>% 
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, tps) %>% 
  count(id) %>% 
  filter(n==1) %>% #On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>% 
  group_by(id) %>% 
  count(id) %>% 
  filter(n==2) %>% #on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

#Notre df de comparaison est prêt. On peut procéder à l'élagage de d.

d_paired <- d %>% 
  filter(id %in% d_comp$id) %>% 
  mutate(grp = recode(grp, "Ex" = "Expérimental", "Co" = "Contrôle" ))

##################################
# Résumés après appairage strict #
##################################


# N, âge, sex score : avant / après par classe et sexe
d_sump <- d_paired %>% 
  mutate(sex=ifelse(sex=="M", "garçons","filles")) %>% 
  group_by(grp, clas, sex) %>% 
  summarise(n=n(),
            mean_age= mean(age)
  )

# N, score : avant / après par classe
d_sump2 <- d_paired %>% 
  group_by(clas, tps) %>% 
  summarise(n=n(),
            mean_sco=mean(sco))

# N, score : avant / après par groupe
d_sump3 <- d_paired %>% 
  group_by(tps, grp) %>% 
  summarise(n=n(),
            mean_sco=mean(sco),
            sd_sco=sd(sco),
            max=max(sco),
            min=min(sco)) %>% 
  ungroup()

# N, score : avant / après par classe
d_sump4 <- d_paired %>% 
  group_by(tps, clas) %>% 
  summarise(n=n(),
            mean_sco=mean(sco),
            sd_sco=sd(sco),
            max=max(sco),
            min=min(sco)) %>% 
  ungroup()


######################################
#visualisation des données appairées #
######################################

#score global par classes
vis_sco <- d_paired %>% 
  ggplot() +
  aes(x = clas, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#score global par groupe
vis_sco2 <- d_paired %>% 
  ggplot() +
  aes(x = grp, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#score global par groupe
vis_sco3 <- d_paired %>% 
  ggplot() +
  aes(x = tps, color = grp, y = sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = grp), geom = "line") +
  labs(title = "Mesure d'inhibition", y = "Score au test") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

# score par classe au temps 2 et au temps 1

d_paired_2 <- d_paired %>% 
  filter(tps=="2")

d_paired_1 <- d_paired %>% 
  filter(tps=="1")

vis_sco4 <- d_paired_2 %>% 
  ggplot() +
  aes(x = clas, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices - temps 2", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_sco5 <- d_paired_1 %>% 
  ggplot() +
  aes(x = clas, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices - temps 1", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))


#######################
# Stat inférentielles #
#######################

# On a une variable intra (temps) et une variable inter (groupe)
# Notre H c'est que le groupe expé progresse significativement en comparaison au GC.

# Au préalable: conditions d'applications à vérifier.
# A FAIRE

# Formule possible : aov(dependent_variable ~ independent variable, data = data_df)
# On cherche donc une interaction tps*grp à p<.05.

stats <- aov(sco ~ tps*grp, data = d_paired)
eta_sq(stats)
summary(stats)
tidy_stats <- tidy(stats)