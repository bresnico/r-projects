#lancement des packages
library(readxl)
library(tidyverse)

#importation des données brutes présentées sous la forme d'un tableau unique de un temps.

d <- read_excel("hepvs2020_ac_raw.xlsx")

#Création de la moyenne "sco" pour chaque participant (On ne s'occupe pas de la variable "total")

d <- d %>% 
  mutate(sco = rowMeans(d[,7:31],na.rm =T))

#Procédons à la préparation d'un petit résumé de cet échantillon (par classe ou/et genre)

d_sum <- d %>% 
  mutate(sex=ifelse(sex=="M", "garçons","filles")) %>% 
  group_by(clas, sex) %>% 
  summarise(n=n(),
            av_age=round(mean(age), digits=2))

d_sum2 <- d %>% 
  group_by(clas) %>% 
  summarise(n=n(),
            mean_sco=mean(sco))

###############
#visualisation#
###############

#score global par classes
vis_sco <- d %>% 
  ggplot() +
  aes(x = clas, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#score global par groupe
vis_sco2 <- d %>% 
  ggplot() +
  aes(x = grp, y = sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen aux 25 exercices", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))
