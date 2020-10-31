# lancement des packages----
library(tidyverse)
library(readxl)

# Importation des données brutes (N = 162) sous la forme d'une répartition en 2 temps.

d <- read_excel("hepvs2020_lm_raw.xlsx") #les variables ont été correctement importées.

#########################
# travail sur les dates #
#########################

# modification de la variable "date" en temps 1, temps 2.

d <- d %>% 
  rename(id = code) %>%  #dans cet ordre.
  mutate(id = tolower(id)) #gestion de la casse.

d <- d %>% 
  mutate(
    date = case_when(date >= as.POSIXct("30.08.2020", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("11.09.2020", format="%d.%m.%Y", tz="utc") ~ "temps 1",
                     date >= as.POSIXct("11.10.2020", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("21.10.2020", format="%d.%m.%Y", tz="utc") ~ "temps 2",
                     TRUE ~ "autre temps"))

# Au passage, R a modifié le type de variable "date".

# une idée des données par date en créant l'objet d_date
d_date <- d %>% 
  group_by(date) %>% 
  summarize(n=n(),)

# Recodage des variables au score inversé sur variable bien-être.
d <- d %>% 
  mutate(be8_4 = 8 - be8_4,
         be8_5 = 8 - be8_5,
         be8_8 = 8 - be8_8,
         )

# Création des moyennes de chaque questionnaire (ou sous-dimension pour la PANAS) pour chaque observation :
d <- d %>% 
  mutate(be_sco = rowMeans(select(.,starts_with("be")) ,na.rm =T),
         pro_sco = rowMeans(select(.,starts_with("pro")),na.rm =T),
         panp_sco = rowMeans(select(.,starts_with("panp")),na.rm =T),
         pann_sco = rowMeans(select(.,starts_with("pann")),na.rm =T),
         )

##################
# Pairage strict #
##################

#suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d %>% 
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, date) %>% 
  count(id) %>% 
  filter(n==1) %>% #On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>% 
  group_by(id) %>% 
  count(id) %>% 
  filter(n==2) %>% #on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

#Notre df de comparaison est prêt. On peut procéder à l'élagage de d_long.

d_paired <- d %>% 
  filter(id %in% d_comp$id)

#######################
# Résumés des données #
#######################

############################### Summarise à boule !!

d_sum1 <- d_paired %>%
  group_by(condition, date) %>% 
  summarise(n(), mean(be_sco), mean(panp_sco), mean(pann_sco), mean(pro_sco))


  

#################
# visualisation #
#################

#be
vis_be <- d_paired %>% 
  ggplot() +
  aes(x = date, y = be_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure de bien-être", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#be2
vis_be2 <- d_paired %>% 
  ggplot() +
  aes(x = date, color = condition, y = be_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = condition), geom = "line") +
  labs(title = "Mesure bien-être", y = "Score de bien-être") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

#panp
vis_panp <- d_paired %>% 
  ggplot() +
  aes(x = date, color = condition, y = panp_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = condition), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure d'émotions positives", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#panp2
vis_panp2 <- d_paired %>% 
  ggplot() +
  aes(x = date, color = condition, y = panp_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = condition), geom = "line") +
  labs(title = "Mesure d'émotions positives", y = "Score PANAS+") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

#pann
vis_pann <- d_paired %>% 
  ggplot() +
  aes(x = date, y = pann_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure d'émotions négatives", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#pann2
vis_pann2 <- d_paired %>% 
  ggplot() +
  aes(x = date, color = condition, y = pann_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = condition), geom = "line") +
  labs(title = "Mesure émotions négatives", y = "PANAS-") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

#pro
vis_pro <- d_paired %>% 
  ggplot() +
  aes(x = date, y = pro_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure de climat de classe", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#pro2
vis_pro2 <- d_paired %>% 
  ggplot() +
  aes(x = date, color = condition, y = pro_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = condition), geom = "line") +
  labs(title = "Mesure climat", y = "Score de 13 items") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

# bidouillages, expérimentations

essai <- ggplot(data = d_paired,
       aes(x = condition, y = panp_sco,
           color = condition)) +
  geom_boxplot() +
  facet_grid(~date)

########################
# stats inférentielles #
########################

# Création du tibble de stat.

d_aov <- d_paired %>% 
  select(date, id, classe, condition, sex, age, be_sco, pro_sco, panp_sco, pann_sco ) %>% 
  mutate(classe = factor(classe, levels = c("B","C","D","E")),
         condition = factor(condition, levels = c("Expérimentale","Contrôle")),
         sex = factor(sex, levels = c("1","2")),
         date = factor(date, levels = c("temps 1","temps 2"))) %>% 
  arrange(date, id)

glimpse(d_aov) # sympa le glimpse. c'est quoi ?

# préparation anova DV = émotion positives IV = temps de mesure, condition. On cherche une interaction. sinon, pas intéressant, non ?

aov1 <- aov(panp_sco ~ date * condition * classe, data = d_aov)

glm1 <- glm(panp_sco ~ date * condition, data = d_aov)

summary(aov1)

