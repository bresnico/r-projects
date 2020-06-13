library(tidyverse)
library(readxl)

# Importation des données Qualtrics à disposition ----
d_t1_raw <- read_excel("crips2019_lv_raw_t1.xlsx")
d_t2_raw <- read_excel("crips2019_lv_raw_t2.xlsx")

#Ajout de la variable temps sur chaque df et normalisation des id en minuscule.

d_t1 <- d_t1_raw %>% 
  mutate(temps="temps 1",
         id=tolower(id)) 

d_t2 <- d_t2_raw %>% 
  mutate(temps="temps 2",
         id=tolower(id))

#Quelques données sur l'état des participants au Temps 1 et au Temps 2 avant suppression des ID non-membres d'une paire entre T1 et T2.
d_t1_sum <- d_t1 %>% 
  group_by(group,sex) %>% 
  summarise(n=n(),
            age_moy=round(mean(age),
                          digits=1)) 

d_t2_sum <- d_t2 %>% 
  group_by(group,sex) %>% 
  summarise(n=n(),
            age_moy=round(mean(age),
                          digits=1)) 

#Création des variables de score----
#Mise en formation long sur un df

d_long <- bind_rows(d_t1,d_t2)

#Recondage des variables au score inversé

d_long <- d_long %>% 
  mutate(hbs20__7 = 6 - hbs20__7,
         pec5__5 = 6 - pec5__5,
         be8__4 = 8 - be8__4,
         be8__5 = 8 - be8__5,
         be8__8 = 8 - be8__8,
         est10__3 = 5 - est10__3,
         est10__5 = 5 - est10__5,
         est10__7 = 5 - est10__7,
         est10__10 = 5 - est10__10,
         sou13__6 = 6 - sou13__6,
         sou13__9 = 6 - sou13__9,
         mot16__4 = 8 - mot16__4,
         mot16__8 = 8 - mot16__8,
         mot16__12 = 8 - mot16__12,
         mot16__15 = 8 - mot16__15,
         mot16__16 = 8 - mot16__16)

#Création des moyennes de chaque questionnaire pour chaque observation

d_long <- d_long %>% 
  mutate(hbs_sco = rowMeans(select(.,starts_with("hbs")),na.rm =T),
         pec_sco = rowMeans(select(.,starts_with("pec")),na.rm =T),
         be_sco  = rowMeans(select(.,starts_with("be")) ,na.rm =T),
         est_sco = rowMeans(select(.,starts_with("est")),na.rm =T),
         cli_sco = rowMeans(select(.,starts_with("cli")),na.rm =T),
         sou_sco = rowMeans(select(.,starts_with("sou")),na.rm =T),
         mot_sco = rowMeans(select(.,starts_with("mot")),na.rm =T))


#suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d_long %>% 
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, temps) %>% 
  count(id) %>% 
  filter(n==1) %>% #On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>% 
  group_by(id) %>% 
  count(id) %>% 
  filter(n==2) %>% #on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

#Notre df de comparaison est prêt. On peut procéder à l'élagage de d_long.

d_long_paired <- d_long %>% 
  filter(id %in% d_comp$id) %>% 
  mutate(group=ifelse(group=="con", "contrôle","expérimental"))

#procédons à la préparation d'un petit résumé de cet échantillon.

d_long_paired_sum <- d_long_paired %>% 
   mutate(sex=ifelse(sex=="1", "garçons","filles")) %>% 
   group_by(temps, group, sex) %>% 
  summarise(n=n())

d_long_paired_sum2 <- d_long_paired %>% 
  group_by(temps, group) %>% 
  summarise(n=n(),
            mean_hbs=mean(hbs_sco),
            mean_pec=mean(pec_sco),
            mean_be=mean(be_sco),
            mean_est=mean(est_sco),
            mean_cli=mean(cli_sco),
            mean_sou=mean(sou_sco),
            mean_mot=mean(mot_sco),
            mean_sho1=mean(sho_1),
            mean_sho2=mean(sho_2),
            mean_sho3=mean(sho_3),
            mean_sho4=mean(sho_4))

#Repérage des données manquantes----
#ras.


#Repérage des données extrêmes----
#On verra plus tard.

###############
#visualisation#
###############

#HBSC

# comme ZOE mais inutle GRACE A stat_summary MAIS a voir les barres derreur quand je voudrai faire avec la fonction dédiée.
#vis_hbs <- d_long_paired %>% 
#  group_by(temps, group) %>% 
#  summarise(moy=mean(hbs_sco, na.rm=T),
#            sd=sd(hbs_sco, na.rm=T),
#            sd_low=moy-sd,
#            sd_up=moy+sd) %>% 
#  mutate(moy=round(moy, digits = 2))


#préparation du plot pour le HBSC

# 1- essai à partir de l'aide de Zoé
#essai <- ggplot(d_long_paired) +
#  geom_jitter(aes(x = temps, y = hbs_sco, color = group), size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.5, jitter.width = .2)) +
#  geom_errorbar(data = vis_hbs, aes(x=temps, ymin=sd_low, ymax=sd_up), width = .1) +
#  geom_line(data = vis_hbs, aes(x=temps, y=moy, group = 1)) +
#  geom_point(data=vis_hbs, aes(x=temps, y=moy, fill=temps), size = 5, shape = 24, color = "black") +
#  geom_label(data=vis_hbs, aes(x=temps, y=moy, label=moy, fill=temps), nudge_x = .3, nudge_y = -.02, size = 8, color = "white")+
#  guides(fill=FALSE)

# 2.1- essai à partir de https://sebastiansauer.github.io/vis_interaction_effects/

#essai2 <- d_long_paired %>% 
#  group_by(temps, group) %>% 
#  summarise(hbs_sco_essai2 = mean(hbs_sco))
#plot2 <- essai2 %>% 
#  ggplot() +
#  aes(x = temps, y = hbs_sco_essai2, color = group) +
#  geom_line(aes(group = group)) +
#  geom_point()

#3- Essais avec un autre angle d'attaque.
#vis_hbs_boxplot <- ggplot(d_long_paired) +
#  geom_jitter(aes(x = temps, y = hbs_sco, color = group), size = 3, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
#  geom_line(data=essai2, aes(group = group, x = temps, y = hbs_sco_essai2, color = group), position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
#  geom_point(data=essai2, aes(x = temps, y = hbs_sco_essai2, color = group), position = position_jitterdodge(dodge.width=.7, jitter.width = .2), shape = 24)
  
#2.2- CHOIX FINAL OK :-) à partir de https://sebastiansauer.github.io/vis_interaction_effects/ avec stat_summary et fun

vis_hbs <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = hbs_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure des CPS", y = "Score au HBSC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("Groupe", palette = "Set1")

vis_pec <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = pec_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure de régulation émotionnelle", y = "Score au PEC (5 items") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_be <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = be_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure du bien-être scolaire", y = "Score aux 8 items du bien-être scolaire") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_est <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = est_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure de motivation scolaire", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_cli <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = cli_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure de climat scolaire", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_sou <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = sou_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure du sentiment de soutien de l'enseignant·e", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_mot <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, y = mot_sco) +
  geom_boxplot(alpha = .5) +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, aes(group = group), geom = "line") +
  labs(title = "Mesure de motivation", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Set1")

vis_sho_1 <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, group = group, y = sho_1) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, geom = "line") +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.8, jitter.width = .4)) +
  labs(title = "Mesure du sentiment d'amitié", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer("Groupe", palette = "Dark2")

vis_sho_2 <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, group = group, y = sho_2) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, geom = "line") +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.8, jitter.width = .4)) +
  labs(title = "Mesure d'humeur", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Dark2")

vis_sho_3 <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, group = group, y = sho_3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, geom = "line") +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.8, jitter.width = .4)) +
  labs(title = "Mesure d'intérêt", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Dark2")

vis_sho_4 <- d_long_paired %>% 
  ggplot() +
  aes(x = temps, color = group, group = group, y = sho_4) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
  stat_summary(fun = mean, geom = "line") +
  geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.8, jitter.width = .4)) +
  labs(title = "Mesure d'énergie", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_color_brewer("Groupe", palette = "Dark2")

