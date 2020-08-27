#lancement des packages
library(readxl)
library(tidyverse)

#importation des données brutes présentées sous la forme d'un tableau unique de trois temps (mais en fait deux temps de mesure pour les id commençant par fc et qui nous intéressent)
#après préparation ad hoc dans Qualtrics

d <- read_excel("ors2020_ad_raw.xlsx")
str(d) #on observe que la variable "RecordedDate" semble avoir été bien intégrée (POSIXct)

#######################
#travail sur les dates#
#######################
#modification de la variable "RecordedDate" en "date" en temps 1 et temps 2 tout en filtrant les id enregistrés hors temps 1 et temps 2.
#mais on commence par la renommer.

d <- d %>% 
  rename(date = RecordedDate) %>%  #dans cet ordre.
  mutate(id = tolower(id)) #gestion de la casse.

d <- d %>% 
  mutate(
    date = case_when(date >= as.POSIXct("01.08.2019", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("31.08.2019", format="%d.%m.%Y", tz="utc") ~ "temps 1",
                     date >= as.POSIXct("01.06.2020", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("30.06.2020", format="%d.%m.%Y", tz="utc") ~ "temps 2",
                     TRUE ~ "autre temps")) #on privilégie case_when car on a 3 conditions et on va gérer les dates.

#Au passage, R a modifié le type de variable "date". On le laisser respirer... et on filtre... (si j'intègre filter dans le pipe, ça bug...)

d <- d %>% filter(date =="temps 1" | date == "temps 2")

#Recodage des variables au score inversé

d <- d %>% 
  mutate(hbs20_7 = 6 - hbs20_7,
         be8_4 = 8 - be8_4,
         be8_5 = 8 - be8_5,
         be8_8 = 8 - be8_8,
         sem17_5 = 6 - sem17_5,
         sem17_10 = 6 - sem17_10,
         kid17_6 = 6 - kid17_6,
         kid17_15 = 6 - kid17_15,
         kid17_16 = 6 - kid17_16,
         kid17_17 = 6 - kid17_17,)

#Création des moyennes de chaque questionnaire pour chaque observation (sauf les 3 shorts questions et le score du nombre d'amis qui est "ami" - "ennemis")

d <- d %>% 
  mutate(sem_sco = rowMeans(select(.,starts_with("sem")) ,na.rm =T),
         hbs_sco = rowMeans(select(.,starts_with("hbs")),na.rm =T),
         be_sco  = rowMeans(select(.,starts_with("be")) ,na.rm =T),
         kid_sco = rowMeans(select(.,starts_with("kid")),na.rm =T),
         ami_sco = ami - nam,
         sho1_sco = sho_1_1,
         sho2_sco = sho_2_1,
         sho3_sco = sho_3_1)

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

#procédons à la préparation d'un petit résumé de cet échantillon.

d_paired_sum <- d_paired %>% 
  mutate(sex=ifelse(sex=="1", "garçons","filles")) %>% 
  group_by(date, sex) %>% 
  summarise(n=n(),
            av_age=round(mean(age), digits=2))

d_paired_sum2 <- d_paired %>% 
  group_by(date) %>% 
  summarise(n=n(),
            mean_hbs=mean(hbs_sco),
            mean_sem=mean(sem_sco),
            mean_be=mean(be_sco),
            mean_kid=mean(kid_sco),
            mean_sho1=mean(sho1_sco),
            mean_sho2=mean(sho2_sco),
            mean_sho3=mean(sho3_sco),
            mean_ami=mean(ami_sco))

###############
#visualisation#
###############

#SEM
vis_sem <- d_paired %>% 
  ggplot() +
  aes(x = date, y = sem_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure de l'engagement", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_hbs <- d_paired %>% 
  ggplot() +
  aes(x = date, y = hbs_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure des CPS", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_be <- d_paired %>% 
  ggplot() +
  aes(x = date, y = be_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure du Bien-être", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_kid <- d_paired %>% 
  ggplot() +
  aes(x = date, y = kid_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure de bien-être du Kidscreen", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_ami <- d_paired %>% 
  ggplot() +
  aes(x = date, y = ami_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Score général ami-ennemi", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_sho1 <- d_paired %>% 
  ggplot() +
  aes(x = date, y = sho1_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure de fatigue", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_sho2 <- d_paired %>% 
  ggplot() +
  aes(x = date, y = sho2_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure d'amitié", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_sho3 <- d_paired %>% 
  ggplot() +
  aes(x = date, y = sho3_sco) +
  geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
  labs(title = "Mesure d'humeur", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

###########################################
#Différence temps 1 et temps 2 pour le SEM#
#    ETUDE DE CAS AVEC UN LONG FORMAT     #
###########################################

#Ordonnons par "temps" puis "id" sinon le test t gère repère pas les bonnes paires. On doit pas avoir de bug dans les lignes.

#Je fais un peu du code gras mais c'est pour bien comprendre.

d_sem <- d_paired %>% 
  arrange(date, id)

#ON RENONCE A LA VERIFICATION DES CONDITIONS D'APPLICATION. ON VOIT DEJA A L'OEIL QUE LA NORMALITE EST PAS TOP...MAIS BON.
#On devrait vérifier :
#outliers ?
#normalité de la différence des moyennes et QQ plot MAIS comment la calculer en format long ??


#Paired t-test
t.test(sem_sco ~ date, d_sem, paired=TRUE) #on conclut que la diff est stat. sign.

#effect size: ON Y RENONCE
