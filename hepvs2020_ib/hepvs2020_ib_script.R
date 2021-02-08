#lancement des packages
library(readxl)
library(tidyverse)

#modification des en-têtes du fichier excel à la main.
#importation des données brutes présentées sous la forme d'un tableau unique de 2 temps.

d <- read_excel("../../data/hepvs2020_ib_raw.xlsx")
str(d) #on observe que la variable "RecordedDate" semble avoir été bien intégrée (POSIXct)

#######################
#travail sur les dates#
#######################
#modification de la variable "RecordedDate" en "date" en temps 1 et temps 2 tout en filtrant les id enregistrés hors temps 1 et temps 2.
#mais on commence par la renommer.

d <- d %>% 
  mutate(id = tolower(id)) #gestion de la casse.

d <- d %>% 
  mutate(
    dat = case_when(dat >= as.POSIXct("01.02.2020", format="%d.%m.%Y", tz="utc") & dat <= as.POSIXct("01.03.2020", format="%d.%m.%Y", tz="utc") ~ "temps 1",
                     dat >= as.POSIXct("01.05.2020", format="%d.%m.%Y", tz="utc") & dat <= as.POSIXct("01.07.2020", format="%d.%m.%Y", tz="utc") ~ "temps 2",
                     TRUE ~ "autre temps")) #on privilégie case_when car on a 3 conditions et on va gérer les dates.

#Au passage, R a modifié le type de variable "date". On le laisser respirer... et on filtre... (si j'intègre filter dans le pipe, ça bug...)

d <- d %>% filter(dat =="temps 1" | dat == "temps 2")


#Création des moyennes de chaque questionnaire pour chaque observation (sauf les 3 shorts questions et le score du nombre d'amis qui est "ami" - "ennemis")

#suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d %>% 
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, dat) %>% 
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
  group_by(dat, sex, lan) %>% 
  summarise(n=n())

d_paired_sum2 <- d_paired %>% 
  group_by(dat) %>% 
  summarize(n=n(),
            mean_1=mean(a),
            mean_2=mean(b),
            mean_3=mean(c),
            mean_4=mean(d),
            mean_5=mean(e),
            mean_6=mean(f, na.rm=T),
            mean_7=mean(g),
            mean_8=mean(h),
            mean_9=mean(i),
            mean_10=mean(j),
            mean_11=mean(k),
            mean_12=mean(l),
            mean_13=mean(m, na.rm=T),
            mean_14=mean(n_),
            mean_15=mean(o),
            mean_16=mean(p))
            

###############
#visualisation#
###############


vis_a <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = a) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 1", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_b <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = b) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 2", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_c <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = c) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 3", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_d <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = d) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 4", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_e <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = e) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 5", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_f <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = f) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 6", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_g <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = g) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 7", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_h <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = h) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 8", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_i <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = i) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 9", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_j <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = j) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 10", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_k <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = k) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 11", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_l <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = l) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 12", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_m <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = m) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 13", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_n <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = n_) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 14", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_o <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = o) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 15", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_p <- d_paired %>% 
  ggplot() +
  aes(x = dat, y = p) +
  #geom_boxplot(alpha = .5, outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Mesure item 16", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#########################
#comparaison de moyennes#
#########################

#On doit arranger les données de d_paired par groupe de temps et par ID en ordre alphabétique pour que R compare les bons duos.

d_paired_arranger <- d_paired %>% 
  group_by(dat) %>% 
  arrange(id, .by_group = TRUE)

#test.t pour long format

stu_a <- t.test(a ~ dat, 
       data=d_paired, 
       paired=TRUE, 
       conf.level=0.95)

stu_b <- t.test(b ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_c <- t.test(c ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_d <- t.test(d ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_e <- t.test(e ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

# stu_f <- t.test(f ~ dat, #gestion des NA ???
#                data=d_paired, 
#                paired=TRUE, 
#                conf.level=0.95)

stu_g <- t.test(g ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_h <- t.test(h ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_i <- t.test(i ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_j <- t.test(j ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_k <- t.test(k ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_l <- t.test(l ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

# stu_m <- t.test(m ~ dat, #gestion des NA ???
#                data=d_paired, 
#                paired=TRUE, 
#                na.rm=TRUE,
#                conf.level=0.95)

stu_n <- t.test(n_ ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

stu_o <- t.test(o ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)


stu_p <- t.test(p ~ dat, 
                data=d_paired, 
                paired=TRUE, 
                conf.level=0.95)

