# lancement des packages
library(readxl)
library(tidyverse)
library(rio)

# Importation des données brutes présentées sous la forme d'un tableau unique d'un temps.
d <- read_excel("dupp2020_ms_raw.xlsx")
str(d) #on observe que la variable "RecordedDate" semble avoir été bien intégrée (POSIXct)

d <- d %>% 
  rename(date = RecordedDate) %>%  #dans cet ordre.
  mutate(id = tolower(id)) #gestion de la casse.

# Recodage des variables au score inversé

d <- d %>% 
  mutate(cps.26_3 = 6 - cps.26_3,
         cps.26_25 = 6 - cps.26_25,
         be.8_4 = 8 - be.8_4,
         be.8_5 = 8 - be.8_5,
         be.8_8 = 8 - be.8_8,
         con.10_4 = 10 - con.10_4,
         con.10_5 = 10 - con.10_5,
         con.10_6 = 10 - con.10_6,
         con.10_7 = 10 - con.10_7,
         con.10_9 = 10 - con.10_9,
         cli.53_2 = 5 - cli.53_2,
         cli.53_3 = 5 - cli.53_3,
         cli.53_4 = 5 - cli.53_4,
         cli.53_5 = 5 - cli.53_5,
         cli.53_7 = 5 - cli.53_7,
         cli.53_8 = 5 - cli.53_8,
         cli.53_9 = 5 - cli.53_9,
         cli.53_10 = 5 - cli.53_10,
         cli.53_12 = 5 - cli.53_12,
         cli.53_13 = 5 - cli.53_13,
         cli.53_14 = 5 - cli.53_14,
         cli.53_16 = 5 - cli.53_16,
         cli.53_17 = 5 - cli.53_17,
         cli.53_19 = 5 - cli.53_19,
         cli.53_20 = 5 - cli.53_20,
  )
         
# Création des moyennes de chaque questionnaire pour chaque observation

d <- d %>% 
  mutate(cps_sco = rowMeans(select(.,starts_with("cps")) ,na.rm =T),
         con_sco = rowMeans(select(.,starts_with("con")),na.rm =T),
         be_sco  = rowMeans(select(.,starts_with("be")) ,na.rm =T),
         cli_sco = rowMeans(select(.,starts_with("cli")),na.rm =T),
         har_1_sco = rowMeans(select(.,starts_with("har_1")),na.rm = T),
         har_2_sco = rowMeans(select(.,starts_with("har_2")),na.rm = T)
         )

d$clas.fac <- as.factor(d$clas)

##########
# Résumé #
##########

summary <- d %>% 
  group_by(clas) %>% 
  summarize(n=n(),
            cps=mean(cps_sco),
            confiance=mean(con_sco),
            be=mean(be_sco),
            climat=mean(cli_sco),
            question_1=mean(har_1_sco),
            question_2=mean(har_2_sco),
  )

#################
# visualisation #
#################


#score global par classe
vis_cps <- d %>% 
  ggplot() +
  aes(x = clas, y = cps_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - CPS", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_con <- d %>% 
  ggplot() +
  aes(x = clas, y = con_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - confiance", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))


vis_be <- d %>% 
  ggplot() +
  aes(x = clas, y = be_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - bien-être", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_cli <- d %>% 
  ggplot() +
  aes(x = clas, y = cli_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - climat", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_har_1 <- d %>% 
  ggplot() +
  aes(x = clas, y = har_1_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - question 1", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

vis_har_2 <- d %>% 
  ggplot() +
  aes(x = clas, y = har_2_sco, color = clas.fac) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + #Le group = 1 est nécessaire pour dire à la ligne de connecter tous les points.
  labs(title = "Score moyen - question 2", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

#################
# df pour marie #
#################

d_ms <- d %>% 
  select(., date, id, clas, sex, cps_sco, con_sco, be_sco, cli_sco, har_1_sco, har_2_sco)
export(d_ms, "donnees_marie.xlsx")
