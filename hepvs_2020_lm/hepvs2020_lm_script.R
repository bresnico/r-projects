# lancement des packages----
library(tidyverse)
library(readxl)

# Importation des données brutes (N= ??? ) sous la forme d'une répartition en 2 temps.

d <- read_excel("hepvs2020_lm_raw.xlsx") #les variables ont été correctement importées.

#######################
#travail sur les dates#
#######################

# modification de la variable date" en temps 1, temps 2.

##############################
d <- d %>% 
  rename(date = RecordedDate) %>%  #dans cet ordre.
  mutate(id = tolower(id)) #gestion de la casse.

d <- d %>% 
  mutate(
    date = case_when(date >= as.POSIXct("20.10.2019", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("05.11.2019", format="%d.%m.%Y", tz="utc") ~ "temps 1",
                     date >= as.POSIXct("16.12.2019", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("20.12.2019", format="%d.%m.%Y", tz="utc") ~ "temps 2",
                     date >= as.POSIXct("19.01.2020", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("24.01.2020", format="%d.%m.%Y", tz="utc") ~ "temps 3",
                     date >= as.POSIXct("18.03.2020", format="%d.%m.%Y", tz="utc") & date <= as.POSIXct("04.04.2020", format="%d.%m.%Y", tz="utc") ~ "temps 4",
                     TRUE ~ "autre temps")) #on privilégie case_when car on a 5 conditions et on va gérer les dates. On devrait pas avoir d'autre temps.

# Au passage, R a modifié le type de variable "date".

# une idée des données par date en créant l'objet n_date_d
n_date_d <- d %>% 
  group_by(date) %>% 
  summarize(n=n(),)

# Recodage des variables au score inversé
d <- d %>% 
  mutate(con10_4 = 10 - con10_4,
         con10_5 = 10 - con10_5,
         con10_6 = 10 - con10_6,
         con10_7 = 10 - con10_7,
         con10_9 = 10 - con10_9,
         sem17_5 = 6 - sem17_5,
         sem17_10 = 6 - sem17_10,
         kid17_6 = 6 - kid17_6,
         kid17_15 = 6 - kid17_15,
         kid17_16 = 6 - kid17_16,
         kid17_17 = 6 - kid17_17)

# Création des moyennes de chaque questionnaire pour chaque observation (sauf les 3 shorts questions)

d <- d %>% 
  mutate(sem_sco = rowMeans(select(.,starts_with("sem")) ,na.rm =T),
         con_sco = rowMeans(select(.,starts_with("con")),na.rm =T),
         kid_sco = rowMeans(select(.,starts_with("kid")),na.rm =T),
         sho1_sco = sho_1,
         sho2_sco = sho_2,
         sho3_sco = sho_3)