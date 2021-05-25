# Faire deux lignes animées pour mon job 20-21
# une ligne "planifié"
# une ligne "réalisé"
# Par domaine

# Autre graphique du temps total par mois

# Packages
library(tidyverse)
library(readxl)

# Data
d <- read_xlsx("../../data/hepvs2021_nb.xlsx", sheet = 2)
e <-  read_xlsx("../../data/hepvs2021_nb.xlsx", sheet = 1)
e <- e[1:15,]

e <- e %>% 
  rename(Domaine = `Domaine (1 % = 19 h)`,
         Réalisé = `Réalisé (1 aout - 1 mai)`)
  
# Ggplot statique
p <- e %>% 
  ggplot() +
  aes(x = Domaine, y = Planifié) +
  geom_line()

# gganimate

