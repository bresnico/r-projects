# Packages
library(readxl)
library(tidyverse)

# Importation des données brutes présentées sous la forme d'un tableau unique.

d <- read_excel("../../data/hepvs2021_33_raw.xlsx")
str(d) 

###########################
# Préparation des données #
###########################

d <- d %>% 
  mutate(
         q1 = factor(q1, levels = c("1","2","3","4"), labels = c("pas du tout", "plutôt non", "plutôt oui", "tout à fait")),
         q2 = factor(q2, levels = c("1","2","3","4"), labels = c("pas du tout", "plutôt non", "plutôt oui", "tout à fait")),
         q3 = factor(q3, levels = c("1","2","3","4"), labels = c("pas du tout", "plutôt non", "plutôt oui", "tout à fait")),
         q4 = factor(q4, levels = c("1","2","3","4"), labels = c("pas du tout", "plutôt non", "plutôt oui", "tout à fait")),
  )

##########
# Tables #
##########
sum_ques <- d %>% 
  group_by(tra) %>% 
  summarize(n=n()) %>% 
  ungroup()


#################
# Visualisation #
#################

# Liste des items
items = list("Je suis satisfait·e du cours.",
             "Je suis parvenu·e à transférer les apports du cours en stage.",
             "J'ai le sentiment que ce cours m'a fait progresser en gestion de classe.",
             "L'adaptation du cours aux conditions sanitaires a été satisfaisante."
             )
# Plots en loop par langue

# Liste des noms des variables que l'on veut (q1 à q4 uniquement).
var_list = names(d)[2:5]

# création de la liste pour accueillir les 4 plots
plot_list = list()

for (i in 1:4) {
  p <- d %>% 
    ggplot() +
    aes(fill = tra) +
    aes_string(x = var_list[i]) +
    labs(title = paste("Mesure item ",items[[i]]), y = "Somme") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    geom_bar() +
    labs(x = "Répartition", fill = "Projections 2021") +
    scale_x_discrete(drop = FALSE) # Forcer l'affichage des catégories vides
    #theme(legend.title = element_blank()) # Pour cacher le titre de la légende
  plot_list[[i]] = p
}
