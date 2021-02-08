# Packages
library(readxl)
library(tidyverse)

# Importation des données brutes présentées sous la forme d'un tableau unique.

d <- read_excel("../../data/hepvs2020_nb_raw.xlsx")
str(d) 

###########################
# Préparation des données #
###########################

d <- d %>% 
  mutate(cat = factor(cat, levels = c("1","2","3","4"), labels = c("profs", "direction élargie", "animation", "admin et secrétariat")),
    ite1 = factor(ite1, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite2 = factor(ite2, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite3 = factor(ite3, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite4 = factor(ite4, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite5 = factor(ite5, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite6 = factor(ite6, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite7 = factor(ite7, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
    ite8 = factor(ite8, levels = c("1","2","3","4","5"), labels = c("pas du tout", "plutôt non", "moyen", "plutôt oui", "tout à fait")),
         )

##########
# Tables #
##########
sum_ques <- d %>% 
  group_by(lan, cat) %>% 
  summarize(n=n())


#################
# Visualisation #
#################

# Liste des items
items = list("Expérience positive de cette matinée","Organisation adéquate de cette matinée","Communication claire autour de cette matinée","Bonne gestion technique de cette matinée","Qualité du contenu de cette matinée","Transposition facile dans son métier","Bonne compréhension de la fermeture d'une boucle qualité","Bon sentiment de capacité personnelle à fermer les boucles")

# Plots en loop par langue

# Liste des noms des variables que l'on veut (ite1 à ite8 uniquement).
var_list = names(d)[4:11]

# création de la liste pour accueillir les 8 plots
plot_list = list()

for (i in 1:8) {
  p <- d %>% 
    ggplot() +
    aes(fill = lan) +
    aes_string(x = var_list[i]) +
    labs(title = paste("Mesure item",items[[i]]), y = "Somme") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    geom_bar()
  plot_list[[i]] = p
}
#dev.off()

# Plots en loop par catégorie de personnel

# Liste des noms des variables que l'on veut (ite1 à ite8 uniquement).
var_list2 = names(d)[4:11]

# création de la liste pour accueillir les 8 plots
plot_list2 = list()

for (i in 1:8) {
  p2 <- d %>% 
    ggplot() +
    aes(fill = cat) +
    aes_string(x = var_list2[i]) +
    labs(title = paste("Mesure item",items[[i]]), y = "Somme") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    geom_bar()
  plot_list2[[i]] = p2
}
#dev.off()


