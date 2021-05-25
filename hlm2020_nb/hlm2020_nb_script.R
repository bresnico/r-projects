# Préparation des packages
library(tidyverse)
library(lubridate)
library(rio)
library(readxl)
library(ggrepel) # pour les labels des plots
library(limer)

# Acquisition des données

peers <- read_xlsx("../../data/hlm2020_nb_peers_raw.xlsx")
teach <- read_xlsx("../../data/hlm2020_nb_teach_raw.xlsx")

options(lime_api = 'https://survey.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'nbr_low') #Compte limité
options(lime_password = '82BBdJyTjqzz')
get_session_key()  # Log in
responses <- get_responses(993289, sResponseType = "short")  # Get results from survey
release_session_key()
quali <- responses

quali <- quali %>% 
  rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
  rename_with(~ gsub('[[:punct:]]', '', .x)) %>% #on peut mettre un _ à la place de rien
  select(!c("lastpage","seed","startdate","submitdate",)) %>% 
  rename(lan = startlanguage, dat = datestamp)

quali$dat <- ymd_hms(quali$dat)


quali <- quali %>% 
  mutate(
    a = factor(a, levels = c("1","2"), labels = c("Haut-Lac", "Monthey")),
    b = factor(b, levels = c("1","2"), labels = c("Cycle 1", "Cycle 2")),
    c1 = factor(c1, levels = c("1","2","3","4"), labels = c("Totalement en désaccord", "plutôt en désaccord", "plutôt en accord", "totalement en accord")),
    c2 = factor(c2, levels = c("1","2","3","4"), labels = c("Totalement en désaccord", "plutôt en désaccord", "plutôt en accord", "totalement en accord")),
    c3 = factor(c3, levels = c("1","2","3","4"), labels = c("Totalement en désaccord", "plutôt en désaccord", "plutôt en accord", "totalement en accord")),
    c4 = factor(c4, levels = c("1","2","3","4"), labels = c("Totalement en désaccord", "plutôt en désaccord", "plutôt en accord", "totalement en accord")),
    c5 = factor(c5, levels = c("1","2","3","4"), labels = c("Totalement en désaccord", "plutôt en désaccord", "plutôt en accord", "totalement en accord")),
    )


# Création de mon résumé d'échantillon à la main

sample <- data.frame("classe"=c("A","B","C","D"), "gest"=c("duo","solo","solo","solo"), "deg"=c("4","5","3","4"), "cyc"=c("2","2","1","1"), "sit"=c("Monthey","Haut-Lac","Haut-Lac","Haut-Lac"), "enf"=c(2,1,"classe",1))

# teach - création des scores des sous-dimensions

teach <- teach %>% 
  mutate(sco_gp = rowMeans(select(., c("sep16_2","sep16_3","sep16_6","sep16_10","sep16_11","sep16_15")) ,na.rm = T),
         sco_gr = rowMeans(select(., c("sep16_5","sep16_8","sep16_9","sep16_12","sep16_13")), na.rm = T),
         sco_ip = rowMeans(select(., c("sep16_1","sep16_4")), na.rm = T),
         sco_ie = rowMeans(select(., c("sep16_7","sep16_14","sep16_16")), na.rm = T),
         sco_tot = rowMeans(select(., starts_with("sep16_")), na.rm = T),
                  )

# teach - statistiques de base

teach_sum <- teach %>% 
  group_by(clas) %>% 
  summarize(sco_gp,
            sco_gr,
            sco_ip,
            sco_ie,
            sco_tot)

# peers - création du score total

peers <- peers %>% 
  mutate(
         be8_4 = 8 - be8_4,
         be8_5 = 8 - be8_5,
         be8_8 = 8 - be8_8,
         )

peers <- peers %>% 
  mutate(
         sco_be = rowMeans(select(.,starts_with("be")) ,na.rm =T)
         )

# peers - statistiques de base

peers_sum <- peers %>% 
  group_by(clas) %>% 
  summarize(mean=mean(sco_be), max=max(sco_be), min=min(sco_be), median=median(sco_be), std=sd(sco_be))

# peers - visualisation

be_vis <- peers %>%
  ggplot() +
  aes(x = clas, y = sco_be, fill = clas, color = clas, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(size = 5, alpha = .5, width = 0.3) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans chaque classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))

############
# classe a #
############

# sample (à la main)

sample_a <- data.frame("classe"=c("A"), "gest"=c("duo"), "deg"=c("4"), "cyc"=c("2"), "sit"=c("Monthey"), "enf"=c(2))


# teach - statistiques de base

teach_sum_a <- teach %>% 
  group_by(clas) %>%
  filter(clas=="a") %>% 
  summarize(sco_gp,
            sco_gr,
            sco_ip,
            sco_ie,
            sco_tot)

# peers - statistiques de base

peers_sum_a <- peers %>% 
  filter(clas=="a") %>% 
  group_by(clas) %>% 
  summarize(mean=mean(sco_be), max=max(sco_be), min=min(sco_be), median=median(sco_be), std=sd(sco_be))

# peers - visualisation
pos <- position_jitter(width = 0.2, seed = 2)
peers_a <- peers %>% 
  filter(clas=="a")

be_vis_a <- peers_a %>%
   
  ggplot() +
  aes(x = clas, y = sco_be, fill = clas, color = clas, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans la classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))


############
# classe b #
############

# sample (à la main)

sample_b <- data.frame("classe"=c("B"), "gest"=c("solo"), "deg"=c("5"), "cyc"=c("2"), "sit"=c("Haut-Lac"), "enf"=c(1))


# teach - statistiques de base

teach_sum_b <- teach %>% 
  group_by(clas) %>%
  filter(clas=="b") %>% 
  summarize(sco_gp,
            sco_gr,
            sco_ip,
            sco_ie,
            sco_tot)

# peers - statistiques de base

peers_sum_b <- peers %>% 
  filter(clas=="b") %>% 
  group_by(clas) %>% 
  summarize(mean=mean(sco_be), max=max(sco_be), min=min(sco_be), median=median(sco_be), std=sd(sco_be))

# peers - visualisation
pos <- position_jitter(width = 0.2, seed = 2)
peers_b <- peers %>% 
  filter(clas=="b")

be_vis_b <- peers_b %>%
  
  ggplot() +
  aes(x = clas, y = sco_be, fill = clas, color = clas, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans la classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))

  


############
# classe c #
############

# sample (à la main)

sample_c <- data.frame("classe"=c("C"), "gest"=c("solo"), "deg"=c("3"), "cyc"=c("1"), "sit"=c("Haut-Lac"), "enf"=c("classe"))


# teach - statistiques de base

teach_sum_c <- teach %>% 
  group_by(clas) %>%
  filter(clas=="c") %>% 
  summarize(sco_gp,
            sco_gr,
            sco_ip,
            sco_ie,
            sco_tot)

# peers - statistiques de base

peers_sum_c <- peers %>% 
  filter(clas=="c") %>% 
  group_by(clas) %>% 
  summarize(mean=mean(sco_be), max=max(sco_be), min=min(sco_be), median=median(sco_be), std=sd(sco_be))

# peers - visualisation
pos <- position_jitter(width = 0.2, seed = 2)
peers_c <- peers %>% 
  filter(clas=="c")

be_vis_c <- peers_c %>%
  
  ggplot() +
  aes(x = clas, y = sco_be, fill = clas, color = clas, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans la classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))


############
# classe d #
############

# sample (à la main)

sample_d <- data.frame("classe"=c("D"), "gest"=c("solo"), "deg"=c("4"), "cyc"=c("1"), "sit"=c("Haut-Lac"), "enf"=c(1))


# teach - statistiques de base

teach_sum_d <- teach %>% 
  group_by(clas) %>%
  filter(clas=="d") %>% 
  summarize(sco_gp,
            sco_gr,
            sco_ip,
            sco_ie,
            sco_tot)

# peers - statistiques de base

peers_sum_d <- peers %>% 
  filter(clas=="d") %>% 
  group_by(clas) %>% 
  summarize(mean=mean(sco_be), max=max(sco_be), min=min(sco_be), median=median(sco_be), std=sd(sco_be))

# peers - visualisation
pos <- position_jitter(width = 0.2, seed = 2)
peers_d <- peers %>% 
  filter(clas=="d")

be_vis_d <- peers_d %>%
  
  ggplot() +
  aes(x = clas, y = sco_be, fill = clas, color = clas, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans la classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))

##################
# synthèse quali #
##################

# Liste des items
items <- list("J'ai trouvé l'intervention de l'ERIE très utile globalement.",
              "Je pense que le ou les élèves concernés par l'intervention ont progressé dans leur comportement.",
              "Je pense que l'intervention de l'ERIE a fait évoluer ma manière d'enseigner.",
              "J'ai le sentiment que l'intervention de l'ERIE m'a permis de prendre du recul sur la situation.",
              "Je pense qu'une intervention de l'ERIE dans ma classe ne devrait jamais durer trop longtemps dans l'année scolaire."
              )

# Liste des noms des variables que l'on veut (q1 à q5 uniquement).
var_list <- c(names(quali)[6:10], names(quali)[13])

# création de la liste pour accueillir les 6 plots
plot_list = list()

for (i in 1:5) {
  p <- quali %>% 
    ggplot() +
    aes(fill = b) +
    aes_string(x = var_list[i]) +
    labs(title = paste(items[[i]]), y = "Somme") +
    theme(plot.title = element_text(hjust = 0.5, size = 6),
          axis.text.x = (element_text(angle = 50, vjust = 0.5, hjust=0.5))) +
    geom_bar() +
    labs(x = "", fill = "Par cycle") +
    scale_x_discrete(drop = FALSE) # Forcer l'affichage des catégories vides
  #theme(legend.title = element_blank()) # Pour cacher le titre de la légende
  plot_list[[i]] = p
}

# Synthèse

syn <- data.frame("Thèmes"=c("Dynamique de projet","Organisation annuelle","Développement de compétences des élèves","Développement de compétences des enseignant·es","Attractivité du métier"),
                  "Risques"=c("Perdre de vue les objectifs de progression","Se suradapter aux besoins des collègues","Considérer l'élève comme le problème de la classe","Laisser les personnes-ressources porter seules","S'épuiser dans la mission de pompier urgentiste"),
                  "Opportunités"=c("Repérer les situations stagnantes","Renforcer la collaboration","Considérer les ressources et besoins de l'élève dans sa classe","Engager les enseignant·es dans la recherche de solutions","Gérer des situations imprévues"),
                  "Pistes"=c("contractualiser la durée","Figer un calendrier annuel des rencontres","Prendre en compte l'élève dans son système classe","Mettre en oeuvre un protocole de prise en charge","trouver des défis stimulants dans un cadre protecteur")
                 )
