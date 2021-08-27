library(knitr)
library(tidyverse)
library(lubridate)
library(rio)
library(readxl)
library(ggrepel) # pour les labels des plots
library(limer)

# Acquisition des données

options(lime_api = 'https://sondage.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'nbr_low')
options(lime_password = '82BBdJyTjqzz')

# log in
get_session_key()

# Données enfants

responses <- get_responses(717311, sResponseType = "short")  
peers <- responses

# Données profs
responses <- get_responses(851424, sResponseType = "short")  
teach <- responses

#log out
release_session_key()

# Préparation de la boucle pour traitement des données.
vec <- list("peers", "teach")

# Mise à jour des variables selon une syntaxe de type q1_1, tri et adaptation des variables meta

for (i in 1:2) {
  g <- get(vec[[i]])
  
  g <- g %>% 
    rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
    rename_with(~ gsub('[[:punct:]]', '_', .x)) %>%
    select(!c("lastpage","seed","startdate","submitdate",)) %>% 
    rename(lan = startlanguage, dat = datestamp) 
    
  # Création de la variable classe
  g$classe <- str_extract(g$q1, "[a-z]+")
  
  # Création de la variable temps
  g$temps <- str_sub(g$q1, start = -2L, end = -2L)
  
  # Mise à jour du format des dates avec lubridate
  g$dat <- ymd_hms(g$dat)
  
  # Modification des df d'origine
  
  if (i == 1) {
    peers <- g %>% 
      mutate(be8_4 = 8 - be8_4,
           be8_5 = 8 - be8_5,
           be8_8 = 8 - be8_8) %>% 
      mutate(sco_be  = rowMeans(select(.,starts_with("be")) ,na.rm =T))
  } else {
    teach <- g %>% 
      mutate(
        sco_gp = rowMeans(select(., c("sep16_2","sep16_3","sep16_6","sep16_10","sep16_11","sep16_15")) ,na.rm = T),
        sco_gr = rowMeans(select(., c("sep16_5","sep16_8","sep16_9","sep16_12","sep16_13")), na.rm = T),
        sco_ip = rowMeans(select(., c("sep16_1","sep16_4")), na.rm = T),
        sco_ie = rowMeans(select(., c("sep16_7","sep16_14","sep16_16")), na.rm = T),
        sco_tot = rowMeans(select(., starts_with("sep16_")), na.rm = T),
      )      
  }

}

#Données enfants pris en charge (erie.xlsx)
erie <- read_excel("erie.xlsx")

#Données classes (log.xlsx)

sample <- read_excel("log.xlsx")

#########################################################################
# work in progress .....................................................#
#########################################################################

pos <- position_jitter(width = 0.2, seed = 2)

be_vis_1_2 <- peers %>%
  filter(classe=="a") %>%
  ggplot() +
  aes(x = temps, y = sco_be, fill = temps, color = classe, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores dans la classe") +
  ylab("Score de bien-être") +
  xlab("classe") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer("temps", palette = "Set1")

be_vis_1_2 <- peers %>%
  ggplot() +
  aes(x = temps, y = sco_be, fill = temps, color = classe, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  #geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation des scores aux temps 1 et 2") +
  ylab("Score de bien-être") +
  xlab("Temps") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer("temps", palette = "Set1")

######################################
# gtsummary in progress ------------ #
######################################

library(gtsummary)
gt_teach <- teach %>% 
  select(id, classe, temps, sco_gp, sco_gr)

gt_teach_sum <- gt_teach %>% 
  group_by(classe, temps) %>% 
  summarise(min=min(sco_gp),
            max=max(sco_gp),
            mean=mean(sco_gp)
            )

gt_table <- 
  tbl_summary(
    gt_teach_sum,
    by = temps, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

gt_table2 <- tbl_summary(gt_teach_sum, by = temps)

gt_table3 <-
  tbl_cross(gt_teach_sum,
            row = classe,
            col = temps,
            percent = "cell"
            ) %>%
  add_p()



###############

#Données enfants pris en charge (erie.xlsx)
erie <- read_excel("erie.xlsx")

# Création de la variable classe
erie$classe <- str_extract(erie$code, "[a-z]+")

# Création de la variable temps
erie$temps <- str_sub(erie$code, start = -2L, end = -2L)

# Création du score emo
erie <- erie %>% 
  mutate(emo5_5 = 6 - emo5_5) %>% 
  mutate(emo_sco  = rowMeans(select(.,starts_with("emo")) ,na.rm =T))

# Création du score cps
erie <- erie %>% 
  mutate(cps26_3 = 6 - cps26_3,
         cps26_25 = 6 - cps26_25) %>% 
  mutate(cps_sco  = rowMeans(select(.,starts_with("cps")) ,na.rm =T))

pos <- position_jitter(width = 0.2, seed = 2)


emo_vis <- erie %>% 
  filter(classe== "a") %>%
  filter(temps == "1") %>% 
  ggplot() +
  aes(x = classe, y = emo_sco, fill = classe, color = classe, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation") +
  ylab("Score de régulation émotionnelle") +
  xlab("élève(s) suivi(s)") +
  theme(plot.title = element_text(hjust = 0.5))  

# ERIE - élèves emo - variante temps all
emo_vis_1_2 <- erie %>% 
  filter(classe== "a") %>%
  ggplot() +
  aes(x = temps, y = emo_sco, fill = classe, color = classe, alpha = 0.8) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_label_repel(position = pos, aes(label=id), size = 4, color = "black", max.overlaps = Inf) +
  theme(legend.position='none') +
  ggtitle("Visualisation") +
  ylab("Score de régulation émotionnelle") +
  xlab("élève(s) suivi(s)") +
  theme(plot.title = element_text(hjust = 0.5)) 
