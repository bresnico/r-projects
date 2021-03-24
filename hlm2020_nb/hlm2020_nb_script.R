# Préparation des packages
library(tidyverse)
library(rio)
library(readxl)
library(ggrepel) # pour les labels des plots

# Acquisition des données
peers <- read_xlsx("../../data/hlm2020_nb_peers_raw.xlsx")
teach <- read_xlsx("../../data/hlm2020_nb_teach_raw.xlsx")

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