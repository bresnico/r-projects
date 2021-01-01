# Préparation des packages
library(tidyverse)
library(rio)
library(readxl)

# Acquisition des données
peers <- read_xlsx("peers/hlm2020_nb_peers_raw.xlsx")
teach <- read_xlsx("teach/hlm2020_nb_teach_raw.xlsx")

# Création de mon résumé d'échantillon à la main

sample <- data.frame("classe"=c("A","B"), "gest"=c("duo","solo"), "deg"=c("4","5"), "cyc"=c("2","2"), "sit"=c("Monthey","Haut-Lac"), "enf"=c(2,1))

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
