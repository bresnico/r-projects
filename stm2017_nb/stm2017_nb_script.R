# lancement des packages----
library(readxl)
library(tidyverse)
library(ggrepel) # pour les labels des plots

# importation des données brutes présentées sous la forme d'un tableau unique de deux temps, issu de manipulations depuis deux Google Forms (octobre 2016 et avril 2017)

d <- read_excel("../../data/stm2017_nb_raw.xlsx")
str(d)

# Recodage des variables au score inversé----



d <- d %>% # On parle des deux variables uniquement
  mutate(ks_9 = 6 - ks_9,
         ks_10 = 6 - ks_10
         )

# Création du score général aux 26 questions et ajout à d----
d <- d %>% 
  mutate(ks_sco = rowMeans(select(.,starts_with("ks_")) ,na.rm =T),
         ks_sco = round(ks_sco,2)) # score arrondi à deux décimales

# Création du score spécifique aux items liés au bien-être psychologique et ajout à d----
d <- d %>% 
  mutate(ks_bep = rowMeans(select(.,ks_6,ks_7,ks_8,ks_9,ks_10,ks_11) ,na.rm =T),
  )

# Création du score spécifique aux items liés à l'amitié et ajout à d----
d <- d %>% 
  mutate(ks_ami = rowMeans(select(.,ks_19,ks_20,ks_21,ks_22) ,na.rm =T),
  )

# Création du score spécifique aux items liés au bien-être scolaire et ajout à d----
d <- d %>% 
  mutate(ks_bes = rowMeans(select(.,ks_23,ks_24,ks_25,ks_26) ,na.rm =T),
  )

# Création du tableau strictement pairé----

# suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d %>% 
  drop_na(id) %>% # pour éliminer les lignes non identifiées
  arrange(id) %>% # visuel
  group_by(id, tim) %>% 
  count(id) %>% 
  filter(n==1) %>% # On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>% 
  group_by(id) %>% 
  count(id) %>% 
  filter(n==2) %>% # on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

# Notre df de comparaison est prêt. On peut procéder à l'élagage.

d_paired <- d %>% 
  filter(id %in% d_comp$id) # d_paired contient donc 14 paires d'observation; on peut faire les descriptives à partir de cela.

# procédons à la préparation d'un petit résumé de cet échantillon.

d_paired_sum <- d_paired %>% 
  group_by(tim, sex) %>% 
  summarise(n=n())

# stat descriptives (mean et sd pour nos 3 scores d'intérêt)----

d_paired_sum2 <- d_paired %>% 
  group_by(tim) %>% 
  summarise(n=n(),
            mean_ks=round(mean(ks_sco),2),
            mean_ks_bep=round(mean(ks_bep),2),
            mean_ks_ami=round(mean(ks_ami),2),
            mean_ks_bes=round(mean(ks_bes),2),
            sd_ks=round(sd(ks_sco),2),
            sd_ks_bep=round(sd(ks_bep),2),
            sd_ks_ami=round(sd(ks_ami),2),
            sd_ks_bes=round(sd(ks_bes),2),
            )

# Retour sur d pour observer qualitativement les dispersions des participants d'origine (avant pairage, élimination)----

#########################
# Visualisation en loop #
#########################

# créer une position qui sera reprise par jitter et aussi text_repel (donc set.seed n'est plus nécessaire)
pos <- position_jitter(width = 0.2, seed = 2)

# Préparation de la boucle titre----
tit <- c("bien-être total",
            "bien-être psychologique",
            "amis et soutien social",
            "environnement scolaire"
           ) # vecteur pour le titre de chaque plot

# Préparation de la boucle des variables----
var <- names(d)[30:33]

# Préparation de la liste de plots----
plot_list <- list()

# chaque liste est dans le même ordre !

for (i in 1:4) {
p <- d %>% 
    ggplot() +
    aes(x = tim, colour = tim) +
    aes_string(y = var[i]) +
    geom_boxplot(outlier.shape = NA, alpha = .2, aes(fill = tim), show.legend = FALSE) +
    geom_jitter(data = subset(d, id!="gui" & id!="ben" & id!="ken" & id!="nor" & id!="mar"), position = pos, size = 5, alpha = .5, show.legend = FALSE) + # On détermine visuellement la limite qui nous intéresse pour les labels
    ylim(0, 5) +
    geom_jitter(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, size = 5, alpha = 1, color = "red", show.legend = FALSE) +
    geom_label_repel(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, aes(label=id), size = 4, color = "black") +
    labs(y="Score", x = "Temps de mesure", title = paste("Résultats au Kidscreen (27 items)", tit[i])) +
    theme(plot.title = element_text(hjust = 0.5)) 
    plot_list[[i]] <- p
    }
dev.off()  


# enregistrement des plots en png par fichier séparé avec un nom correspondant au nom de la dimension.
for (i in 1:4) {
  temp_plot = plot_list[[i]]
  ggsave(temp_plot, file=paste0("plot_", tit[[i]],".png"), width = 14, height = 10, units = "cm")
}

dev.off()  


#############################
# Visualisation sans boucle #
#############################
set.seed(2)

vis_sco <- d %>% 
  ggplot() +
  aes(x = tim, y = ks_sco, colour = tim) +
  geom_boxplot(outlier.shape = NA, alpha = .2, aes(fill = tim), show.legend = FALSE) +
  geom_jitter(data = subset(d, id!="gui" & id!="ben" & id!="ken" & id!="nor" & id!="mar"), position = pos, size = 5, alpha = .5, show.legend = FALSE) + # On détermine visuellement la limite qui nous intéresse pour les labels
  ylim(0, 5) +
  geom_jitter(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, size = 5, alpha = 1, color = "red", show.legend = FALSE) +
  geom_label_repel(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, aes(label=id), size = 4, color = "black") +
  labs(y="Score", x = "Temps de mesure", title = "Résultats au Kidscreen (26 items)") +
  theme(plot.title = element_text(hjust = 0.5))

vis_ami <- d %>% 
  ggplot() +
  aes(x = tim, y = ks_ami, colour = tim) +
  geom_boxplot(outlier.shape = NA, alpha = .2, aes(fill = tim), show.legend = FALSE) +
  geom_jitter(data = subset(d, id!="gui" & id!="ben" & id!="ken" & id!="nor" & id!="mar"), position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_jitter(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, size = 5, alpha = 1, color = "red", show.legend = FALSE) +
  geom_label_repel(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, aes(label=id), size = 4, color = "black") +
  labs(y="Score", x = "Temps de mesure", title = "Résultats au Kidscreen - dimension amitié et soutien social") +
  theme(plot.title = element_text(hjust = 0.5))

vis_bes <- d %>% 
  ggplot() +
  aes(x = tim, y = ks_bes, colour = tim) +
  geom_boxplot(outlier.shape = NA, alpha = .2, aes(fill = tim), show.legend = FALSE) +
  geom_jitter(data = subset(d, id!="gui" & id!="ben" & id!="ken" & id!="nor" & id!="mar"), position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_jitter(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, size = 5, alpha = 1, color = "red", show.legend = FALSE) +
  geom_label_repel(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, aes(label=id), size = 4, color = "black") +
  ylim(0, 5) +
  labs(y="Score", x = "Temps de mesure", title = "Résultats au Kidscreen - dimension environnement scolaire") +
  theme(plot.title = element_text(hjust = 0.5))


vis_bep <- d %>% 
  ggplot() +
  aes(x = tim, y = ks_bep, colour = tim) +
  geom_boxplot(outlier.shape = NA, alpha = .2, aes(fill = tim), show.legend = FALSE) +
  geom_jitter(data = subset(d, id!="gui" & id!="ben" & id!="ken" & id!="nor" & id!="mar"), position = pos, size = 5, alpha = .5, show.legend = FALSE) + 
  geom_jitter(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, size = 5, alpha = 1, color = "red", show.legend = FALSE) +
  geom_label_repel(data = subset(d, id=="gui" | id=="ben" | id=="ken" | id=="nor" | id=="mar"), position = pos, aes(label=id), size = 4, color = "black") +
  ylim(0, 5) +
  labs(y="Score", x = "Temps de mesure", title = "Résultats au Kidscreen - dimension bien-être psychologique") +
  theme(plot.title = element_text(hjust = 0.5))