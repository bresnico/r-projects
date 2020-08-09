#lancement des packages
library(readxl)
library(tidyverse)

#modification des en-têtes du fichier excel à la main.
#importation des données brutes présentées sous la forme d'un tableau unique de 2 temps.

d <- read_excel("hepvs2020_ib_raw.xlsx")
str(d) #on observe que la variable "RecordedDate" semble avoir été bien intégrée (POSIXct)

#######################
#travail sur les dates#
#######################
#modification de la variable "RecordedDate" en "date" en temps 1 et temps 2 tout en filtrant les id enregistrés hors temps 1 et temps 2.
#mais on commence par la renommer.

d <- d %>% 
  mutate(id = tolower(id)) #gestion de la casse.

d <- d %>% 
  mutate(
    dat = case_when(dat >= as.POSIXct("01.02.2020", format="%d.%m.%Y", tz="utc") & dat <= as.POSIXct("01.03.2020", format="%d.%m.%Y", tz="utc") ~ "temps 1",
                     dat >= as.POSIXct("01.05.2020", format="%d.%m.%Y", tz="utc") & dat <= as.POSIXct("01.07.2020", format="%d.%m.%Y", tz="utc") ~ "temps 2",
                     TRUE ~ "autre temps")) #on privilégie case_when car on a 3 conditions et on va gérer les dates.

#Au passage, R a modifié le type de variable "date". On le laisser respirer... et on filtre... (si j'intègre filter dans le pipe, ça bug...)

d <- d %>% filter(dat =="temps 1" | dat == "temps 2")


#Création des moyennes de chaque questionnaire pour chaque observation (sauf les 3 shorts questions et le score du nombre d'amis qui est "ami" - "ennemis")

#suppression des id non strictement membre d'une paire (t1, t2) par création d'un df de comparaison.

d_comp <- d %>% 
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, dat) %>% 
  count(id) %>% 
  filter(n==1) %>% #On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>% 
  group_by(id) %>% 
  count(id) %>% 
  filter(n==2) %>% #on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

#Notre df de comparaison est prêt. On peut procéder à l'élagage de d_long.

d_paired <- d %>% 
  filter(id %in% d_comp$id)

#procédons à la préparation d'un petit résumé de cet échantillon.

d_paired_sum <- d_paired %>% 
  group_by(dat, sex, lan) %>% 
  summarise(n=n())

d_paired_sum2 <- d_paired %>% 
  group_by(dat) %>% 
  summarize(n=n(),
            mean_1=mean(a),
            mean_2=mean(b),
            mean_3=mean(c),
            mean_4=mean(d),
            mean_5=mean(e),
            mean_6=mean(f, na.rm=T),
            mean_7=mean(g),
            mean_8=mean(h),
            mean_9=mean(i),
            mean_10=mean(j),
            mean_11=mean(k),
            mean_12=mean(l),
            mean_13=mean(m, na.rm=T),
            mean_14=mean(n_),
            mean_15=mean(o),
            mean_16=mean(p))
            

#######################
#visualisation en loop#
#######################
# Liste des noms des variables que l'on veut (a à p uniquement).
var_list = names(d_paired)[6:21]

# création de la liste pour accueillir les 16 (21-5) plots
plot_list = list()

for (i in 1:16) {
  p <- d_paired %>% 
    ggplot() +
    aes(x = dat) +
    aes_string(y = var_list[i]) +
    geom_jitter(size = 5, alpha = .5, width = 0.3) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
    labs(title = paste("Mesure item",i), y = "Score") +
    theme(plot.title = element_text(hjust = 0.5))  
  plot_list[[i]] = p
}
dev.off()

# enregistrement des plots en png par fichier séparé avec un nom correspondant au nom de la variable et non de son numéro.
for (i in 1:16) {
  temp_plot = plot_list[[i]]
  ggsave(temp_plot, file=paste0("plot_", var_list[[i]],".png"), width = 14, height = 10, units = "cm")
}
