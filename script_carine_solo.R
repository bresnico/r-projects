#Zone de lancement des packages----
library(readxl)
library(tidyverse)

#modification github !!!

#Importation des données brutes----
data1 <- read_excel("data_raw_carine_roche.xlsx") #on note la présence de données manquantes. Les deux scores aberrants ont été transformés en NA dans le fichier excel.
data2 <- data1 #data2 utilisé pour les échantillons dans le markdown.
data2$classe <- as.factor(data2$classe) #transformation de la variable nommée "classe" en variable catégorielle
levels(data2$classe)[c(1,2)] <- c("Classe A","Classe B") #modification des levels de la variable catégorielle "classe"

#Obtention des données de l'échantillon----
data3 <- data1 %>% 
  summarise(n_total=n(),
             n_filles=sum(sexe=="F"),
             n_garcons=sum(sexe=="G"),
             n_categories=nlevels(data2$classe),
             n_classe_A=sum(classe=="1"),
             n_classe_B=sum(classe=="2"),
            #créé uniquement ci-dessous pour faire mon tableau artisanal
             n_classe_A_filles=sum(sexe=="F"&classe=="1"),
             n_classe_A_garcons=sum(sexe=="G"&classe=="1"),
             n_classe_B_filles=sum(sexe=="F"&classe=="2"),
             n_classe_B_garcons=sum(sexe=="G"&classe=="2"))

#Préparation du tableau artisanal (parce que je sais pas le générer) de résumé par level de la variable classe pour la présentation de l'échantillon
#ne fonctionne pas.
data4 <- matrix(c("Classe","n","Filles","Garçons",levels(data2$classe)[1],data3$n_classe_A,data3$n_classe_A_filles,data3$n_classe_A_garcons,levels(data2$classe)[2],data3$n_classe_B,data3$n_classe_B_filles,data3$n_classe_B_garcons),nrow=3, ncol=4, byrow=TRUE)

#autre essai avec la création de data2bis à partir de data2. Utilisation de as.factor puis table().
data2bis <- data2[c(2,3)]
as.factor(data2bis$sexe)
sample <- table(data2bis)
sample2 <- addmargins(sample)
colnames(sample2)[3] <- "total"
rownames(sample2)[3] <- "total"
#Obtention des moyennes et écart-types pour chaque classe et à chaque temps. ----
#En jouant avec group_by et mutate
data5 <- data1 %>% 
  mutate(score_avant=rowMeans(select(.,starts_with("avant")),na.rm =T),
         score_apres=rowMeans(select(.,starts_with("apres")),na.rm =T),
         diff_score=score_apres-score_avant)

data6 <- data5 %>% 
  group_by(classe,sexe) %>% 
  summarise(n=n(),
            avant_avg=mean(score_avant),
            apres_avg=mean(score_apres),
            avant_sd=sd(score_avant),
            apres_sd=sd(score_apres),
  )
data6[4:7]<- round(data6[4:7],2) #arrondir les mean et sd à 2 décimales.
#Création des boxplots descriptifs----
data7 <- data5[,c(2,32,33)]
# je dois restructurer mon tableau...car la variable de score est unique. Mais je dois ajouter la variable de temps.
data8 <- pivot_longer(data7, c(score_avant,score_apres), names_to="temps", values_to="score")
# je veux que score_avant apparaisse en premier
data8$temps <- factor(data8$temps,levels=c("score_avant","score_apres"))
data8$classe <- as.factor(data8$classe)
levels(data8$classe)[levels(data8$classe)=="1"] <- "A"
levels(data8$classe)[levels(data8$classe)=="2"] <- "B"
#le graph avec deux variantes

library(ggplot2)

plot_desc <- ggplot(data8, aes(y = score, x = temps, fill=factor(classe))) +
  geom_boxplot() +
  geom_jitter(color = "red", alpha = 0.5)

plot_desc2 <- ggplot(data8) +
 aes(x = temps, y = score, fill = classe)+
 geom_boxplot() +
 labs(x = "Temps de mesure", y = "Score des élèves", title = "Représentation des statistiques descriptives", subtitle = "groupement par classe") +
 theme_minimal() +
 facet_wrap(vars(classe))




