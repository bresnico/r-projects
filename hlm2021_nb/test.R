
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

# Fonction prometteuse 

a <- tbl_continuous(
  data = peers,
  variable = sco_be,
  by = temps,
  include = classe
  )




###########

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
