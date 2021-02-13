library(tidyverse)

library(magrittr)
library(dplyr)
# case study : On va créer une boucle pour calculer des scores (moyennes d'items) avec map().

# Soit un df comprenant 8 items be et 4 items emo.

d <- data.frame(id = c("luc", "josette","pierrette"), be8_1 = c(6,7,7), be8_2 = c(6,7,7), be8_3 = c(6,7,7), be8_4 = c(6,7,7), be8_5 = c(6,7,7), be8_6 = c(6,7,7), be8_7 = c(6,7,7), be8_8 = c(6,7,7), emo4_1 = c(10,2,2), emo4_2 = c(10,2,2), emo4_3 = c(10,2,2), emo4_4 = c(10,2,2))

# classique----

# marrant de constater que le code ne marche que grâce à ., et aussi na.rm
d <- d %>% 
  mutate(be_sco = rowMeans(select(.,starts_with("be")), na.rm = T),
         emo_sco = rowMeans(select(.,starts_with("emo")), na.rm = T)
         )

#OK Loop for----
# A partir des conseils de Hadley : https://r4ds.had.co.nz/iteration.html
# fonctionnel. MAIS j'ai pas du tout compris le sens de "{}" et : dans mutate
# c'est lié aux attentes de mutate comme premier paramètre name-value pair. solution dans <data-masking> help.
# interpolation syntax from the glue package: "{var}" := expression.

variables <- list("be", "emo") # C'est ici que je liste à la main les VD
temp_name <- vector("character", length = 1) # Déclaration de la variable temporaire

for (i in seq_along(variables)) {
  
  temp_name <- paste0(variables[[i]],"_sco")
  
  d <- d %>%
    mutate("{temp_name}" := rowMeans(select(.,starts_with(variables[[i]])), na.rm = T))
  
}

#OK function a partir de for----

essai <- function (df, x, fun) {
  temp_name <- vector("character", length = 1) # Déclaration de la variable temporaire: mais ca sert à rien car sa long ne change pas.
  
  for (i in seq_along(x)) {
    
    temp_name <- paste0(x[[i]],"_sco")
    
    df <- df %>%
    fun(.,"{temp_name}" := rowMeans(select(.,starts_with(x[[i]])), na.rm = T))
  }
  df
}

h2 <- essai(h,c("emo","be"),mutate)



# map()----
# no idea comment faire qqch d'élégant...






#########
# ready #
#########

# Reverse scoring items  conv_man(df, "be", c(4,5,8), 8)
# ajout d'un select pour faciliter la gestion du mutate
conv_man <- function (df, x, item, key) {
  for (i in seq_along(item)) {
    temp_item <- paste0(x,"_",item[[i]])
    df <- df %>% 
      mutate("{temp_item}" := key - pull(df,temp_item))
  }
  df
}

d_rev <- d %>% 
  conv_man(., "be8", c("4","5","8"), 8) %>% 
  conv_man(., "emo4", c("1","2"), 13)

# Scoring by var scoring(df, c("var 1, ...))


scoring <- function (df, x) {
  temp_name <- vector("character", length = 1) # Déclaration de la variable temporaire: mais ca sert à rien car sa long ne change pas.
  
  for (i in seq_along(x)) {
    
    temp_name <- paste0(x[[i]],"_sco")
    
    df <- df %>%
      mutate(.,"{temp_name}" := rowMeans(select(.,starts_with(x[[i]])), na.rm = T))
  }
  df
}

d_scored <- scoring(d_rev,c("emo","be"))




#################################################################################
# Perspectives 1----
# créer une fonction qui fait (1) mon paired sample, (2) convertir les items inversés selon les variables avec case_when, (2) créée les scores.

# OK avec pull qui extrait une seule colonne en vecteur)
# x peut être c("emo", "be")
essai_conv_auto <- function (df, x) {
  for (i in seq_along(x)) {
    temp_name_conv <- 
    if (x[[i]] == "emo") { 
      conv <- c("1","2")
      key <- 13
    } else if (x[[i]] == "be") {
      conv <- c("4","5","8")
      key <- 8
    } else if  (x[[i]] == "truc") {
      conv <- "autre"
      key <- 0
    } else {
      conv <- "inconnu"
      key < 0
    }
    
    for (j in seq_along(conv)) {
      temp_name_conv <- paste0(x[[i]],"_",conv[[j]])
      df <- df %>% 
        
        mutate("{temp_name_conv}" := key - pull(.,temp_name_conv)) 
        
    }
  }
df
}
    
g <- essai_conv_auto(d,c("emo","be"))





###################################################################
# fonction pour créer mes premiers ggplot2.

