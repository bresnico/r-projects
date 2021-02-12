library(tidyverse)

# On va créer une boucle pour calculer des scores (moyennes d'items) avec map().

# Soit un df comprenant 4 items be et 4 items emo.

id <- c("luc", "josette")
be_1 <- c(6,8)
be_2 <- c(5,8)
be_3 <- c(3,9) 
be_4 <- c(3,9)
emo_1 <- c(1,2)
emo_2 <- c(2,3)
emo_3 <- c(2,3)
emo_4 <- c(1,2)

d <- data.frame(id, be_1, be_2, be_3, be_4, emo_1, emo_2, emo_3, emo_4)

# classique----

# marrant de constater que le code ne marche que grâce à ., et aussi na.rm
d <- d %>% 
  mutate(be_sco = rowMeans(select(.,starts_with("be")), na.rm = T),
         emo_sco = rowMeans(select(.,starts_with("emo")), na.rm = T)
         )

# Loop for----
# fonctionnel. MAIS j'ai pas du tout compris le sens de "{}" et : dans mutate
# c'est lié aux attentes de mutate comme premier paramètre name-value pair. solution dans <data-masking> help.
# interpolation syntax from the glue package: "{var}" := expression.

variables <- list("be", "emo")

for (i in seq_along(variables)) {
  
  temp_name <- paste0(variables[[i]],"_sco")
  
  d <- d %>%
    mutate("{temp_name}" := rowMeans(select(.,starts_with(variables[[i]])), na.rm = T))
  
}


# map()----


variables <- list("be", "emo")

# function----
