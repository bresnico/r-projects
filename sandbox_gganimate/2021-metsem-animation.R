# 2021 - 01 - 14
# Exemple de construction d'animations avec R
# B. Coulmont

# packages
library(tidyverse)
library(gganimate)

load(url("http://coulmont.com/varia/data-metsem.Rdata"))

p <- augustin  %>% 
  ggplot() +
  geom_point() + 
  aes(x = dpt, y = rang) +
  transition_time (time = annais) +
  shadow_wake(wake_length = .25, wrap = FALSE, alpha = .1,  size= .3, colour = "red") +
  scale_y_reverse() +
  labs(title = "Rang du prénom  en {round(frame_time)}")

animate(p,
        nframes = 10,
        width = 800,
        height = 800,
        renderer = av_renderer("anime.mp4",
                               codec = "libx264"))


animate(p,
        renderer = ("anime.gif"))
