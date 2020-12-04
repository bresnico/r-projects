# Chargement des libraries----

library(tidyverse) # manipulation selon les règles tidy
library(rio) # pour la conversion sav <-> csv ou/et xlsx

# Acquisition des 6 bdd et conversion----
# en xlsx pour gagner du temps dans la manipulation

for (i in 1:6) {
  a <- paste("bdd",i,".sav", sep = "")
  b <- paste("bdd",i,".xlsx", sep = "")
  convert(a,b)
}

# Conversion du Master xlsx -> csv
convert("quebec2020_jf_master.xlsx", "quebec2020_jf_master.csv")

# Le fichier csv est passé à PGA.