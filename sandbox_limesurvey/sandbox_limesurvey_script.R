
# Load library
library(limer)
library(tidyverse)
library(lubridate)
library(rio)

# Setup API details
options(lime_api = 'https://survey.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = 'L0912@tercy21')


# Do stuff with LimeSurvey API
get_session_key()  # Log in
responses <- get_responses(191945, sResponseType = "short")  # Get results from survey

# Paramètres d'importation des données
# raw_data <- call_limer(method = "export_responses", 
#                        params = list(iSurveyID = 191945, 
#                                      sDocumentType = "csv", 
#                                      sLanguageCode = NULL, 
#                                      sCompletionStatus = "complete", 
#                                      sHeadingType = "code", 
#                                      sResponseType = "short"))
# d <- base64_to_df(raw_data) #Si nécessaire

# Release session key
release_session_key()

# Mise à jour des noms des variables PAS ENCORE BON
d <- responses

# Mise à jour des variables selon une syntaxe de type q1_1, tri et adaptation des variables meta
d <- d %>% 
  rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
  rename_with(~ gsub('[[:punct:]]', '_', .x)) %>%
  select(!c("lastpage","seed","startdate","submitdate",)) %>% 
  rename(lan = startlanguage, dat = datestamp)

# Mise à jour light
d <- d %>% 
  select(!c("lastpage","seed","startdate","submitdate",)) %>% 
  rename(lan = startlanguage, dat = datestamp)

# Mise à jour du format des dates avec lubridate
d$dat <- ymd_hms(d$dat)

# Gestion des temps de mesure
d <- d %>% 
mutate(
  tim = case_when(dat >= as_date("01.01.2021", format="%d.%m.%Y") & dat <= as_date("21.01.2021", format="%d.%m.%Y") ~ "temps 1",
                  TRUE ~ "autre temps"))

export(d,"sortie.xlsx")
