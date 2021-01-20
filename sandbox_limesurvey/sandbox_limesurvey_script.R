
# Load library
library(limer)

# Setup API details
options(lime_api = 'https://survey.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = '******')


# Do stuff with LimeSurvey API
get_session_key()  # Log in
responses <- get_responses(731372, sResponseType = "short")  # Get results from survey

# Paramètres d'importation des données
# raw_data <- call_limer(method = "export_responses", 
#                        params = list(iSurveyID = 731372, 
#                                      sDocumentType = "csv", 
#                                      sLanguageCode = NULL, 
#                                      sCompletionStatus = "complete", 
#                                      sHeadingType = "code", 
#                                      sResponseType = "short"))
# d <- base64_to_df(raw_data) #Si nécessaire

# Release session key
release_session_key()

# Mise à jour des noms des variables PAS ENCORE BON
library(tidyverse)
 d <- responses

# Mise à jour des variables selon une syntaxe de type q1_1
d <- d %>% 
  rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
  rename_with(~ gsub('[[:punct:]]', '_', .x)) 
 
 