
# Load library
library(limer)

# Setup API details
options(lime_api = 'https://survey.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = '*****')


# Do stuff with LimeSurvey API
get_session_key()  # Log in
responses <- get_responses(251482)  # Get results from survey

# Paramètres d'importation des données
raw_data <- call_limer(method = "export_responses", 
                       params = list(iSurveyID = 12345, 
                                     sDocumentType = "csv", 
                                     sLanguageCode = "en", 
                                     sCompletionStatus = "complete", 
                                     sHeadingType = "code", 
                                     sResponseType = "long"))
base64_to_df(raw_data) #Si nécessaire



# Release session key
release_session_key()
