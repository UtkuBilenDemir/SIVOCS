library(limer)
library(yaml)
library(digest)
library(caTools)
library(base64enc)

cred <- read_yaml("01_data/cred.yaml")
cred$user

#change the next options (website, user, password)
options(lime_api = cred$connector)
options(lime_username = cred$user)
options(lime_password = cred$password)

# first get a session access key
get_session_key()
# list all surveys. A vector is returned
survey_vector <- call_limer(method='list_surveys')
# Convert the vector into a matrix
survey_matrix <- matrix(survey_vector, ncol=5)

data <- get_responses(iSurveyID = 718586, sResponseType = 'short')
nrow(data)


## raw_data <- call_limer(method = "export_responses", 
##                         params = list(iSurveyID = 718586, 
##                                     sDocumentType = ".R", 
##                                     sLanguageCode = "en", 
##                                     sCompletionStatus = "complete", 
##                                     sHeadingType = "code", 
##                                     sResponseType = "long"))
## 
## raw_data <- base64_to_df(raw_data)
## raw_data
