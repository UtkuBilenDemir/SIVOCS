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
options(lime_password =cred$password)

# first get a session access key
get_session_key()
# list all surveys. A vector is returned
survey_vector<-call_limer(method='list_surveys')
# Convert the vector into a matrix
survey_matrix <- matrix(survey_vector, ncol=5)

print(survey_matrix[1,])
data <- get_responses(iSurveyID = 718586, sResponseType = 'short')

responses <- base64_to_df(get_responses(718586))
get_responses(iSurveyID= 718586, sLanguageCode= 'latin1', sResponseType='short')

call_limer(method="export_responses",
params = list (iSurveyID=718586,
sDocumentType="csv",
sLanguageCode=" "))

data <- call_limer(method = "export_statistics" ,
                  params = list(iSurveyID = 718586
                  ))
data <- base64_to_df(data)
data

hmac(key = base64decode(data),
 object = data,
 algo = 'sha512',
 raw = TRUE)

### !!! USELESS UNTIL the connector GETS PERMISSION ON RESPONSES !!! ###