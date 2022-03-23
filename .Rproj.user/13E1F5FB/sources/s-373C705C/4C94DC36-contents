library(fastDummies)
library(stringr)

data <- read.csv("./01_data/results-survey718586.csv")

# name the meta variables correctly
colnames(data)[142:ncol(data)] <- c("persID",
                                    "gender",
                                    "projID",
                                    "title",
                                    "discipline",
                                    "funding",
                                    "runtime"
                                    )

# Create a df with only the questions
data.questions <- data[, 11:94]

# Handle disciplines
setwd("/home/ubd/Nextcloud/utku_SIVOCS")
## disciplines <- read.csv("./DisciplinesPerFundingInstrument.csv", sep=";")


#--- Domains
## disc_df <- read.csv("disc_df.csv")
## data$domain <- disc_df$domain[as.vector(sapply(data$discipline, FUN = function(x) {which(x == disc_df$discipline)}))]
grant_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantExport_with_abstracts.csv", sep=";")
match_grant_df <- grant_df[match(data$projID, grant_df$Project.Number), ]
domain <- unlist(lapply(str_split(match_grant_df$Discipline.Name.Hierarchy, ";"), `[[`, 1))
data$domain <- domain 

data <- dummy_cols(data, select_columns = "domain")
colnames(data)[150] <- "Biology_and_Medicine"
colnames(data)[151] <- "SSH"
colnames(data)[152] <- "Math._Natur._and_Eng."

# A df with only numerical columns
data.num_questions <- data.questions[, as.vector(sapply(FUN=is.numeric, data.questions))]

# A df for python
write.csv(data, "./01_data/mod_data.csv")
write.csv(data.questions, "./01_data/mod_question_data.csv")
