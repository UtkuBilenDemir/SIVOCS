library(fastDummies)
library(stringr)

data <- read.csv("./01_data/results-survey718586.csv")
fata <- read.csv("./01_data/results-survey718586.csv", stringsAsFactors = TRUE)

# name the meta variables correctly
colnames(data)[142:ncol(data)] <- c("persID",
                                    "gender",
                                    "projID",
                                    "title",
                                    "discipline",
                                    "funding",
                                    "runtime"
                                    )
colnames(fata)[142:ncol(data)] <- c("persID",
                                    "gender",
                                    "projID",
                                    "title",
                                    "discipline",
                                    "funding",
                                    "runtime"
                                    )


# Create a df with only the questions
data.questions <- data[, 11:94]
fata.questions <- fata[, 11:94]


#--- Domains
grant_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantExport_with_abstracts.csv", sep = ";")
match_grant_df <- grant_df[match(data$projID, grant_df$Project.Number), ]
domain <- unlist(lapply(str_split(match_grant_df$Discipline.Name.Hierarchy, ";"), `[[`, 1))
data$domain <- domain 
fata$domain <- domain 

data <- dummy_cols(data, select_columns = "domain")
fata <- dummy_cols(fata, select_columns = "domain")
colnames(data)[150] <- "Biology_and_Medicine"
colnames(fata)[150] <- "Biology_and_Medicine"
colnames(data)[151] <- "SSH"
colnames(fata)[151] <- "SSH"
colnames(data)[152] <- "Math._Natur._and_Eng."
colnames(fata)[152] <- "Math._Natur._and_Eng."


# A df with only numerical columns
data.num_questions <- data.questions[, as.vector(sapply(FUN = is.numeric, data.questions))]
fata.num_questions <- fata.questions[, as.vector(sapply(FUN = is.numeric, fata.questions))]


# Eliminate the open questions from fata and fata.questions
open_questions <- c("adoptByPolicyHow.other.", "kindOfChangeOther", "impulseForNonAcad.other.")
fata <- fata[, !c(colnames(fata) %in% open_questions)]
fata.questions <- fata.questions[, !c(colnames(fata.questions) %in% open_questions)]


# ------------- CSVs for python
write.csv(data, "./01_data/mod_data.csv")
write.csv(data, "./01_data/mod_fata.csv")
write.csv(fata.questions, "./01_data/mod_question_data.csv")
write.csv(fata.questions, "./01_data/mod_question_fata.csv")
