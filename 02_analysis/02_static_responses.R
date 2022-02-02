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


# ------------ Created factorized dataframes
# Find the factor columns
fac_cols <- which(sapply(FUN = is.factor, fata.questions))


# ------------ Refactor the factor columns
ny <- c(0, 1)
ony <- c(NA, 0, 1)
nature <- c(NA, 3, 2, 0, 1)
effects <- c(NA, 2, 1, 3, NA, 0)

levels(fata.questions[, fac_cols[1]]) <- c(NA, 0, 1, 2, 3, 4)
levels(fata.questions[, fac_cols[2]])  <- c(NA, 0, 1, 2)
levels(fata.questions[, fac_cols[3]])  <- ny
levels(fata.questions[, fac_cols[4]]) <- ny
levels(fata.questions[, fac_cols[5]]) <- ny
levels(fata.questions[, fac_cols[6]]) <- ny
levels(fata.questions[, fac_cols[7]]) <- ny
levels(fata.questions[, fac_cols[8]]) <- ny
levels(fata.questions[, fac_cols[9]]) <- nature
levels(fata.questions[, fac_cols[10]]) <- nature
levels(fata.questions[, fac_cols[11]]) <- nature
levels(fata.questions[, fac_cols[12]]) <- nature
levels(fata.questions[, fac_cols[12]]) <- nature
levels(fata.questions[, fac_cols[13]]) <- nature
levels(fata.questions[, fac_cols[14]]) <- nature
levels(fata.questions[, fac_cols[15]]) <- nature
levels(fata.questions[, fac_cols[16]]) <- ony
levels(fata.questions[, fac_cols[17]]) <- ony
levels(fata.questions[, fac_cols[18]]) <- effects
levels(fata.questions[, fac_cols[19]]) <- effects
levels(fata.questions[, fac_cols[20]]) <- effects
levels(fata.questions[, fac_cols[21]]) <- effects
levels(fata.questions[, fac_cols[22]]) <- effects
levels(fata.questions[, fac_cols[23]]) <- effects
levels(fata.questions[, fac_cols[24]]) <- effects
levels(fata.questions[, fac_cols[25]]) <- ny
levels(fata.questions[, fac_cols[26]]) <- ny
levels(fata.questions[, fac_cols[27]]) <- ny


# Split META and FEATURE variables
fata.wo_questions <- fata[, !(colnames(fata) %in% colnames(fata.questions))]

meta_df <- fata.questions[c(2, 3)]
meta_df <- as.data.frame(cbind(fata.wo_questions[, c(59, 63, 62, 65:68)]))

feat_df <- fata.questions[c(1, 4, 7:ncol(fata.questions))]

# Create also a numeric dataframe for features
feat_df.num <- feat_df
for (i in which(sapply(FUN = is.factor, feat_df.num))) {
# as.numeric(levels(f))[f]
  feat_df.num[, i] <- as.numeric(levels(feat_df.num[, i]))[feat_df.num[, i]]
}

# Remove missing 88 99
feat_df.num[feat_df.num >= 50] <- NA
feat_df.num_o <- feat_df.num
feat_df.num_o[is.na(feat_df.num_o)] <- 0

# ------------ CSVs for python
write.csv(data, "./01_data/mod_data.csv")
write.csv(data, "./01_data/mod_fata.csv")
write.csv(fata.questions, "./01_data/mod_question_data.csv")
write.csv(fata.questions, "./01_data/mod_question_fata.csv")
write.csv(feat_df.num, "./01_data/fata_num_questions.csv")
