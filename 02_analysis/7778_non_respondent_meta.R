# Import the whole sample
sample_df <- read.csv("./01_data/snf_df_-_fname_lname.csv")

# Respondent Data Frame
source("./02_analysis/02_static_responses.R")

# Expansion steps are already established at
# 1502_batch2_expansion.R

collab_df <- read.csv("./5555_preliminary_analysis/00_data/P3_CollaborationExport.csv", sep=";")
grant_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantExport_with_abstracts.csv", sep=";")
output_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantOutputDataExport.csv", sep=";")
person_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PersonExport.csv", sep=";")
pub_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PublicationExport.csv", sep=";")

projID <- sample_df$SP.Project.Number
persID <- sample_df$ID

# All columns from collab_df
match_collab_df <- collab_df[match(projID, collab_df$Project.Number), ]
match_collab_df <- match_collab_df[, c(3:ncol(match_collab_df))]


# Frant Export
match_grant_df <- grant_df[match(projID, grant_df$Project.Number), ]
match_grant_df <- match_grant_df[, c(1, 3, 5, 8:10, 12, 13, 15, 16, 18, 19)]
match_grant_df <- match_grant_df[, c(4:8, 11:12)]

# Output
match_output_df <- output_df[match(projID, output_df$Project.Number), ]

# Person
match_person_df <- person_df[match(persID, person_df$Person.ID.SNSF), ]
match_person_df <- match_person_df[, 4:7]

# Publication
match_pub_df <- pub_df[match(projID, pub_df$Project.Number), ]
match_pub_df <- match_pub_df[, c(3:10, 15:17, 19:26)]


rownames(sample_df) <- 1:nrow(sample_df)
rownames(match_grant_df)  <- 1:nrow(match_grant_df)
rownames(match_person_df) <- 1:nrow(match_person_df)
rownames(match_collab_df) <- 1:nrow(match_collab_df)
rownames(match_pub_df)    <- 1:nrow(match_pub_df)
rownames(match_output_df) <- 1:nrow(match_output_df)


out_df <- as.data.frame(cbind(sample_df,
                        match_grant_df,
                        match_person_df,
                        match_collab_df, 
                        match_pub_df,
                        match_output_df
                        )
)



sum(sample_df$ID != out_df$Person.ID.SNSF)



# REMOVE SURVEY PEOPLE
non_respondent_df <- out_df[-c(as.vector(match(data$persID, persID))), ]

# REMOVE OLD PROJECTS
non_respondent_df$SP.End.Date[non_respondent_df$Number.of.Projects == 1 ] <- non_respondent_df$Project.End.Dates[non_respondent_df$Number.of.Projects == 1 ]
end_years <- as.numeric(lapply(sapply(FUN=strsplit, non_respondent_df$SP.End.Date, "\\."), `[[`, 3))
non_respondent_df <- non_respondent_df[end_years > 2017, ]

write.csv(non_respondent_df, "./01_data/denden.csv")
