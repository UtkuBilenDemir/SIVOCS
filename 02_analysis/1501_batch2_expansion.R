library(readxl)


cand_df <- as.data.frame(read_excel("./deneme_batch2_edited.xlsx"))
collab_df <- read.csv("./5555_preliminary_analysis/00_data/P3_CollaborationExport.csv", sep=";")
grant_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantExport_with_abstracts.csv", sep=";")
output_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantOutputDataExport.csv", sep=";")
person_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PersonExport.csv", sep=";")
pub_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PublicationExport.csv", sep=";")

projID <- cand_df$projID
persID <- cand_df$persID

# All columns from collab_df
match_collab_df <- collab_df[match(projID, collab_df$Project.Number), ]
match_collab_df <- match_collab_df[, c(3:ncol(match_collab_df))]


# Add URLs
url_base <- "https://survey3.zsi.at/index.php/admin/responses/sa/view/surveyid/718586/id/"
url_col <- paste0(url_base, cand_df$id)


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

colnames(match_output_df)

colnames(match_person_df)
head(match_person_df)


# Adjust candidate df

cand_df$name <- paste(cand_df$firstname, cand_df$lastname)

colnames(cand_df)
cand_df_reord <- cand_df[, c(1, 156, 141, 142,145,  143, 144, 146, 147, 148, 151, 149, 6, 7, 
            13:97)]




out_df <- as.data.frame(cbind(cand_df_reord[, 1:13], 
                              url_col, 
                              match_grant_df,
                              match_person_df,
                              match_collab_df, 
                              match_pub_df,
                              match_output_df,
                              cand_df_reord[, 14:ncol(cand_df_reord)]))


colnames(out_df)[27] <- "Collaboration.Country"
colnames(out_df)[13] <- "cluster"
colnames(out_df)[14] <- "survey.URL"
colnames(out_df)[48] <- "Journal.Abstract"
colnames(out_df)[52] <- "Output.URL"
colnames(out_df)[53] <- "Output.Year"
colnames(out_df)
out_df <- out_df[, c(2:13, 54, 14:17, 20, 21, 24:53, 55:(ncol(out_df))-1)]
write.csv(out_df, "batch2.1.csv")
saveRDS(out_df, "batch2.1.RDS")


table(out_df$gender)
