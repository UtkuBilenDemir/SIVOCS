library(readxl)


cand_df <- as.data.frame(read_excel("./SIVOCS_interview_candidates__batch1.xls"))
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



out_df <- as.data.frame(cbind(cand_df[, 1:12], 
                              url_col, 
                              match_grant_df,
                              match_person_df,
                              match_collab_df, 
                              match_pub_df,
                              match_output_df,
                              cand_df[, 13:109]))






out_df <- out_df[, c(2:8, 24, 9:10, 19, 20, 11, 18, 14:16, 21, 22, 12, 25, 26, 27, 28, 29:42, 47, 49, 50, 51, 52, 13, 53:ncol(out_df))] 


colnames(out_df)[39] <- "Journal.Abstract"  
colnames(out_df)[22] <- "Collaboration.Country"  
colnames(out_df)[44] <- "Survey.URL"  
colnames(out_df)[42] <- "Output.URL"  
colnames(out_df)[43] <- "Output.Year"  

  
out_df <- out_df[, c(1:17, 20:ncol(out_df))] 
colnames(out_df)[26] <- "Book.Authors"
colnames(out_df)[27] <- "Publication.Status"

colnames(out_df[, c(1:25, 27:32, 26, 33:ncol(out_df))])

out_df <- out_df[, c(1:25, 27:32, 26, 33:ncol(out_df))] 

write.csv(out_df, "cand2.csv")

