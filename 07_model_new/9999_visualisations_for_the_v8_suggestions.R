source("./07_model_new/07_metadata_SI-index_relations.R")


trans_df <- data.frame(
trans = feat_df.num$transdisciplinaryExp.rate.
, domain = meta_df$domain
)

# trans_df <- trans_df[!is.na(trans_df$trans), ]

 trans_df$trans_cat <- ifelse(is.na(trans_df$trans),"na",
                              ifelse(trans_df$trans < 4, "0 - 3",
         ifelse(trans_df$trans < 7, "4 - 6",
                "7 - 10"
                )
         )
 )


trans_freq <- trans_df %>%
  group_by(trans_cat, domain) %>%
  summarise(n())


# -> Gen domain freq table with contribToSI

contrib_df <- data.frame(
contrib = feat_df.num$contribToSI.rate.
, domain = meta_df$domain
)

# trans_df <- trans_df[!is.na(trans_df$trans), ]

 contrib_df$contrib_cat <- ifelse(is.na(contrib_df$contrib),"na",
                              ifelse(contrib_df$contrib < 4, "0 - 3",
         ifelse(contrib_df$contrib < 7, "4 - 6",
                "7 - 10"
                )
         )
 )


contrib_freq <- contrib_df %>%
  group_by(contrib_cat, domain) %>%
  summarise(n())

colnames(df_model)


