source("./02_analysis/02_static_responses.R")

df_ult <- readRDS("df_ult.RDS")
data[data == 99] <- NA
data[data == 88] <- NA
int_data <- data
int_data$it <- e12.trans_df$inv.trans

# TODO: Consider including all high SI Knowledge people

rm_ids <- function (df1, df2) {
  cat(df2$projID, " are removed")
  out <- df1[!c(df1$projID %in% df2$projID), ]
  rownames(out) <- 1:nrow(out)
  return(out)
}
# Remove the existent interview candidates
int_data <- int_data[-c(which(int_data$projID %in% df_ult$projID)),] 
rownames(int_data) <- 1:nrow(int_data)

# --- Low mot vs. high contrib
df.low_mot_high_contrib <- int_data[int_data$benefitForNonAcademy == 0 &
           int_data$contribToSI.rate. >= 6, 
]

df.low_mot_high_contrib$reasoning <- "Low motiv. to benefit for society - High SI contrib."

#--------------------------------------------------0
df.out <- df.low_mot_high_contrib
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)


df.out <- df.out[!is.na(df.out$id), ]

# --- High outcome - low trans. involvement of citizens
df.high_outcome_low_trans <- int_data[int_data$impactTargetGroup.pub. >= 7 &
                             int_data$groupsInvolved.citiz. < 1 & 
                             int_data$groupsInvolved.welfare. < 1 & 
                             int_data$groupsInvolved.media. < 1 & 
                             int_data$groupsInvolved.policy. < 1 & 
                             int_data$groupsInvolved.civsoc.< 1 
                             , ]

df.high_outcome_low_trans$reasoning <- "Low trans. involvement - High public outcome"
nrow(df.high_outcome_low_trans)
#--------------------------------------------------0.1
df.out <- rbind(df.out, df.high_outcome_low_trans)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

nrow(df.out)

# --- HIGH SI contirb vs LOW trans involv. (USELESS)
ccc <- int_data[int_data$it <= 1 & int_data$contribToSI.rate. >= 6, ]
which(!is.na(ccc$id))

#-------------------------------------------------- 4
ccc$reasoning <- "LOW trans involv. - HIGH SI contirb "
df.out <- rbind(df.out,  ccc)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]





#############################################
# --- low motivation - 


low_mot_df <- int_data[int_data$motivation.welfare. <= 3, ]
low_mot_df <- low_mot_df[!is.na(low_mot_df$id),]
write.csv(low_mot_df, "low_mot_df.csv")

low_mot_high_imp <- low_mot_df[ low_mot_df$Impactstatements.capab. >= 7 | 
     low_mot_df$Impactstatements.emanc. >= 7 |  
     low_mot_df$Impactstatements.understanding. >= 7   , ]
      
#-------------------------------------------------- -1
low_mot_high_imp$reasoning <- "Low motivation to improve human condition - high impact statement on emancipation/ capability/ understanding"
df.out <- rbind(df.out,  low_mot_high_imp)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

nrow(df.out)

# --- Low fam SI - High Social Impact
low_fam <- int_data[int_data$familiarWithSI.response. <= 3, ]
write.csv(low_fam, "low_fam.csv")

low_fam_high_imp <- low_fam[(low_fam$Impactstatements.emanc. >= 7 |
         low_fam$Impactstatements.mitig. >= 7 |
         low_fam$Impactstatements.capab. >= 7
           
),
          ]


sum(!is.na(low_fam_high_imp$id))

#-------------------------------------------------- -1
low_fam_high_imp$reasoning <- "Low SI familiarity - - high impact statement on emancipation/ capability/ understanding"
df.out <- rbind(df.out,  low_fam_high_imp)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

nrow(df.out)




















#++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++



# --- High SI Knowledge low contribution to SI
df.familiar_SI <- int_data[int_data$familiarWithSI.response. >= 7, ]
nrow(df.familiar_SI)

df.familiar_SI_low_contrib <- df.familiar_SI[df.familiar_SI$contribToSI.rate. < 3, ][c(4,5),]
df.familiar_SI_low_contrib$reasoning <- "High SI familiarity - Low SI contribution"

#--------------------------------------------------1
df.out <- rbind(df.out, df.familiar_SI_low_contrib)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)


# Remove the selected ones
# --- High Motivation - Low public outcome 
df.high_mot <- int_data[int_data$motivation.welfare. > 7, ]
# low public impact
df.high_mot_low_pubImpact <- df.high_mot[df.high_mot$impactTargetGroup.pub. <= 3 & 
                                           df.high_mot$impactTargetGroup.socgr. <= 3 &
                                           df.high_mot$impactTargetGroup.welfare. <= 3 &
                                           df.high_mot$impactTargetGroup.busi. <= 3 &
                                           df.high_mot$impactTargetGroup.civsoc. <= 3 &
                                           df.high_mot$impactTargetGroup.policy. <= 3 
                                           , ]


nrow(df.high_mot_low_pubImpact)

#-------------------------------------------------- 2
df.high_mot_low_pubImpact$reasoning <- "High mot. to imp. human condition - Low impact (all cat. except academia)"
df.out <- rbind(df.out,  df.high_mot_low_pubImpact)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

# --- Transdisciplinary involvment - low public impact
df.high_trans_low_pubImpact <- int_data[int_data$groupsInvolved.citiz. == 2 & 
                    #int_data$Impactstatements.emanc. <=3
                    int_data$impactTargetGroup.pub. <=3
                    , ]

df.high_trans_low_pubImpact$reasoning <- "Transdisciplinary Involvement - Low public impact"
df.out <- rbind(df.out,  df.high_trans_low_pubImpact)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

# --- Impulse from academic world: a societal problem - Low social impact
df.high_impuls_low_socImpact <- int_data[int_data$impulseForNonAcad.soc. == "Y" & 
      int_data$Impactstatements.capab. <= 3 &
      int_data$Impactstatements.emanc. <= 3 &
      int_data$impactTargetGroup.pub. <= 3 &
      int_data$Impactstatements.mitig. <= 3 
    , ]

#-------------------------------------------------- 3
df.high_impuls_low_socImpact$reasoning <- "Impulse from a societal problem - Low social impact"
df.out <- rbind(df.out,  df.high_impuls_low_socImpact)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]



# --- Obvious cases
bbb <- int_data[int_data$contribToSI.rate. >= 7, ]

#-------------------------------------------------- 3
bbb$reasoning <- "(Obvious Cases) High SI familiarity"
df.out <- rbind(df.out,  bbb)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]


aaa <- int_data[int_data$familiarWithSI.response. >= 7, ]
aaa[aaa$motivation.welfare. >= 7, ]

ncol( bbb)

#-------------------------------------------------- 3
aaa$reasoning <- "(Obvious Cases) High SI Familiarity"
df.out <- rbind(df.out,  aaa)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

nrow(df.out)



# --- Low trans
ddd <- int_data[int_data$targetGroupsGoals.empower. == 1 &
                  int_data$targetGroupsGoals.improve. == 1 &
                  int_data$Impactstatements.emanc. <=3 &
                  int_data$Impactstatements.mitig. <=3
                , ]


sum(!is.na(ddd$id))

#############################################
# --- Overall high scored respondents


high_contrib_df <- int_data[int_data$contribToSI.rate. > 7, ]
high_contrib_df <- high_contrib_df[!is.na(high_contrib_df$id),]

#-------------------------------------------------- -1
high_contrib_df$reasoning <- "Overall high SI stats"
df.out <- rbind(df.out,  high_contrib_df)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]


#############################################
# --- high motivation - caused behaviour change


high_mot_df <- int_data[int_data$motivation.welfare. > 7, ]
high_mot_df <- high_mot_df[!is.na(high_mot_df$id),]
write.csv(high_mot_df, "high_mot_df.csv")


high_mot_beh_df <- high_mot_df[high_mot_df$kindOfChange.pub. == "beh" |
     high_mot_df$kindOfChange.policy. == "beh" |
      high_mot_df$kindOfChange.socgr. == "beh" |
      high_mot_df$kindOfChange.welfare. == "beh" |
      high_mot_df$kindOfChange.civsoc. == "beh", ]

#-------------------------------------------------- -1
high_mot_beh_df$reasoning <- "High motivation to imp. human condition + changed behaviour (public, policy, civil society orgs)"
df.out <- rbind(df.out,  high_mot_beh_df)
rownames(df.out) <- 1:nrow(df.out)
int_data <- rm_ids(int_data, df.out)
rownames(int_data) <- 1:nrow(int_data)

df.out <- df.out[!is.na(df.out$id), ]

nrow(df.out)

#--- Perfect cases
int_data[int_data$motivation.welfare. >= 7 & int_data$groupsInvolved.citiz. >= 1 & int_data$impactTargetGroup.welfare. >= 7,]


df.out <- df.out[, c(1:5, 154, 6:153)] 
write.csv(df.out, "deneme_batch2.csv")

