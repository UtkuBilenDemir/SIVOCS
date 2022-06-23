## setwd("../../utku_SIVOCS/")
source("./02_analysis/02_static_responses.R")
source("./02_analysis/99_question_groups.R")
# Load the feature importances from python script
feature_list <- read.csv("./02_analysis/PFA_feature_importance.csv")

# Feature Removal by PFA
#-------------------------------------------------------------------------------
features_to_rm <- tail(feature_list$X0, 40)
# Qualitatively select the worse performing features
features_to_rm <- features_to_rm[c(22,26,29:40)]
features_to_rm <- c(features_to_rm, 
                    "dissChannels.trad.",
                    "dissChannels.socmed.",
                    "dissChannels.consult.",
                    "dissChannels.events.", 
                    "dissChannels.public.",
                    "concepts.data.",
                    "concepts.code.",
                    "concepts.infra.",
                    "contribToSI.rate.",
                    "groupsInvolved.res.",
                    "natureOfInvolvement.res.",
                    "contribToSI.rate.",
                    "concepts2"
                    )

# -> remove the weak features
df_red <- feat_df.num_o[, !(colnames(feat_df.num_o ) %in% features_to_rm)]


# Further feature removal (NEW) 
#-------------------------------------------------------------------------------

## SI Familiarity Factor
factor_1 <- c("familiarWithSI.response.", "transdisciplinaryExp.rate.")
df_red <- df_red %>%
 dplyr::select(-all_of(factor_1))

## MISC:business
factor_busi <- c("groupsInvolved.busi.", "impactTargetGroup.busi.", "kindOfChange.busi.")
df_red <- df_red %>%
  dplyr::select(-all_of(factor_busi))


## MISC:scalability
factor_scale <- c("scalabilityRating.up.", "scalabilityRating.out.", "scalabilityRating.deep.")
df_red <- df_red %>%
  dplyr::select(-all_of(factor_scale))


## Academic motivation, we are not interested
acad_mot <- c("motivation.pheno.")
df_red <- df_red %>%
  dplyr::select(-all_of(acad_mot))

colnames(df_red)

# Contribution to SI is a control variable
#-------------------------------------------------------------------------------


df_model <- feat_df.num_o %>%
  dplyr::select(-all_of(c(
    factor_1
    , "contribToSI.rate."
    , "motivation.pheno."
    , "scalabilityRating.up."
    , "scalabilityRating.out."
    , "scalabilityRating.deep."
    , "groupsInvolved.res."
    , "concepts.pub."
    , "concepts.data."
    , "concepts.code."
    , "concepts.infra."          
    , "concepts.review."
    , "concepts2"
    , "concepts3"
    )))
ncol(df_model)

setdiff(colnames(df_model), colnames(df_red))
