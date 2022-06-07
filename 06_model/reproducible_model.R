## SIVOCS Model
# This example only includes the theory driven model
# , the other ones will not be included as long as theory driven model found satisfactory

## -- setup --------------------------------------------------------------------
library(psych)
library(plotly)
library(lavaan)
library(tidyverse)
library(tidyr)
library(nFactors)

data <- read.csv("./mod_fata.csv")
fata.questions <- read.csv( "./mod_question_data.csv")
fata.questions <- read.csv( "./mod_question_fata.csv")
feat_df.num <- read.csv("./fata_num_questions.csv")

source("./question_groups.R")
# Load the feature importances from python script
feature_list <- read.csv("./PFA_feature_importance.csv")

knitr::opts_chunk$set(echo = FALSE, 
                      message=FALSE, 
                      warning=FALSE
                      )


## -- Feature importances------------------------------------------------------------
feature_list$X0 <- factor(feature_list$X0, levels = rev(feature_list$X0))
ggplot(feature_list, aes(x = X1, y = X0))+
  geom_col( width = 0.7)

## -- Features to remove ----------------------------------------------------
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
                    "contribToSI.rate."
                    )

# -> remove the weak features
df_red <- feat_df.num_o[, !(colnames(feat_df.num_o ) %in% features_to_rm)]


##-- Scree ---------------------------------------------------------------------
ev <- eigen(cor(df_red)) # get eigenvalues

ap <- parallel(subject=nrow(df_red),var=ncol(df_red),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors



## ----------------------------------------------------------------------------
# --- FA explanatory
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 8 factors, 
# with varimax rotation 
fit <- factanal(df_red, 8, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

## -- Model to feed into CFA ---------------------------------------------------

model_theory <-"
  ## SI Familiarity
  fam =~ familiarWithSI.response.+transdisciplinaryExp.rate.
  
  ## intention_agency
  ia_human_condition =~ motivation.welfare.+benefitForNonAcademy+impulseForNonAcad.soc.+targetGroupsGoals.improve.+impulseForNonAcad.health.+impulseForNonAcad.ecol.
  
  ia_non_academic =~ impulseForNonAcad.econ.+impulseForNonAcad.tech.
  
  ## transdisciplinary_apects
  transdisciplinary_social =~ groupsInvolved.citiz.+groupsInvolved.civsoc.+groupsInvolved.welfare.+natureOfInvolvement.citiz.+natureOfInvolvement.civsoc.+natureOfInvolvement.welfare.+targetGroupsGoals.socneeds.+targetGroupsGoals.socgroups.+targetGroupsGoals.empower.+targetGroupsGoals.diversity.
  
  ## outcome
  outcome_public =~ impactTargetGroup.pub.+impactTargetGroup.socgr.+impactTargetGroup.welfare.+impactTargetGroup.civsoc.+kindOfChange.pub.+kindOfChange.socgr.+kindOfChange.welfare.+kindOfChange.civsoc.
  
  outcome_statement =~ Impactstatements.capab.+Impactstatements.emanc.+Impactstatements.understanding.+Impactstatements.mitig.+Impactstatements.unknown.+Impactstatements.unaddressed.
  
  ## MISC:scalability
  scale =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  
  ## MISC:policy
  policy =~ groupsInvolved.policy.+impactTargetGroup.policy.+kindOfChange.policy.+natureOfInvolvement.policy.+groupsInvolved.policy.+adoptByPolicyHow.SQ001. 
  
  ## MISC:business
  busi =~ groupsInvolved.busi.+impactTargetGroup.busi.+kindOfChange.busi.
"


## ----------------------------------------------------------------------------
fit_theory <- cfa(model_theory, df_red)
summary(fit_theory, fit.measures = T, standardized = T)

## -- Data Fitting -------------------------------------------------------------
# -> Fit the data frame into the model
DF.refined <- data.frame(predict(fit_theory))

# -> Get row means of the fitted model and scale in aligment with the majority
# of the variables (and self-assessment SI-Rate)
SI.fit_scale <- scales::rescale(apply(DF.refined, mean, MARGIN = 1), to = c(0, 10))
SI_rate.self <- feat_df$contribToSI.rate.

# -> Create a df for fitted and self assessed SI-Rates
si_df <- data.frame(
  fitted = SI.fit_scale,
  self   = SI_rate.self
  ) 
# Turn self assesment into an ordinal with 3 levels
si_df.likert <- si_df
si_df.likert$self <- likert_recode(feat_df.num$contribToSI.rate.)

# -> Vis. the relation
col_seq <- c("#f4a582",
"#92c5de",
"#2166ac")

SI_corr_vis <- si_df.likert %>% 
  filter(!is.na(self)) %>%
  gather(key="fitted", value="self") %>%
  ggplot(aes(x=fitted, y=self, fill=self),  show.legend = FALSE) +
    geom_boxplot() +
  ylab(" ") +
  theme_light() +
  theme(axis.text.y=element_blank()) +
  scale_fill_discrete(name = "SI Rate (self-assessment)") + 
  xlab("Theory driven SI-Index") + 
  scale_fill_manual(values=col_seq, name="SI Rate (self-assessment)") + 
guides(fill=guide_legend(reverse = TRUE))

SI_corr_vis



## -- Corr. between the fitted index and SI-rate -------------------------------
comp_cor <- cor(si_df$fitted, si_df$self, use = "pairwise.complete.obs", method = "spearman")



# Change the confidence interval fill color
ggplot(si_df, aes(y=self, x=fitted)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="blue") + 
  geom_text( aes( x=0, y=9, label=paste0("Cor: " , round(comp_cor, 2))),                 , 
           color="black", 
           size=4, fontface="bold" )

