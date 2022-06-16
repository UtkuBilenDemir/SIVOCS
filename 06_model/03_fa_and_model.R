library(psych)
library(GPArotation)
library(plotly)
library(lavaan)
library(scales)
library(tidyverse)
library(dplyr)

## setwd("../../utku_SIVOCS/")
source("./02_analysis/02_static_responses.R")
source("./02_analysis/99_question_groups.R")
# Load the feature importances from python script
feature_list <- read.csv("./02_analysis/PFA_feature_importance.csv")

# Feature Removal
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

# remove the weak features
df_red <- feat_df.num_o[, !(colnames(feat_df.num_o ) %in% features_to_rm)]


# Further feature removal
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

colnames(df_red)

#-------------------------------------------------------------------------------


df_model <- feat_df.num_o %>%
  dplyr::select(-all_of(c(factor_1, "contribToSI.rate.")))

colnames(df_model)


# Scree Plot
#-------------------------------------------------------------------------------
library(nFactors)
df_red2 <- df_red[, !colnames(df_red) %in% c("motivation.pheno.")] # Removing this one doesn't change anything
ev <- eigen(cor(df_red2)) # get eigenvalues

ap <- parallel(subject=nrow(df_red2), var=ncol(df_red2),
               rep=1000,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors

df_red.rescaled <- sapply(df_red2, rescale, c(0,10))
df_red.rescaled <- na.omit(df_red.rescaled)
efa_model <- fa.parallel(df_red.rescaled, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors


# EFA with different number of factors
#-------------------------------------------------------------------------------
four_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 4)
five_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 5)
six_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 6)
ten_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 9)

anova(four_factor, five_factor, six_factor)

summary(four_factor)
summary(five_factor)
summary(six_factor)
summary(ten_factor)

# Tucker Lewis should be ~ 0.9, we have lower
# RMSA is sufficient with 0.05
# Factors can be corrrelated with each other up to 0.75



#-------------------------------------------------------------------------------
print(six_factor, cut = .3, digits = 2)
plot(six_factor)
cor.plot(six_factor)


# CFA Models
#-------------------------------------------------------------------------------

cfa_five <-
# Intention
"
a =~motivation.welfare.+
benefitForNonAcademy+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
impulseForNonAcad.soc.+
targetGroupsGoals.socgroups.+
natureOfInvolvement.civsoc.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

## Intention-other
b =~ motivation.prob.+
impulseForNonAcad.ecol.+
impulseForNonAcad.health.+
impulseForNonAcad.tech.+
impulseForNonAcad.econ. 


# Actors
c =~ groupsInvolved.citiz.+
natureOfInvolvement.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.welfare.+
groupsInvolved.civsoc.+
natureOfInvolvement.civsoc.+
kindOfChange.civsoc. 

d =~ kindOfChange.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.acad.+
kindOfChange.welfare.

e =~ Impactstatements.capab.+
Impactstatements.emanc.+
Impactstatements.understanding.+
Impactstatements.mitig.+
Impactstatements.unknown.+
Impactstatements.unaddressed.+
impactTargetGroup.socgr.+
impactTargetGroup.policy.+
impactTargetGroup.civsoc.
"

model_theory2 <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  impulseForNonAcad.soc.+ 
  targetGroupsGoals.improve. 
  
  ia_other_nonacad_impulse =~ 
  impulseForNonAcad.econ.+ 
  impulseForNonAcad.ecol.+
  impulseForNonAcad.health.+ 
  impulseForNonAcad.tech.
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~ 
  groupsInvolved.citiz. + 
  groupsInvolved.civsoc. + 
  groupsInvolved.welfare. + 
  natureOfInvolvement.citiz. + 
  natureOfInvolvement.civsoc. + 
  natureOfInvolvement.welfare.
  
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.
  
  # NOVELTY 
  innovativeness =~
  kindOfChange.pub. + 
  kindOfChange.socgr. + 
  kindOfChange.welfare. + 
  kindOfChange.civsoc.
  
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.welfare. + 
  impactTargetGroup.civsoc. + 
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig. + 
  Impactstatements.unknown. + 
  Impactstatements.unaddressed.
  
  # MISC:policy
  policy =~ groupsInvolved.policy.+impactTargetGroup.policy.+kindOfChange.policy.+natureOfInvolvement.policy.+groupsInvolved.policy.+adoptByPolicyHow.SQ001."



model_theory3 <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  targetGroupsGoals.improve. 
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~ 
  groupsInvolved.citiz. + 
  groupsInvolved.civsoc. + 
  groupsInvolved.welfare. + 
  groupsInvolved.policy.+
  natureOfInvolvement.citiz. + 
  natureOfInvolvement.civsoc. + 
  natureOfInvolvement.policy.+
  natureOfInvolvement.welfare.
  
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.
  
  # NOVELTY 
  innovativeness =~
  kindOfChange.pub. + 
  kindOfChange.socgr. + 
  kindOfChange.policy.+
  kindOfChange.welfare. 
  
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.welfare. + 
  impactTargetGroup.civsoc. + 
  impactTargetGroup.policy.+
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig. + 
  Impactstatements.unknown. + 
  Impactstatements.unaddressed.
"

# Scale also df_model
df_model <- na.omit(df_model)
df_model <- sapply(FUN=rescale, df_model, c(0, 10))
fit.model2 <- cfa(model_theory3, data = df_model, estimator = "MLR", mimic = "Mplus")
# MLR because our data is not normally distributed


# Factor loadings should be > 0.7, remove everything below 0.6 (it is Std.all)
# CFI, TLI > 0.9
# RMSEA, SRMR < 0.08
summary(fit.model2, fit.measures = T, standardized = T, rsq = T)  # Not a good fit

pred <- predict(fit.model2)
## pred <- t(pred)

df_pred <- apply(apply(pred,MARGIN = 2, rescale, c(0,10)), MARGIN = 2, round, 2)
df_pred <- as.data.frame(df_pred)

#-------------------------------------------------------------------------------
lm_fitted <- lm(outcomes ~ 
     ia_human_condition + 
     transdisciplinary_involvement +
     transdisciplinary_goals +
     innovativeness,
   df_pred
     )
summary(lm_fitted)

#-------------------------------------------------------------------------------
si_index <- rowMeans(df_pred)


# Split into 2 groups
inds <- sample(2, nrow(df_pred), replace = T, prob = c(0.8, 0.2))
train <- df_pred[inds == 1, ]
test <- df_pred[inds == 2, ]

library(MASS)
colnames(df_pred)
ord_log_model <- polr(
  as.factor(outcomes) ~ 
    ia_human_condition + 
    transdisciplinary_involvement +
    transdisciplinary_goals +
    innovativeness,
  train,
  Hess = T
  )
summary(ord_log_model)


(ctable <- coef(summary(ord_log_model)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind("p value" = p))


train[1:5, "outcomes"]
pred <- predict(ord_log_model, train[1:5, ], type = "prob")
print(pred, digits = 2)
colnames(pred)[apply(pred, 1,which.max)]


pred2 <- predict(ord_log_model, test[1:5, ], type = "prob")
print(pred2, digits = 2)
colnames(pred2)[apply(pred2, 1,which.max)]

pred3 <- predict(ord_log_model, train)
(tab <- table(pred3, train$outcomes))
1- sum(diag(tab))/sum(tab)
babap <- as.data.frame(tab)


#-------------------------------------------------------------------------------
# LOGIT

# -> Create an index where the (fitted) features are used incrementally
# to apply a binary SI classification

# a new df
df_fitted <- df_pred
df_colnames <- colnames(df_pred)
df_fitted <- as.data.frame(df_fitted)
df_fitted$id <- 1:nrow(df_fitted)

str(df_fitted)


# These are what we call socially innovative "projects"
# Incrementally higher than 3 in all of the fitted features
si_ids <- df_fitted %>%
  dplyr::filter(.[[df_colnames[1]]] > 3) %>%
  dplyr::filter(.[[df_colnames[2]]] > 3) %>%
  dplyr::filter(.[[df_colnames[3]]] > 3) %>%
  dplyr::filter(.[[df_colnames[4]]] > 3) %>%
  dplyr::filter(.[[df_colnames[5]]] > 3) %>%
  dplyr::select(id)
  

# Create a binary feature which classifies between socially innovative 
# and non-innovative projects
df_fitted$si <- 0
df_fitted$si[unlist(si_ids)] <- 1

# Remove the ids column
df_fitted <- df_fitted[, colnames(df_fitted) != "id"]

log_model <- glm(si ~ ., data = df_fitted, family = "binomial")
summary(log_model)

