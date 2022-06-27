source("./07_model_new/14_multinomial_logit.R")

miro_features <- c(
"motivation.welfare."
, "benefitForNonAcademy"
, "impulseForNonAcad.soc."
, "targetGroupsGoals.improve."
, "targetGroupsGoals.empower."
, "Impactstatements.unknown."
, "groupsInvolved.civsoc."
, "groupsInvolved.policy."
, "groupsInvolved.citiz."
, "groupsInvolved.welfare."
, "natureOfInvolvement.civsoc."
, "natureOfInvolvement.policy."
, "natureOfInvolvement.citiz."
, "natureOfInvolvement.welfare."
, "targetGroupsGoals.socneeds."
, "targetGroupsGoals.socgroups."
, "scalabilityRating.out."
, "scalabilityRating.up."
, "targetGroupsGoals.diversity."
, "concepts3"
, "impactTargetGroup.pub."
, "impactTargetGroup.socgr."
, "impactTargetGroup.welfare."
, "impactTargetGroup.civsoc."
, "impactTargetGroup.policy."
, "kindOfChange.pub."
, "kindOfChange.socgr."
, "kindOfChange.welfare."
, "kindOfChange.civsoc."
, "kindOfChange.policy."
, "adoptByPolicy.rate."
, "Impactstatements.mitig."
, "Impactstatements.understanding." 
, "Impactstatements.emanc."
, "Impactstatements.capab."
)

setdiff(miro_features, colnames(df_red))

df_red.miro <- as.data.frame(
  cbind(
    df_red
    , feat_df.num_o[, c("scalabilityRating.out."
                        , "scalabilityRating.up."
                        ,  "concepts3"
                        , "adoptByPolicy.rate."
                        )
                    ]
    )
  ) 

df_miro <- df_red.miro[, miro_features]

#---
# -> TEST with efa
#---

# Scree Plot
#-------------------------------------------------------------------------------
ev <- eigen(cor(df_miro)) # get eigenvalues

ap <- parallel(
  subject=nrow(df_miro)
  , var=ncol(df_miro)
  , rep=1000
  , cent=.05
  )
nS <- nScree(
  x=ev$values
  , aparallel = ap$eigen$qevpea
  )
plotnScree(nS)  # 9 Factors

df_red.rescaled <- sapply(df_miro, scales::rescale, c(0,10))
df_red.rescaled <- na.omit(df_red.rescaled)
as.data.frame(df_red.rescaled)
efa_model <- fa.parallel(df_red.rescaled, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors

# EFA with different number of factors
#-------------------------------------------------------------------------------

four_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 4)
print(four_factor, cut = .4, digits = 2)

five_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 5)
print(five_factor, cut = .4, digits = 2)
?fa
#-------------------------------------------------------------------------------
# -> when we reduce even more?


miro_features2 <- c("motivation.welfare."
, "benefitForNonAcademy"
, "impulseForNonAcad.soc."
, "targetGroupsGoals.improve."
, "groupsInvolved.civsoc."
, "groupsInvolved.citiz."
, "natureOfInvolvement.civsoc."
, "natureOfInvolvement.citiz."
, "Impactstatements.emanc."
, "Impactstatements.capab."
, "targetGroupsGoals.socneeds."
, "targetGroupsGoals.socgroups."
, "targetGroupsGoals.empower."
, "targetGroupsGoals.diversity."
, "scalabilityRating.out."
, "scalabilityRating.up."
, "Impactstatements.unknown."
, "Impactstatements.unaddressed."
, "kindOfChange.pub."
, "concepts3"
, "impactTargetGroup.pub."
, "impactTargetGroup.socgr."
, "impactTargetGroup.civsoc."
, "adoptByPolicy.rate."
, "Impactstatements.mitig."
, "Impactstatements.understanding."
)

df_miro2 <- df_red.miro[, miro_features2]

# Scree Plot
#-------------------------------------------------------------------------------
ev <- eigen(cor(df_miro2)) # get eigenvalues

ap <- parallel(
  subject=nrow(df_miro2)
  , var=ncol(df_miro2)
  , rep=1000
  , cent=.05
  )
nS <- nScree(
  x=ev$values
  , aparallel = ap$eigen$qevpea
  )
plotnScree(nS)  # 9 Factors

df_red.rescaled <- sapply(df_miro2, scales::rescale, c(0,10))
df_red.rescaled <- na.omit(df_red.rescaled)
as.data.frame(df_red.rescaled)
efa_model <- fa.parallel(df_red.rescaled, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors


four_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 4)
print(four_factor, cut = .4, digits = 2)

five_factor <- fa(df_red.rescaled, "varimax", fm = "ml", nfactors = 5)
print(five_factor, cut = .4, digits = 2)

setdiff(colnames(df_red2), miro_features2 )

library(stats)
library(psych)


bartlett.test(df_model2)
KMO(df_model2)
