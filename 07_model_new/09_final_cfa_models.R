# LAST SALVO STARTS HERE:
source("./07_model_new/08_miroboard_models.R")

# Collection of the variables that doesn't fit well:
# impulseForNonAcad.tech.+
# impulseForNonAcad.ecol.+
# impulseForNonAcad.health.+
# impulseForNonAcad.econ.+
# motivation.prob.+
# ---

# model_miro6
final_model1<- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.+
Impactstatements.unknown.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.

outputs_outcomes =~
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# miro_model7
final_model2 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.+
Impactstatements.unknown.+
Impactstatements.unaddressed.

outputs_outcomes =~
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.
"



fit.final_model1 <- cfa(
  final_model1
  , data = df_model2
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )


fit.final_model2 <- cfa(
  final_model2
  , data = df_model2
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )


summary(
  fit.final_model2
  , fit.measures = T
  , standardized = T
  , rsq = T
  ) 

# -> FIT both models
pred1 <- predict(fit.final_model1)
pred2 <- predict(fit.final_model2)

# -> Generate meaningful dataframes
pred_df1 <- as.data.frame(
  apply(
    apply(
      pred1
      , MARGIN = 2
      , scales::rescale
      , c(0,10))
    , MARGIN = 2
    , round
    , 2
    )
  )

pred_df2 <- as.data.frame(
  apply(
    apply(
      pred2
      , MARGIN = 2
      , scales::rescale
      , c(0,10))
    , MARGIN = 2
    , round
    , 2
    )
  )




