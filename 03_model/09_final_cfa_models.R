# LAST SALVO STARTS HERE:
source("./03_model//08_miroboard_models.R")

# NOTES
# ---
# - Explain excluded variables
# - ordered = T is a special computation for the ordinal variables


# Collection of the variables that doesn't fit well:
# impulseForNonAcad.tech.+
# impulseForNonAcad.ecol.+
# impulseForNonAcad.health.+
# impulseForNonAcad.econ.+
# motivation.prob.+
# ---

# initially miro_model6
final_model1 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.

# ACTORS & NETWORKS
an_transdisciplinary_inv =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

an_trandisciplinary_goals =~
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
Impactstatements.unaddressed.+
kindOfChange.pub.+
kindOfChange.socgr.

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

# ACTORS & NETWORKS
an_transdisciplinary_inv =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

an_transdisciplinary_goals =~
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
Impactstatements.unaddressed.+
kindOfChange.pub.


outputs_outcomes =~
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
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
  # , std.lv = T
  , ordered = T
  )

saveRDS(fit.final_model2, "./03_model//fit.final_model2.Rds")
saveRDS(df_model2, "./03_model//df_model2.Rds")

fit2 <- semTable::semTable ( fit.final_model2 , file =
                         file.path ( "./03_model/outputs/" , "fit" ) ,
                        type = "csv" ,
                       print.results = T,
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

library(semPlot)
semPlot::semPaths(fit.final_model2)

lavInspect(fit.final_model2, "cov.lv")
lavInspect(fit.final_model2, "cor.lv")
inspect(fit.final_model2, "cor.lv")

# ------------------------------------------------------------------------------
# Model Tables

library(modelsummary)
library(kableExtra)
library(gt)

models <- list(
  "Model" = fit.final_model2
)

modelsummary(models)
semoutput::sem_tables(fit.final_model2, )
