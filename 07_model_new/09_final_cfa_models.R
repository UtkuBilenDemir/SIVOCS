# LAST SALVO STARTS HERE:
source("./07_model_new/08_miroboard_models.R")

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

saveRDS(fit.final_model2, "./07_model_new/fit.final_model2.Rds")
saveRDS(df_model2, "./07_model_new/df_model2.Rds")

fit2 <- semTable::semTable ( fit.final_model2 , file =
                         file.path ( "./07_model_new/outputs/" , "fit" ) ,
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


## semPaths(fit.final_model2,
##          what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters
##          whatLabels = "est", # This argument controls what the edge labels represent. In this case, parameter estimates
##          style = "lisrel", # This will plot residuals as arrows, closer to what we use in class
##          residScale = 8, # This makes the residuals larger
##          theme = "colorblind", # qgraph colorblind friendly theme
##          nCharNodes = 0, # Setting this to 0 disables abbreviation of nodes
##          # manifests = paste0("Q",1:10), # Names of manifests, to order them appropriatly.
##          reorder = FALSE, # This disables the default reordering
##          # nodeNames = nodeNames, # Add a legend with node names
##          legend.cex = 0.5, # Makes the legend smaller
##          rotation = 2, # Rotates the plot
##          layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
##          cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
##          curvePivot = TRUE, # Changes curve into rounded straight lines
##          sizeMan = 4, # Size of manifest variables
##          sizeLat = 10, # Size of latent variables
##          mar = c(2,5,2,5.5), # Figure margins
##          filetype = "pdf", width = 8, height = 6, filename = "SIVOCS_path_diag" # Save to PDF
## )


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
?sem_tables()

sem_fitmeasures(fit.final_model2)
sem_factorloadings(fit.final_model2)







