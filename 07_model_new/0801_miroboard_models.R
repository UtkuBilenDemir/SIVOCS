source("./07_model_new/07_metadata_SI-index_relations.R")

# df_model doesn't include the scalability variables but
# we intend to use 2 of those in the new cfa model
# -> keep the out & up-scalability
# -> remove all purely academic variables

df_model2 <- feat_df.num_o %>%
  dplyr::select(-all_of(c(
    factor_1
    , "contribToSI.rate."
    , "motivation.pheno."
    , "scalabilityRating.deep."
    , "groupsInvolved.res."
    , "impactTargetGroup.acad."
    , "kindOfChange.acad."
    , "concepts.pub."
    , "concepts.data."
    , "concepts.code."
    , "concepts.infra."          
    , "concepts.review."
    , "concepts2"
    ## , "concepts3"
    )))

# a less strict version of the reduced dataframe
df_less_red.rescaled <- as.data.frame(cbind(df_red.rescaled, df_model2[, c("scalabilityRating.out.", "scalabilityRating.up.", "concepts3", "adoptByPolicy.rate.")]))
## df_model <- as.data.frame(df_model)

# An overlook on the correlations
library(corrr)
corr_matrix <- df_model2 %>%
  correlate() %>% 
  stretch()

best_correlations <- corr_matrix[order(corr_matrix$r, decreasing = T),]
write.csv(best_correlations[1:100, ], "./07_model_new/best_correlations.csv")


# Collection of the variables that doesn't fit well:
# impulseForNonAcad.tech.+
# impulseForNonAcad.ecol.+
# impulseForNonAcad.health.+
# impulseForNonAcad.econ.+
# motivation.prob.+
# ---
# groupsInvolved.busi.+
# natureOfInvolvement.media.+
# natureOfInvolvement.busi.+
# ---
# Impactstatements.unknown.+
# Impactstatements.unaddressed.+



miro_model <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
Impactstatements.unknown.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.

outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.+ 
Impactstatements.emanc. +
Impactstatements.capab.
"

# New category as trans_goals_achievements
miro_model2 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
Impactstatements.unknown.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.

outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# targetgroup goals as novelty maybe?
miro_model3 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# No policy
miro_model4 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# No welfare
miro_model5 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# Broader outcome
miro_model6 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.+
Impactstatements.unknown.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.

outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"


fit.miro_model <- cfa(
  miro_model6
  , data = df_model2
  , estimator = "MLR"
  , mimic = "Mplus"
  , std.lv = T
  )

fit.miro_model_ordered <- cfa(
  miro_model6
  , data = df_model2
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )

summary(fit.miro_model_ordered, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.miro_model, fit.measures = T, standardized = T, rsq = T)  # Not a good fit

parameterestimates(fit.miro_model)
fitmeasures(fit.miro_model_ordered)

# Share this in annex 
cov(df_model2)



library(semPlot)
semPaths(fit.miro_model_ordered, what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters
         whatLabels = "est", # This argument controls what the edge labels represent. In this case, parameter estimates
         style = "lisrel", # This will plot residuals as arrows, closer to what we use in class
         residScale = 8, # This makes the residuals larger
         theme = "colorblind", # qgraph colorblind friendly theme
         nCharNodes = 0, # Setting this to 0 disables abbreviation of nodes
         ## manifests = paste0("Q",1:10), # Names of manifests, to order them appropriatly.
         reorder = FALSE, # This disables the default reordering
         ## nodeNames = nodeNames, # Add a legend with node names
         legend.cex = 0.5, # Makes the legend smaller
         rotation = 2, # Rotates the plot
         layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
         cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
         curvePivot = TRUE, # Changes curve into rounded straight lines
         sizeMan = 4, # Size of manifest variables
         sizeLat = 10, # Size of latent variables
         mar = c(2,5,2,5.5), # Figure margins
         filetype = "pdf", width = 8, height = 6, filename = "SIVOCS" #  Save to PDF
)





