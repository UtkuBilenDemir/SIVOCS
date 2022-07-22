rm(list = ls())

library(ggplot2)
library(ggbiplot)
library(psych)

# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
#source("./02_analysis/99_question_groups.R")

# -------------------- PCA

# Are there completely NA colums?
sum(apply(FUN = sum,
          MARGIN = 2,
          apply(FUN = is.na,
                MARGIN = 2,
                data.num_questions)) == nrow(data.num_questions
                )
    )

pca_model <- prcomp(na.omit(data.num_questions),
                    scale = TRUE,
                    center = TRUE)
#plot(pca_model$x[, 1], pca_model$x[, 2])


# How much variation in each component
pca_model.var <- pca_model$sdev^2
pca_model.var_per <- cumsum(pca_model.var)/sum(pca_model.var)

barplot(pca_model.var_per,
        main = "Scree Plot")


# A more meaningful visualisation of PCA
pca_model.data <- data.frame(Sample = row.names(pca_model$x), 
                                                X = pca_model$x[, 1], 
                                                Y = pca_model$x[, 2]
                             )

ggplot(data = pca_model.data, 
       aes(x = X, y = Y, label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", round(pca_model.var_per[1], 2), "%", sep = "")) +
  ylab(paste("PC2 - ", round(pca_model.var_per[2], 2), "%", sep = "")) +
  theme_bw() +
  ggtitle("First 2 components")

# Visualisation of all components
ggbiplot(pca_model)

# Most important features
loading_scores <- abs(pca_model$rotation[, 1])
loading_scores.ranked <- sort(loading_scores, decreasing = TRUE)
top_10_features <- loading_scores.ranked[1:10]

top_10_features


# -------------------- Factor Analysis

parallel <- fa.parallel(data.num_questions,
                        fm = "minres",
                        fa = 'fa')
cumsum(parallel)

factors <- fa(data.num_questions, 
              nfactors = 10, 
              rotate = 'oblimin', 
              fm = 'minres')
print(factors)


as.vector(rownames(factors$loadings))[(factors$loadings[, "MR10"] > 0.4)]

library(lavaan)

model <- '
F1 =~ groupsInvolved.civsoc.+groupsInvolved.citiz.+groupsInvolved.welfare.+targetGroupsGoals.socneeds.+targetGroupsGoals.socgroups.+targetGroupsGoals.empower.
F2 =~ adoptByPolicy.rate.+Impactstatements.capab.+Impactstatements.emanc.+Impactstatements.understanding.+Impactstatements.mitig.+Impactstatements.unknown.+Impactstatements.unaddressed.
F3 =~ concepts.pub.+concepts.data.+concepts.code.+concepts.infra.+dissChannels.trad.+dissChannels.web.+dissChannels.platf.
F4 =~ targetGroupsGoals.diversity.
F5 =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
F6 =~ groupsInvolved.policy.+impactTargetGroup.policy.+dissChannels.policy.
F7 =~ transdisciplinaryExp.rate.+groupsInvolved.busi.+impactTargetGroup.pub.+impactTargetGroup.busi.
F8 =~ dissChannels.conf.
F9 =~ contribToSI.rate.
F10 =~ motivation.pheno.+motivation.prob.
'

fit <- cfa(model, data = sapply(FUN=scale, na.omit(data.num_questions)) )
summary(fit, fit.measures=TRUE, standardized=TRUE)


