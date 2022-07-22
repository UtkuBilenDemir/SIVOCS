rm(list = ls())

library(lares)
library(ggplot2)
library(ggbiplot)
library(psych)
# NOTE TO SELF: Remember, the correct and efficient way 
# to that thing you insist to do wrongly is
# as.numeric(levels(f))[f]


# Data Frame
source("./02_analysis/02_static_responses.R")


# PCA with the new variables
# -------------------- PCA

#feat_df.num_omit <- na.omit(feat_df.num)
#null_features.ind <- which(sapply(FUN=sum, feat_df.num) == 0)
#feat_df.num_omit <- feat_df.num_omit[, -c(null_features.ind)]

pca_model <- prcomp(feat_df.num_o,
                    scale = TRUE,
                    center = TRUE)

# How much variation in each component
pca_model.var <- pca_model$sdev^2
pca_model.var_per <- cumsum(pca_model.var)/sum(pca_model.var)

barplot(pca_model.var_per,
        main = "Scree Plot")


# A more meaningful visualisation of PCA
pca_model.data <- data.frame(Sample = pca_model$x, 
                                                X = pca_model$x[, 1], 
                                                Y = pca_model$x[, 2]
                             )

# Visualisation of all components
ggbiplot(pca_model)

# Most important features
loading_scores <- abs(pca_model$rotation[, 2])
loading_scores.ranked <- sort(loading_scores, decreasing = TRUE)
top_10_features <- loading_scores.ranked[1:10]

top_10_features
# PCs explain not much variance, moving on


# Which variables are mostly NA?
apply(FUN = sum, MARGIN = 2, sapply(FUN = is.na, feat_df.num))

# Create a correlation matrix to explore as csv
cor_matrix <- cor(feat_df.num,
    use = "pairwise.complete.obs", 
    method = "spearman"
    )
cor_matrix <- round(cor_matrix, 2)

# save a csv for excel analysis
write.table(cor_matrix, "./03_analysis_outputs/correlation_matrix.csv", sep = "\t")

# !IMPORTANT Most important correlations

# With p-value
corr_cross(feat_df.num, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 30 # display top 10 couples of variables (by correlation coefficient)
)

# without p-value
corr_cross(feat_df.num, # name of dataset
           pvalue = FALSE, # display only significant correlations (at 5% level)
           top = 30 # display top 10 couples of variables (by correlation coefficient)
)


# -------------------- Factor Analysis
parallel <- fa.parallel(feat_df.num,
                        fm = "minres",
                        fa = 'fa')


parallel$fa.values

factors <- fa(data.num_questions, 
              nfactors = 40, 
              rotate = 'oblimin', 
              fm = 'minres')
print(factors)


as.vector(rownames(factors$loadings))[(factors$loadings[, "MR10"] > 0.4)]
