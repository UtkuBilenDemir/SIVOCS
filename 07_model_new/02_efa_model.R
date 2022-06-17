source("./07_model_new/01_feature_removal.R")

library(nFactors)
library(psych)
library(scales)

# Scree Plot
#-------------------------------------------------------------------------------
df_red2 <- df_red[, !colnames(df_red) %in% c("motivation.pheno.")] # Removing this one doesn't change anything
ev <- eigen(cor(df_red2)) # get eigenvalues

ap <- parallel(subject=nrow(df_red2), var=ncol(df_red2),
               rep=1000,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors

df_red.rescaled <- sapply(df_red2, scales::rescale, c(0,10))
df_red.rescaled <- na.omit(df_red.rescaled)
as.data.frame(df_red.rescaled)
efa_model <- fa.parallel(df_red.rescaled, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors


ncol(df_red.rescaled)
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
print(five_factor, cut = .4, digits = 2)
plot(five_factor)
cor.plot(five_factor)

# Herman's method variance bias
#-------------------------------------------------------------------------------
single_factor <- fa(df_red.rescaled, rotate = "none", fm = "ml", nfactors = 1)
# -- Explained Variance by 1 Factor is 0.26 which is smaller than 0.5 ==> 
# No Bias
