library(dplyr)
library(ggplot2)
library(magrittr)
library(ggplot2)
library(PerformanceAnalytics)
library(psych)
library(corrplot)
library(nFactors)

# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

# H1: Higher the knowledge about SI, 
# further the involvement of other disciplines.

corr_matrix_plt(cbind(data$familiarWithSI.response., e1.df$groupsInvolved.res., e1.df_trans))
e1.modf_civil


corr_matrix_plt(cbind(e1.df_trans, data$familiarWithSI.response., data$transdisciplinaryExp.rate., d1.df[, 2:3]))

head(d1.df[, 2:3])
e1.df_model <- data.frame(cbind(e1.df_trans, data$familiarWithSI.response., data$transdisciplinaryExp.rate., d1.df[, 2:3]))
e1.model <- lm(data=e1.df_model, e1.df_trans ~ data$familiarWithSI.response. + data$transdisciplinaryExp.rate. + d1.df[, 2] + d1.df[, 3])
summary(e1.model)

# Bartlett test is a test for homogeneity of variances in
# which the null hypothesis is that all k population
# variances are equal against the alternative that at least
# two are different.
bartlett.test(na.omit(e1.df))
# p value is way smaller than 0.05, that is good

e1.scaled <- apply(na.omit(e1.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(e1.scaled))) # Looks terrible <:O, moving on :%
KMO(cor(na.omit(e1.scaled)))$MSA

# Find the optimal number of factors
e1.ev <- eigen(cor(na.omit(e1.df))) 
e1.ns <- nScree(x = e1.ev$values)
plotnScree(e1.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
e1.fit <- factanal(na.omit(e1.df), 2, rotation = "varimax")
print(e1.fit)

# Visualize
loads <- e1.fit$loadings
fa.diagram(loads)

summary(e1.df)

#----------------- SOLVE E

# Figure out if we can reduce the dimensions in whole E

bartlett.test(na.omit(e.df))
e.scaled <- apply(na.omit(e.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(e.scaled)))
KMO(cor(na.omit(e.scaled)))$MSA

# Find the optimal number of factors
e.ev <- eigen(cor(na.omit(e.df))) 
e.ns <- nScree(x = e.ev$values)
plotnScree(e.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
e.fit <- fa(na.omit(e.df),
                  e.ns$Components[["nkaiser"]],
                  rotate = "varimax",
                  alpha=0.05)
print(e.fit)

head(e.df)
# Visualize
loads <- e.fit$loadings
fa.diagram(loads)


# Figure out if we can reduce the dimensions in whole E

bartlett.test(na.omit(e12.df))
bartlett.test(na.omit(e3.df))
e12.scaled <- apply(na.omit(e12.df), MARGIN=2, FUN=normalize)
e3.scaled <- apply(na.omit(e3.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(e3.scaled)))$MSA
KMO(cor(na.omit(e12.scaled)))$MSA

# Find the optimal number of factors
e12.ev <- eigen(cor(na.omit(e12.df))) 
e3.ev <- eigen(cor(na.omit(e3.df))) 

e12.ns <- nScree(x = e12.ev$values)
e3.ns <- nScree(x = e3.ev$values)
plotnScree(e3.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
e12.fit <- factanal(na.omit(e12.df),
                  e12.ns$Components[["nkaiser"]],
                  rotation = "promax")
e3.fit <- factanal(na.omit(e3.df),
                  e3.ns$Components[["nkaiser"]],
                  rotation = "varimax")

print(e12.fit)

# Visualize
loads <- e.fit$loadings
fa.diagram(loads)

