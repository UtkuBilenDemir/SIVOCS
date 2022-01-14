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

#--------------------- D1

# Hypothesis: The motivation to directly address a natural,
# technical economic or social problem or even to improve
# the human condition/welfare can be a strong component
# for social innovation, although it is not a pre-condition.
# The motivation to better understand a natural, technical,
# economic or social phenomenon, however, points to a rather
# “regular” scientific motivation, without directly aiming to
# problem solving or improving human/welfare conditions.

# We have 3 different variables
# How do the dependent variable correlate against each other
corr_matrix_plt(d1.df)


#--- IDVs

# Academic Fields (Later, dummy)

# F3
corr_matrix_plt(cbind(d1.df, f3.df))

# C3
corr_matrix_plt(cbind(d1.df, c3.df))


#---------------------- D3
corr_matrix_plt(cbind(e1.df, d3.df))

corr_matrix_plt(g1.df)

d3.colnames
e1.colnames
g1.colnames

d1.test.df <- as.data.frame(cbind(d1.df, 
                                  "civ.prod" = g1.modf_civil))

d1.test.lm <- lm(data = d1.test.df, civ.prod ~ motivation.pheno. + motivation.prob. + motivation.welfare. )

summary(d1.test.lm)

# Factor analysis for d1/d2/d3
d.df <- as.data.frame(cbind(d1.df, benefitForNonAcademy=d2.df,d3.df))
head(d.df)

bartlett.test(na.omit(d.df))
# p value is way smaller than 0.05, that is good

d.scaled <- apply(na.omit(d.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(d.scaled))) # Looks terrible <:O, moving on :%
KMO(cor(na.omit(d.scaled)))$MSA

# Find the optimal number of factors
d.ev <- eigen(cor(na.omit(d.df))) 
d.ns <- nScree(x = d.ev$values)
plotnScree(d.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
d.fit <- factanal(na.omit(d.df), d.ns$Components[["nkaiser"]], rotation = "varimax")
print(d.fit)

# Visualize
loads <- e1.fit$loadings
fa.diagram(loads)

summary(e1.df)

