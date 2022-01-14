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

# Bartlett test is a test for homogeneity of variances in
# which the null hypothesis is that all k population
# variances are equal against the alternative that at least
# two are different.
bartlett.test(na.omit(data.questions.numeric))
# p value is way smaller than 0.05, that is good

# MinMaxScaler from Python
normalize <- function(x, na.rm = TRUE) {
    return((x- min(x)) /(max(x)-min(x)))
}
data.scaled <- apply(na.omit(data.questions.numeric), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(data.scaled))) # Looks terrible <:O, moving on :%

KMO(cor(na.omit(data.scaled)))$MSA

cortest.bartlett(na.omit(data.questions.numeric))



# Find the optimal number of factors
data.ev <- eigen(cor(na.omit(data.questions_numeric))) # Sourced from 99, data with only numeric (read ordinak) variables
data.ns <- nScree(x = data.ev$values)
plotnScree(data.ns) 
# If we go with the Kaiser Criteria, there are 17 factors

# Apply factor analysis
data.fit <- factanal(na.omit(data.questions.numeric), 17, rotation = "varimax")
print(data.fit)


# Visualize
loads <- data.fit$loadings
fa.diagram(loads)

# Output
FactorLoadings <- round(data.fit$loadings, 3)
write.csv(FactorLoadings, file="FacLoads.csv")
