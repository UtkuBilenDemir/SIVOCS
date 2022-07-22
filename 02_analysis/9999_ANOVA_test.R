library(psych)
library(corrplot)
library(nFactors)

# Data Frame: data
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

# discipline by motivation to welfare

describeBy(data$motivation.welfare.,
           data$discipline)
length(unique(data$discipline))

anova_m <- aov(data$motivation.welfare. ~
               data$discipline)
summary(anova_m)

pairwise.t.test(data$motivation.welfare.,
           data$discipline)
