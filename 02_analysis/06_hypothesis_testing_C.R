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


# Hypothesis: The self-assessment is an important 
# variable to identify the contribution to social 
# innovation. The higher the self-assessment is the 
# higher is probably the real contribution to social
# innovation. We assume, however, a trend towards under
# estimation. Therefore, it is important to confront 
# the self-assessment with other indicators.

#------------------------- C
c3.df

corr_matrix_plt(e1.df[, -1])
corr_matrix_plt(e1.modf_civil)

corr_matrix_plt(d1.df[, c(2,3)])

c3_lm_df <- as.data.frame(cbind("c3" = c3.df,
                  "bus" = e1.modf_civil$bus,
                  "med" = e1.modf_civil$med,
                  "civ" = e1.modf_civil$civ,
                  "motivation.prob." = d1.df$motivation.prob.,
                  "motivation.welfare." = d1.df$motivation.welfare.)
)


c3_lm <- lm(data=c3_lm_df, c3 ~ bus + med + civ + motivation.prob. + motivation.welfare. + civ:motivation.welfare.)
summary(c3_lm)

c3_lm_alt <- lm(data=c3_lm_df, c3 ~ civ + motivation.prob. + motivation.welfare. + civ:motivation.welfare.)
summary(c3_lm_alt)
