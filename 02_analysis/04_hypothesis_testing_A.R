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


# Hypothesis A1:  Transdisciplinarity is usually an important 
# cornerstone for social innovation, although it is no condition 
# for it. Moreover, not all transdisciplinary research automatically 
# contributes to social innovation. Nevertheless, we assume that
# a higher experience in transdiscikplinary research might point to
# a higher propensity $for social innovation.

# A1 | E1, D1.2, D1.3, D2.3?
a1.df <- data[, c(a1.colnames, e1.colnames, d1.colnames[2:3], d2.colnames)]
a1e1.df <- data[, c(a1.colnames, e1.colnames)]

# Find the optimal number of factors
a1.ev <- eigen(cor(na.omit(a1.df)))
a1e1.ev <- eigen(cor(na.omit(a1e1.df)))
a1.ns <- nScree(x = a1.ev$values)
a1e1.ns <- nScree(x = a1e1.ev$values)
# Another effective way to determine number of factors:
# transdisexpa1e1.princomp <- princomp(na.omit(a1e1.df),
#                                      cor = T)
plotnScree(a1.ns) # ~4
plotnScree(a1e1.ns) # ~3

# Apply factor analysis
a1.fit <- factanal(na.omit(a1.df), 3, rotation = "varimax")
a1e1.fit <- factanal(na.omit(a1e1.df), 2, rotation = "varimax")
print(a1.fit)
print(a1e1.fit)

chart.Correlation(a1.df)
pairs.panels(a1e1.df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm = TRUE
             )

# exploratory factor analysis
a1e1.princomp <- princomp(na.omit(a1e1.df), cor = T) 
summary(a1e1.princomp)
plot(a1e1.princomp)

biplot(a1e1.princomp)

# A1 into 3 clusters (kinda redundant?)
trans_exp <- data$transdisciplinaryExp.rate.
data$transdisexp <- ifelse(trans_exp <= 3, 1,
                      ifelse(trans_exp %in% 4:6, 2,
                        ifelse(trans_exp >= 7, 3, 99)))

ggplot(data, aes(x = transdisexp, y = groupsInvolved.citiz.)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  geom_jitter()

data$groupsInvolved.res.
ggplot(data, aes(x = transdisexp, y = groupsInvolved.res.)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  geom_jitter()

a <- lm(data=data, transdisexp ~ groupsInvolved.citiz. + groupsInvolved.res. + groupsInvolved.citiz.:groupsInvolved.res.)
b <- lm(data=data, transdisexp ~ groupsInvolved.citiz. + groupsInvolved.res.)

c <- lm(data=data, transdisexp ~ groupsInvolved.citiz. + groupsInvolved.res. + groupsInvolved.civsoc. + groupsInvolved.citiz.:groupsInvolved.res.)

data.questions.numeric <- data.questions[, as.vector(sapply(FUN=is.numeric, data.questions))]

corrplot(cor(na.omit(data.questions.numeric[, 1:10])))
chart.Correlation(data[, c("transdisexp",e1.colnames)],
                           histogram = TRUE,
                           pch = 19)
