library(dplyr)
library(ggplot2)
library(magrittr)
library(ggplot2)
library(PerformanceAnalytics)
library(psych)
library(corrplot)
library(nFactors)
library(kableExtra)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(gt)


# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

#!!!!!!!!!!!!!!!!!!!!!!!! CAUTION
# G variables are constituting our main dependent variable
# Inner relations of the sub variables needs to be studied 
# carefully. "impactStatement" sub-variables seem to be
# mergeable according to the factor analysis

# Numeric G variables under factor analysis

# Bartlett test is a test for homogeneity of variances in
# which the null hypothesis is that all k population
# variances are equal against the alternative that at least
# two are different.
bartlett.test(na.omit(g.df))
# p value is way smaller than 0.05, that is good

g.scaled <- apply(na.omit(g.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(g.scaled))) # Looks terrible <:O, moving on :%
KMO(cor(na.omit(g.scaled)))$MSA

# Find the optimal number of factors
g.ev <- eigen(cor(na.omit(g.df))) 
g.ns <- nScree(x = g.ev$values)
plotnScree(g.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
g.fit <- factanal(na.omit(g.df), g.ns$Components[["nkaiser"]], rotation = "varimax")
g.fit2 <- factanal(na.omit(g.df), 3, rotation = "varimax")
print(g.fit)

# Visualize
loads <- g.fit$loadings
fa.diagram(loads)

# Output
FactorLoadings <- round(g.fit$loadings, 3)
write.csv(FactorLoadings, file="G_factor_analysis.csv")

# Trial with scaleability

gh.df <- as.data.frame(cbind(g.df, h2.composite))
bartlett.test(na.omit(gh.df))
gh.scaled <- apply(na.omit(gh.df), MARGIN=2, FUN=normalize)

KMO(cor(na.omit(gh.scaled))) # Looks terrible <:O, moving on :%
KMO(cor(na.omit(gh.scaled)))$MSA

# Find the optimal number of factors
gh.ev <- eigen(cor(na.omit(gh.df))) 
gh.ns <- nScree(x = gh.ev$values)
plotnScree(gh.ns) 
# If we go with the Kaiser Criteria, there are 3 factors

# Apply factor analysis
gh.fit <- factanal(na.omit(gh.df), gh.ns$Components[["nkaiser"]], rotation = "varimax")
gh.fit2 <- factanal(na.omit(gh.df), 3, rotation = "varimax")
print(gh.fit)



#------------------------- Hypo_testing 

#--- G1

# G1 (overall score) correlates with E1
g1e1.df <- as.data.frame(cbind(g1 = g1.modf_civil,
                    e1.modf_civil,
                    res = e1.df$groupsInvolved.res.))
g1e1.model <- lm(data = g1e1.df, g1 ~ civ + bus + med + res)
summary(g1e1.model)

g1e1.df2 <- as.data.frame(cbind(g1 = g1.modf_civil,
                                e1.df))

g1e1.model2 <- lm(data = g1e1.df2, g1 ~ groupsInvolved.res. + 
                  groupsInvolved.busi. + 
                  groupsInvolved.civsoc. + 
                  groupsInvolved.policy. +
                  groupsInvolved.citiz.+
                  groupsInvolved.media. +
                  groupsInvolved.welfare.
                  )
tab_model(g1e1.model2)


# G1 (overall score, no acad) correlates with E1.transdisciplinarity (no interdisc)
g1e1.df3 <- as.data.frame(cbind(g1 = g1.modf_civil,
                                e1 = e1.df_trans))

# G6
ggplot(g1e1.df3, 
       aes(x=e1, y=g1)) + 
  geom_point() + 
  ggtitle("") + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, color='#e7298a')+
  geom_jitter(color='#377eb8')+
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + 
  geom_point(color='#377eb8') +
  #scale_y_continuous(name = "UNC45A", limits = c(5,15), breaks = seq(5, 15, 2)) + 
  #theme(plot.title = element_text(hjust = 0.5), 
        #panel.background = element_blank(), 
  #      axis.line = element_line(color="black"), 
  #      axis.line.x = element_line(color="black")) 
  #theme_bw()
  labs(
    x="groupsInvolved",
    y = "impactTargetGroup")


# G1 (overall score) correlates with E1, A1, E3

corr_matrix_plt(cbind(g1.modf_civil, e1.df_trans, data$transdisciplinaryExp.rate., e3.modf))

g1.model.df <- as.data.frame(cbind(g1 = g1.modf_civil,
                    e1 = e1.df_trans,
                    trans.exp = data$transdisciplinaryExp.rate.,
                    trans.asp = e3.modf))
g1.model <- lm(data = g1.model.df, g1 ~ e1 + trans.exp + trans.asp + e1)
summary(g1.model)


#--- G4
corr_matrix_plt(cbind(g4.df, f3.df, e1.modf, d1.df, d2.df, g1.modf.3))

#--- G6
corr_matrix_plt(g6.df)


#... G composite
