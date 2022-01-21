library(scales)

source("./02_analysis/02_static_responses.R")
data[data == 99] <- NA
data[data == 98] <- NA
data.questions[data.questions == 99] <- NA
data.questions[data.questions == 98] <- NA

data$academicAgeNum[data$academicAge == "c"] <- 3
data$academicAgeNum[data$academicAge == "d"] <- 4 
data$academicAgeNum[data$academicAge == "e"] <- 5 
data$academicAgeNum[data$academicAge == ""] <- NA 
data$academicAgeNum <- as.numeric(data$academicAgeNum)

a1.colnames <- colnames(data.questions)[grep("transdisciplinary", 
                                        colnames(data.questions))]
c3.colnames <- colnames(data.questions)[grep("contribToSI.rate.", 
                                        colnames(data.questions))]
d1.colnames <- colnames(data)[grep("motivation\\.", 
                              colnames(data))]
d2.colnames <- colnames(data.questions)[grep("benefit", 
                                        colnames(data.questions))]
d3.colnames <- colnames(data.questions)[grep("impulseForNonAcad.", 
                                        colnames(data.questions))]
e1.colnames <- colnames(data)[grep("groupsInvolved\\.", 
                              colnames(data))]
e2.colnames <- colnames(data)[grep("natureOfInvolvement\\.", 
                              colnames(data))]
e3.colnames <- colnames(data)[grep("targetGroupsGoals\\.", 
                              colnames(data))]
f1.colnames <- colnames(data.questions)[grep("concepts\\.", colnames(data.questions))]
f3.colnames <- colnames(data.questions)[grep("concepts3", 
                                        colnames(data.questions))]
g1.colnames <- colnames(data.questions)[grep("impactTargetGroup.",
                                        colnames(data.questions))]
g2.colnames <- colnames(data.questions)[grep("kindOfChange",
                                        colnames(data.questions))]
g3.colnames <- colnames(data.questions)[grep("kindOfChangeOther",
                                        colnames(data.questions))]
g4.colnames <- colnames(data.questions)[grep("adoptByPolicy\\.",
                                        colnames(data.questions))]
g5.colnames <- colnames(data.questions)[grep("adoptByPolicyHow",
                                        colnames(data.questions))]
g6.colnames <- colnames(data.questions)[grep("Impactstatements",
                                        colnames(data.questions))]
h1.colnames <-  colnames(data.questions)[grep("dissChannels",
                                        colnames(data.questions))]
h2.colnames <- colnames(data.questions)[grep("scalability",
                                        colnames(data.questions))]



#------------------------ DFs


#--- Data

# A numeric df for factor analysis purposes
data.numeric_bool <- as.vector(sapply(FUN=is.numeric, data.questions))
data.questions_numeric <- data.questions[, data.numeric_bool]


#--- C3

c3.df <- data[, c3.colnames]

#--- D1

d1.df <- data[, d1.colnames]



#--- D1

d2.df <- data[, d2.colnames]


#--- D3

# Impfulse from nonacad world
d3.df <- data[, d3.colnames[1:length(d3.colnames)-1]]
# Turn those into binary variables
d3.df <- apply(d3.df, 2, FUN = function(x) {ifelse(x == "Y", 1, 0)})


#--- E1

e1.df <- data[, e1.colnames]


#--- E2

e2.df <- data[, e2.colnames]
e2.df[e2.df == ""] <- 0
e2.df[e2.df == "cons"] <- 1 
e2.df[e2.df == "contr"] <- 2 
e2.df[e2.df == "colla"] <- 3 
e2.df[e2.df == "cocr"] <- 4 
e2.df <- apply(FUN = as.numeric, MARGIN=2, e2.df)
e2.df <- as.data.frame(e2.df)


#--- E3

e3.df <- data[, e3.colnames]

#--- E12: 1 and 2 combined

e12.df <- data.frame( inv.other.disciplines = e1.df$groupsInvolved.res. + e2.df$natureOfInvolvement.res.,
                      inv.business = e1.df$groupsInvolved.busi. + e2.df$natureOfInvolvement.busi.,
                      inv.civsoc = e1.df$groupsInvolved.civsoc. + e2.df$natureOfInvolvement.civsoc.,
                      inv.policy = e1.df$groupsInvolved.policy. + e2.df$natureOfInvolvement.policy.,
                      inv.citiz = e1.df$groupsInvolved.citiz. + e2.df$natureOfInvolvement.citiz.,
                      inv.media = e1.df$groupsInvolved.media. + e2.df$natureOfInvolvement.media.,
                      inv.welfare = e1.df$groupsInvolved.welfare. + e2.df$natureOfInvolvement.welfare.                     
                      )
### e12.df <- as.data.frame(sapply(e12.df, FUN=function(x) {rescale(x, to=c(0,10))}))


# Create a transdiscip. variable
e12.trans <- e12.df$inv.citiz +
                e12.df$inv.media +
                e12.df$inv.welfare +
                e12.df$inv.civsoc +
                e12.df$inv.policy +
                e12.df$inv.business

### e12.trans_df <- data.frame( inv.other.disciplines  = e12.df$inv.other.disciplines,
###                             inv.trans = rescale(e12.trans, c(0,10))
### )


#e12.trans_df <- rescale(e12.trans_df, to = c(0, 10))

#--- E

# e.df <- as.data.frame(cbind(e1.df, e2.df, e3.df))
# Trial with e12
e.df <- as.data.frame(cbind(e12.df, e3.df))


#--- F3

f1.df <- data[, f1.colnames]


#--- F3

f3.df <- data[, f3.colnames]
f3.df <- ifelse(f3.df == "Y", 1, 0)


#--- G1

g1.df <- data[, g1.colnames]


#--- G2

g2.df <- data[, g2.colnames]


#--- G3

g3.df <- data[, g3.colnames]


#--- G4

g4.df <- data[, g4.colnames]
g4.df[g4.df == 99] <- NA

#--- G5

g5.df <- data[, g5.colnames]

#--- G6

g6.df <- data[, g6.colnames]
g6.df[g6.df == 99] <- NA

#G6 composite

# First calculate the "importance of each column"
### g6.col_weight <- as.vector(rescale(10 - colMeans(g6.df, na.rm = TRUE), c(0.6, 1)))
### g6.weighted_df <- g6.df 
### g6.weighted_df$Impactstatements.capab. <- g6.weighted_df$Impactstatements.capab. * g6.col_weight[1]
### g6.weighted_df$Impactstatements.emanc. <- g6.weighted_df$Impactstatements.emanc. * g6.col_weight[2]
### g6.weighted_df$Impactstatements.understanding. <- g6.weighted_df$Impactstatements.understanding. * g6.col_weight[3]
### g6.weighted_df$Impactstatements.mitig. <- g6.weighted_df$Impactstatements.mitig. * g6.col_weight[4]
### g6.weighted_df$Impactstatements.unknown. <- g6.weighted_df$Impactstatements.unknown. * g6.col_weight[5]
### g6.weighted_df$Impactstatements.unaddressed. <- g6.weighted_df$Impactstatements.unaddressed. * g6.col_weight[6]
### 
### # Take rowsums
### g6.weighted_df$rowSums <- rowSums(g6.weighted_df, na.rm = TRUE)
### g6.weighted_df$nCell <- 6 - apply(g6.weighted_df[, 1:6], MARGIN=1, FUN=function(x){sum(is.na(x))})
### g6.weighted_df$zCell <- apply(g6.weighted_df[, 1:6], MARGIN=1, FUN=function(x){sum(x == 0)})
### g6.weighted_df$nCell[g6.weighted_df$zCell >=3] <-3
### 
### # COMPOSITE
### g6.weighted_df$nCell[g6.weighted_df$nCell == 0] <- 99  
### g6.weighted_df$nCell[g6.weighted_df$nCell <= 3] <- 3 
### g6.weighted_df$composite <- g6.weighted_df$rowSums / g6.weighted_df$nCell
### g6.weighted_df$composite[g6.weighted_df$nCell == 99] <- NA
### 
### 
### g6.dummy <- g6.weighted_df[, 1:6]
### g6.dummy[is.na(g6.dummy)] <- 0
### g6.weighted_df$composite2 <- rowMeans(g6.dummy)

# Scale to 10
### g6.weighted_df$composite.10 <- rescale(g6.weighted_df$composite, c(0, 10))
### g6.weighted_df$composite2.10 <- rescale(g6.weighted_df$composite2, c(0, 10))

# Handle decimals
is.num <- sapply(g6.weighted_df, is.numeric)
g6.weighted_df[is.num] <- lapply(g6.weighted_df[is.num], round, 2)

g6.weighted_df$composite.10.R <- round(g6.weighted_df$composite.10, 1)
write.csv(g6.weighted_df, "g6_weigthed_df.csv")


#--- G, only with the ordinal variables

g.df <- as.data.frame(cbind(g1.df,
                    adoptByPolicy.rate. = g4.df,
                    g6.df))


#--- G7 (H actually but could fit here)

h2.df <- data[, h2.colnames]
h2.df[h2.df==99] <- NA


#--- H1

h1.df <- data[, h1.colnames]
h1.df[is.na(h1.df)] <- 0
### h1.df$h1.composite <- round(rescale(rowMeans(h1.df), c(0,10)), 1)

#------------------------ Combined Variables

# Only civil variables 
e1.modf <- as.data.frame(cbind("e1.bus" = e1.df$groupsInvolved.busi., 
                       "e1.med" = e1.df$groupsInvolved.media.,
                       "e1.civ" = (e1.df$groupsInvolved.civsoc. + e1.df$groupsInvolved.policy. + e1.df$groupsInvolved.welfare. + e1.df$groupsInvolved.citiz. )/4 ,
                       "e1.res" = e1.df$groupsInvolved.res. 

  )
)

e1.modf_civil <- as.data.frame(cbind("bus" = e1.df$groupsInvolved.busi., 
                       "med" = e1.df$groupsInvolved.media.,
                       "civ" = (e1.df$groupsInvolved.civsoc. + e1.df$groupsInvolved.policy. + e1.df$groupsInvolved.welfare. + e1.df$groupsInvolved.citiz. )/4 
  )
)
head(e1.modf_civil)

e1.df_trans <- (e1.df$groupsInvolved.civsoc. + 
               e1.df$groupsInvolved.busi. +
               e1.df$groupsInvolved.policy. + 
               e1.df$groupsInvolved.citiz. + 
               e1.df$groupsInvolved.media. +
               e1.df$groupsInvolved.welfare.) /6

# E3 as single variable

e3.modf <- rep(0, nrow(e3.df))
for (i in 1:ncol(e3.df)) {
  print(i)
  e3.modf <- e3.modf + e3.df[, i]
}
e3.modf <-  e3.modf*2

# Only civil outcomes

g1.df_civil <- g1.df[, 1:ncol(g1.df)-1]

g1.df_civil.comb <- rep(0, nrow(g1.df))
for (i in 1:(length(g1.colnames)-1)) {
  g1.df_civil.comb <- g1.df_civil.comb + g1.df_civil[, i]
  print(i)
}
g1.modf_civil <- g1.df_civil.comb/(length(g1.colnames)-1)

g1.modf.3 <- as.data.frame(cbind(g1.df[, c(1, 2, 7)], 
                                 impactTargetGroup.civorg.comp. = (g1.df$impactTargetGroup.socgr. +
                                           g1.df$impactTargetGroup.welfare. +
                                           g1.df$impactTargetGroup.civsoc. +
                                           g1.df$impactTargetGroup.policy.)/4
  )
)


# H2 as a single variable

h2.composite <- (h2.df$scalabilityRating.deep. + h2.df$scalabilityRating.up. + h2.df$scalabilityRating.out.)/3


#--- AN ATTEMPT FOR AN SI.COMPOSITE
### g1.civil_composite <- round(rescale(rowMeans(g1.df_civil), c(0,10)), 2)

outcome.df <- data.frame( g1.civil_composite = g1.civil_composite,
                          g4.composite = g4.df,
                          g6.composite = g6.weighted_df$composite.10,
                          h2.composite = h2.composite
                          )

temp <- outcome.df
temp[is.na(outcome.df)] <- 0
outcome.composite <- rowMeans(temp)
### outcome.composite <- rescale(outcome.composite, c(0,10))
outcome.composite <- round(outcome.composite, 2)


outcome.composite2 <- rowMeans(outcome.df, na.rm=TRUE)
### outcome.composite2 <- rescale(outcome.composite2, c(0,10))
outcome.composite2 <- round(outcome.composite2, 2)

outcome.composite[is.na(outcome.composite2)] <- NA
outcome.df <- cbind(outcome.df,
                    outcome.composite = outcome.composite,
                    outcome.composite2 = outcome.composite2
                    )
##barplot(table(outcome.composite))


##write.csv(outcome.df, "outcome_df.csv")

#--------------- CUSTOM FUNCTIONS

# Correlation matrix with lm model
corr_matrix_plt <- function (df, colcol=" ") {pairs.panels(
    x=df, 
    method = "spearman", # correlation method
    hist.col = ifelse(colcol==" ",  "#00AFBB", colcol),
    density = TRUE,  # show density plots
    ellipses = TRUE, # show correlation ellipses
    lm = TRUE,
    stars=TRUE,
    jiggle=TRUE,
    factor=2
    )
  }

# MinMaxScaler from Python
normalize <- function(x, na.rm = TRUE) {
    return((x- min(x)) /(max(x)-min(x)))
}

cat(paste0("99 sourced"))

cor_mat <- cor(data.questions[,as.vector(sapply(data.questions, FUN=is.numeric))],  use = "complete.obs", method="spearman")
write.csv(cor_mat, "cor_mat.csv")


#--- Scatter plot
lm_scatter <- function(df, x, y, labx="", laby="", title="", posx=9, posy=0) {
ggplotly(
ggplot(df, 
       aes(y = y, x = x)) + 
  geom_point() + 
  ggtitle(title) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, color='#e7298a')+
  geom_jitter(color='#377eb8')+
  theme_minimal() +
  geom_point(color='#377eb8') +
  labs(
    y=laby,
    x =labx) +
  stat_cor(label.x = posx, label.y =posy , cor.coef.name = "rho", output.type = "text", method = "spearman") +
  stat_regline_equation(output.type = "text",label.x = posx, label.y = posy +1)
)

}







# A custom function to recode numerical responses into ordered factors
likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x <= 3, "0...not at all - 3",
                     ifelse(x > 3 & x <7, "4 - 6" ,
                            ifelse(x >= 7, "7 - 10...fully", " "))))
  
  y <- factor(y, levels = c("0...not at all - 3", "4 - 6", "7 - 10...fully"))
  
  return(y)
}

likert_recode2 <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 0, "No",
                     ifelse(x == 1, "Only Marginally" ,
                            ifelse(x == 2, "Quite Centrally", " "))))
  
  y <- factor(y, levels = c("No", "Only Marginally" , "Quite Centrally"))
  
  return(y)
}

likert_recode3 <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 0, "No Involvement",
                     ifelse(x == 1, "Consultative" ,
                            ifelse(x == 2, "Contributory",
                                ifelse(x == 3, "Collaboratively",
                                    ifelse(x == 4, "Co-created", ""))))))
  
  y <- factor(y, levels = c("No Involvement", 
  "Consultative" , 
  "Contributory",
  "Collaboratively",
  "Co-created"
  ))
  
  return(y)
}
likert_recode4 <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 0, NA,
                     ifelse(x == 1, "Consultative" ,
                            ifelse(x == 2, "Contributory",
                                ifelse(x == 3, "Collaboratively",
                                    ifelse(x == 4, "Co-created", ""))))))
  
  y <- factor(y, levels = c( 
  "Consultative" , 
  "Contributory",
  "Collaboratively",
  "Co-created"
  ))
  
  return(y)
}


#--- A standardized plot for likert variables
likert_plot <- function(recoded_likert, h=TRUE, t=4, c = 1:3) {
  plot( recoded_likert, 
        # Group the items alphabetically
        # group.order=names(colnames(e12.trans_df)),
        # Plot the percentages for each response category
        plot.percents = TRUE,
        # Plot the total percentage for negative responses
        plot.percent.low = FALSE,
        # Plot the total percentage for positive responses
        plot.percent.high = FALSE,
        # Whether response categories should be centered
        # This is only helpful when there is a middle response
        # option such as "neutral" or "neither agree nor disagree"
        centered = TRUE,
        include.histogram = h,
        # Wrap label text for item labels
        wrap=80,
        text.size=t,
        colors =  c("#D32F49", "#F4A582", "#DFDFDF", "#92C5DE", "#338BBE")[c] 

        )


}












#--- This is good
# install.packages("GGally")
library(GGally)

e12_disc <- na.omit(as.data.frame(cbind(e12.trans_df, domain=data$domain)))
e12_disc[, 1:3] <- sapply(FUN=as.numeric,e12_disc[, 1:3])
ggpairs(e12_disc, columns = 1:3, aes(color=domain))
e12_disc[,1]
