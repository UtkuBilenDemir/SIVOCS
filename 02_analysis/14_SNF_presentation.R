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
library(readxl)
library(formattable)
library(likert)
library(ggcorrplot)

# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

cand_df <- as.data.frame(read_excel("./SIVOCS_interview_candidates__batch1.xls"))
collab_df <- read.csv("./5555_preliminary_analysis/00_data/P3_CollaborationExport.csv", sep=";")
grant_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantExport_with_abstracts.csv", sep=";")
output_df <- read.csv("./5555_preliminary_analysis/00_data/P3_GrantOutputDataExport.csv", sep=";")
person_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PersonExport.csv", sep=";")
pub_df <- read.csv("./5555_preliminary_analysis/00_data/P3_PublicationExport.csv", sep=";")

sample_df <- read.csv("./01_data/snf_df_-_fname_lname.csv")
sample_df$domain <- unlist(lapply(str_split(sample_df$SP.Domain, ";"), `[[`, 1))


matched_sample_df <- sample_df[match(data$projID, sample_df$SP.Project.Number), ]

table(sample_df$domain)/nrow(sample_df)

table(data$domain)/nrow(data)

sample_gender <- as.data.frame(round(table(sample_df$Gender)/nrow(sample_df), 2))
respondents_gender <- as.data.frame(round(table(data$gender)/nrow(data), 2))
 

sample_domain <- as.data.frame(round(table(sample_df$domain)/nrow(sample_df), 2))
respondent_domain <- as.data.frame(round(table(data$domain)/nrow(data), 2))

sample <- c(nrow(sample_df) )
respondents <- c(nrow(data))



samp_res_df <- as.data.frame( cbind(sample, respondents))
rownames(samp_res_df) <- c("Number of Participants")
formattable(samp_res_df)

den <- read.csv("./01_data/sample_meta.csv")

formattable(den)

colnames(den)

# A meaningful table for sample bias? 
kbl(den) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Gender", 1, 3, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Scientific Domains", 3, 5, label_row_css = "background-color: #666; color: #fff;")


xtable(d1.df)

#----------------------- Infographics

#--- Age groups
barplot(table(data$academicAge))




#----------------------- Likert visualisation test

#--- A1 B1 C2

a1b1c2 <- as.data.frame(cbind(
  data[, a1.colnames],
  data$familiarWithSI.response.,
  data$contribToSI.rate.
))


names(a1b1c2) <- c(
  V1 = "How would you rate your experience with transdisciplinary research?",
  V2 = "How familiar are you with the concept of 'social innovation'?",
  V3 = "To what extent do you think your SNSF-funded project contributed to social innovation?"
)

likert_recode(a1b1c2)
a1b1c2_items_likert <- 
  #round(d1.df) %>%
  a1b1c2 %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$domain)


save_plot("deneme.svg", plot(items_likert) , width = 36, height = 18) + font_size(axis_title.x = 30)

library(plotly)
plot(a1b1c2_items_likert, 
     # Group the items alphabetically
     #group.order=names(colnames(e12.trans_df)),
     # Plot the percentages for each response category
     plot.percents = TRUE,
     # Plot the total percentage for negative responses
     ## plot.percent.low = FALSE,
     ## # Plot the total percentage for positive responses
     ## plot.percent.high = FALSE,

     #text.size=8,
     # Whether response categories should be centered
     # This is only helpful when there is a middle response
     # option such as "neutral" or "neither agree nor disagree"
     centered = TRUE,
     include.histogram = TRUE
     # Wrap label text for item labels
     #wrap=30
     ) 





#--- D1

likert_recode(d1.df)
d1_items_likert <- 
  #round(d1.df) %>%
  d1.df %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$domain)

likert_plot(d1_items_likert)

#--- E1

likert_recode(e1.df)
e1_items_likert <- 
  #round(d1.df) %>%
  e1.df %>%
  mutate_all(likert_recode2) %>%
  likert(grouping = data$domain)

likert_plot(e1_items_likert)

#--- G1 

likert_recode(g1.df)
g1_items_likert <- 
  #round(d1.df) %>%
  g1.df %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$domain)

likert_plot(g1_items_likert)

#---------------------- Correlations

#--- E1/G1
library(ggpubr)
corr_matrix_plt(as.data.frame(cbind(d1.df, e1.df)))

den_cor <- round(cor(as.data.frame(cbind(g1.df, d1.df)), method = "spearman", use = "pairwise.complete.obs"), 1)

corrp.mat <- cor_pmat(as.data.frame(cbind(g1.df, d1.df)))

ggcorrplot( den_cor, 
            hc.order=FALSE, 
            type="lower", 
            lab=TRUE,  
            p.mat = corrp.mat, 
            ggtheme = ggplot2::theme_minimal,
            colors = c("#3C9AB2", "white", "#F22300"),
            insig="blank")



# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
 
# From the help page:
data(flea)
ggpairs(as.data.frame(cbind(g1.df, d1.df)), columns = 1:5, ggplot2::aes(colour=data$domain)) 



den_cor <- round(cor(as.data.frame(cbind(g1.df, d1.df)), method = "spearman", use = "pairwise.complete.obs"), 1)

corrp.mat <- cor_pmat(as.data.frame(cbind(g1.df, d1.df)))

ggcorrplot(den_cor, 
            hc.order=FALSE, 
            type="lower", 
            lab=TRUE,  
            p.mat = corrp.mat, 
            ggtheme = ggplot2::theme_minimal,
            colors = c("#3C9AB2", "white", "#F22300"),
            insig="blank")

head(g2.df)
