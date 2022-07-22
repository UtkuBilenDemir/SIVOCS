library(dplyr)
library(ggplot2)
source("./02_analysis/02_static_responses.R")

stem.fields <- c("Astronomy, Astrophysics and Space Sciences",
                  "Biophysic",
                  "Experimental Microbiology",
                  "Geochemistry",
                  "Information Technology",
                  "Medical Microbiology",
                  "Neurophysiology and Brain Research",
                  "Organic Chemistry",
                  "Technical Physics")

si.sum <- summary(as.numeric(data$contribToSI.rate.))
si.sum
si.high_range <- si.sum["3rd Qu."] : si.sum["Max."]
si.high_ind <- which(data$contribToSI.rate. %in% si.high_range)
df_high_si <- data[si.high_ind, ] # 34 Obs | 28m 6f 
# Obvious cases show great distribution of disciplines

# Ultimate df to collect them all
# 1. Add obvious cases with high self SI asses. 
df_ult <- df_high_si



td.sum <- summary(data$transdisciplinaryExp.rate.)
td.high_range <- td.sum["3rd Qu."] : td.sum["Max."]
td.high_ind <- which(data$transdisciplinaryExp.rate. %in% td.high_range)
df_high_td <- data[td.high_ind,]
dim(df_high_td)
table(df_high_si$discipline)

# examine stem disc.
df_high_td.stem <- df_high_td[which(df_high_td$discipline %in% stem.fields), ]
# high trans. exp. and high self si asses.
temp_ind <- which(df_high_td.stem$contribToSI.rate. %in% 
      round(summary(df_high_td.stem$contribToSI.rate.)["Mean"]) : summary(df_high_td.stem$contribToSI.rate.)["Max."])
df_high_td.stem.si <- df_high_td.stem[temp_ind, ] # 7 Obs, 7m
# Females of high trans. and stem ?
df_high_td.stem.fe <- df_high_td.stem %>% 
  filter(gender == "female") %>% 
  filter(contribToSI.rate. >= summary(contribToSI.rate.)["1st Qu."])


# examine further disc.
df_high_td.not_stem <- df_high_td[which(!(df_high_td$discipline %in% stem.fields)), ]
temp_ind <- which(df_high_td.not_stem$contribToSI.rate. %in% 
      round(summary(df_high_td.not_stem$contribToSI.rate.)["Mean"]) : summary(df_high_td.not_stem$contribToSI.rate.)["Max."])
df_high_td.not_stem.si <- df_high_td.not_stem[temp_ind,]
df_high_td.not_stem.si


# 2. Add the cases with high transdisciplinarity exp. 
# and high self SI asses.
df_ult <- rbind(df_ult, df_high_td.stem.si)
df_ult <- rbind(df_ult, df_high_td.stem.fe)
df_ult <- rbind(df_ult, df_high_td.not_stem.si)
df_ult <- df_ult[!duplicated(df_ult), ] # 52 Obs | 10f 42m

# Remove those from the main df
data <- data[!(data$id %in% df_ult$id),]


# Consider: Human Condition
hc.sum <- summary(data$motivation.welfare.)
hc.high_range <- hc.sum["3rd Qu."] : hc.sum["Max."]
hc.high_ind <- which(data$motivation.welfare. %in% hc.high_range)
df_high_hc <- data[hc.high_ind, ]

df_high_hc.cit <- df_high_hc %>% filter(groupsInvolved.citiz. > 0)

df_high_hc.civsoc <- df_high_hc %>% filter(impactTargetGroup.civsoc. > 3)

# Unsatisfactory representation 
data_fem <- data[data$gender == "female", ]
data_fem.si <- data_fem[which(data_fem$contribToSI.rate. > summary(data_fem$contribToSI.rate.)["Median"]), ]
df_ult <- rbind(df_ult, data_fem.si)
df_ult <- df_ult[!duplicated(df_ult), ] # 52 Obs | 10f 42m

data <- data[!(data$id == data_fem.si$id), ]

fsi.sum <- summary(data$familiarWithSI.response.)
df_high_fsi <- data[data$familiarWithSI.response. %in% (fsi.sum["3rd Qu."]+2) : fsi.sum["Max."],]

df_high_fsi$feedback

# Find relevant feedbacks in the cluster of high SI familiarity without the necessity of SI cont.
df_high_fsi.feedback <- df_high_fsi[c(1, 4, 20, 27, 31),]
df_ult <- rbind(df_ult, df_high_fsi.feedback)

df_high_fsi <- df_high_fsi[df_high_fsi$feedback == "", ]

# A critical feedback on SI
df_ult <- rbind(df_ult, df_high_fsi[16,])

# Defined above
df_high_hc.cit <- df_high_hc.cit[c(1, 7, 8, 9, 14, 15, 17), ]
df_ult <- rbind(df_ult, df_high_hc.cit)

# Metadata to the front
df_ult <- df_ult[, c(139:147, 1:146, 148)]
df_ult <- df_ult[, 1:146]

# Merge first name last name 
df_ult <- cbind(paste0( df_ult$firstname, " ", df_ult$lastname), df_ult)
colnames(df_ult)[1] <- "name"

# Take id to the front
df_ult <- df_ult[, c(11, 1:10, 12:ncol(df_ult))]


# Remove duplicates
df_ult <- df_ult[!duplicated(df_ult), ] # 64

# Reset row numbers
row.names(df_ult) <- 1:nrow(df_ult)


df_high_fsi[df_high_fsi$natureOfInvolvement.citiz. == "colla",]
df_ult[60,]
# We need to remove some
df_ult <- df_ult[-c(64,66,46,39,35,33,59,69,63,45, 26, 23, 16, 1, 55,70),]

row.names(df_ult) <- 1:nrow(df_ult)


write.csv(df_ult, "SIVOCS_int-cand.csv")
saveRDS(df_ult, "df_ult.RDS")