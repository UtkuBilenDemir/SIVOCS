# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

sum(data.questions >= 50, na.rm = TRUE)

unique(data.questions[data.questions == 98])


# Try the geom_mosaic

