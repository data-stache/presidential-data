{# Libraries
  library(Hmisc)
  library(lubridate)
  library(broom)
  library(tidyverse)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)


# Master Variables -------------------------------------------------------------
RUN_DATE <- Sys.Date()



# Presidential Approval Ratings ------------------------------------------------
# Load FiveThirtyEight Pollster Grades
fte_pollster_grades <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  mutate(run_date = RUN_DATE)

# Overwrite
write.csv(fte_pollster_grades, file = 'data/fte_pollster_grades.csv')

fte_pollster_grades_only <- fte_pollster_grades %>%
  select(pollster = Pollster, fte_grade = X538.Grade)

write.csv(fte_pollster_grades_only, file = 'data/fte_pollster_grades_only.csv')
