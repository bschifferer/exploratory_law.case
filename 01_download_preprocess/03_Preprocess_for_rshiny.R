library(data.table)
library(dplyr)
library(tidyverse)

setwd('~/Projects/Columbia/01_EDV/03_Project/5702_Project')

df <- fread('../data/03_combined/df_combined_v2.csv.gz', sep=',')
df$year <- as.numeric(substr(df$decision_date, 1, 4))
df <- df %>%
  filter(year>=1658)

# Which are the group by keys to reduce the datafile size
group_by_col = c('year', 'state', 'court_name')

# Replacement arguments require maybe adjustments
df <- df %>%
  mutate(state_tmp = str_replace(file, '/Projects/Columbia/01_EDV/03_Project/5702_Project/', ''))
df <- df %>%
  mutate(state = substr(state_tmp, 1, regexpr("-", df$state_tmp)-1))

# Which category columns should be exported
colnames(df) <- make.names(colnames(df))
rel_colnames <- colnames(df)[grepl('cb_data_text_0', colnames(df))]
rel_colnames <- c('cb_data_text_0_medical.malpractice',
                  'cb_data_text_0_drug.recall',
                  'cb_data_text_0_antitrust.prosecution',
                  'cb_data_text_0_asylum',
                  'cb_data_text_0_murder',
                  'cb_data_text_0_arson',
                  'cb_data_text_0_sexual.harassment',
                  'cb_data_text_0_divorce',
                  'cb_data_text_0_intellectual.property',
                  'cb_data_text_0_insurance.claims',
                  'cb_data_text_0_free.speech',
                  'cb_data_text_0_capital.murder',
                  'cb_data_text_0_patent')

df <- df %>%
  mutate_at(vars(rel_colnames),
            funs(as.numeric(.>0)))

df_total <- df %>%
  group_by(.dots = group_by_col) %>%
  summarise(count = n())

df_cases <- df %>%
  select_(.dots = c(group_by_col, rel_colnames)) %>%
  group_by(.dots = c(group_by_col, rel_colnames)) %>%
  summarise(count = n())

# Export the files
write.csv(df_cases, file = '../data/04_rshiny/df_cases.csv')
write.csv(df_total, file = '../data/04_rshiny/df_total_cases.csv')
