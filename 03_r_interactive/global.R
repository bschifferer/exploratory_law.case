library(shiny)
library(shinydashboard)
library(listviewer)
library(plotly)
library(dplyr)
library(tidyverse)
library(httr) 
library(rjson)

source('./functions/helper.R')

#Global Parameters
API_KEY = 'Token <TOKEN>'
URL = 'https://api.case.law/v1/cases/?cite=&full_case=true&page_size=1&search='

# Loading data
df_cases <- read.csv('../data/04_rshiny/df_cases.csv')
df_total <- read.csv('../data/04_rshiny/df_total_cases.csv')
states <- read.csv('../data/04_rshiny/states.csv', sep = ';')
casetypes <- read.csv('../data/04_rshiny/casetypes.csv', sep = ';')

# Processing data
df_total$state <- as.character(df_total$state)
df_cases$state <- as.character(df_cases$state)
casetypes$columnname <- as.character(casetypes$columnname)
casetypes$label <- as.character(casetypes$label)
states$States <- as.character(states$States)
states$States_Code <- as.character(states$States_Code)

min_year = min(df_total$year)
max_year = max(df_total$year)
filter_states <- sort(as.character(unique(df_total$state)))

option_type <- c('Abs.', 'Rel.')

casetypes_geo <- casetypes %>%
  arrange(label)
casetypes_time <- casetypes %>%
  arrange(label)
casetypes_time <- data_frame(columnname = c('all'), label=c('All')) %>% 
  bind_rows(casetypes_time)

df_total <- df_total %>%
  left_join(states, by=c('state'='States'))
df_cases <- df_cases %>%
  left_join(states, by=c('state'='States'))

l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)