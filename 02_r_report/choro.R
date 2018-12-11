library(httr) 
library(rjson)
library(tidyverse)
library(lubridate)
library(data.table)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(scales)
library(tidycensus)
library(ipumsr)

date_lim1 = "1967-01-01"
date_lim2 = "2017-01-01"

start_year = as.integer(strsplit(date_lim1, "-")[[1]][1])
end_year = as.integer(strsplit(date_lim2, "-")[[1]][1])-1

keyword = "internet"

data(df_state_demographics)

setwd("C:/Users/user/Documents/5702_Project")
file_list <- list.files(pattern="*text.csv")

totalList = data.frame(year=integer(), value1=integer(), region=character())

for (name in file_list){
  region <- (strsplit(name, "-")[[1]][1]) 
  print(region)
  data <- fread(name, sep=",")
  data$cb_data_text_0 = ifelse(data[,c(52)] > 0, 1, 0)
  data <- data %>% group_by(decision_date) %>% summarise(total_cases = n(), value1 = sum(cb_data_text_0))
  
  data$decision_date <- as.Date(data$decision_date, format="%Y-%m-%d")
  data <- data %>% na.omit() %>% filter(decision_date >= as.Date(date_lim1) & decision_date < as.Date(date_lim2))
  if(nrow(data)>0){
    data['year'] <- as.integer(format(data$decision_date, "%Y"))
    data <- data %>% group_by(year) %>% summarise(total_cases = sum(total_cases), value1 = sum(value1))
    data['region'] <- tolower(region)
    totalList <- rbind(totalList, data) 
  }
}
totalList$value <- totalList$value1/totalList$total_cases
totalList <- totalList[totalList$value <= 0.3,]

dir.create(file.path("results/", keyword), showWarnings = FALSE)
write.csv(totalList, file = paste0("results/", keyword, "/", keyword, "_", toString(start_year), "_", toString(end_year), ".csv"))

choropleths = list()
index = 1
for (i in seq(from=start_year, to=end_year)){
  mapData <- totalList %>% filter(year == i)
  mapData <- mapData[,c('region', 'value')]
  
  for (state in setdiff(df_state_demographics$region, mapData$region)) {
    df<-data.frame(state,0)
    names(df)<-c("region","value")
    mapData <- rbind(mapData, df)
  }
  mapData = mapData[!mapData$region %in% c("alaska", "hawaii"), ]
  
  c = StateChoropleth$new(mapData)
  c$title = paste0("Appearance of the word ", keyword, " in US Case law, year=", i)
  c$set_num_colors(1)
  c$show_labels=FALSE
  c$clip()
  
  c$bind()
  
  choropleth <- c$render()
  
  df_state_labels = data.frame(long = state.center$x, lat = state.center$y, name=tolower(state.name), label = state.abb)
  df_state_labels = df_state_labels[!df_state_labels$name %in% c("alaska", "hawaii"), ]
  
  choropleth = choropleth + geom_text(data = df_state_labels, aes(long, lat, label = label, group = NULL), color = 'black', check_overlap = TRUE)
  choropleth = choropleth + scale_fill_gradient2(low = "gray", mid = "yellow", high = "red", midpoint=0.04, labels = percent, limits=c(0, 0.082))
  choropleth = choropleth + theme(text = element_text(size=10))
  
  choropleths[[index]] = choropleth
  
  index = index + 1
}

setwd(paste0("C:/Users/user/Documents/5702_Project/results/", keyword))
choroplethr_animate(choropleths)

census_api_key(<API_KEY>, install = TRUE)

pop <- get_acs(geography = "state", variables = c(popluation = "B00001_001"), year=2010)
pop$NAME <- tolower(pop$NAME)
colnames(pop) <- c('GEOID', 'region', 'variable', 'estimate')
pop$estimate <- pop$estimate/1000
totalList <- merge(totalList, pop, by='region') 


