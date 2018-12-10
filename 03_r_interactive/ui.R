ui <- fluidPage(
  fluidRow(
    h2("Law cases - Exploratory analysis")
  ),
  fluidRow(
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(condition = "input.conditionedPanels === 'time'", uiOutput("input_keyword_time")),
      conditionalPanel(condition = "input.conditionedPanels === 'geo'", uiOutput("input_keyword_geo")),
      conditionalPanel(condition = "input.conditionedPanels === 'deep'", uiOutput("input_searchterm")),
      conditionalPanel(condition = "input.conditionedPanels === 'time'", uiOutput("input_states")),
      uiOutput("input_year_slider")
    ),
    
    mainPanel(
      tabsetPanel(id = 'conditionedPanels',
        tabPanel("Time analysis", source('./ui/ui_timeseries.R', local=TRUE)$value, value='time'), 
        tabPanel("Geo analysis", source('./ui/ui_geo.R', local=TRUE)$value, value='geo'), 
        tabPanel("Case deep dive", source('./ui/ui_deepdive.R', local=TRUE)$value, value='deep')
      )
    )
  )
  )
)