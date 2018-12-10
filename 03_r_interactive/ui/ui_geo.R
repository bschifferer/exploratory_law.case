fluidPage(
  fluidRow(
    uiOutput("header_geo")
  ),
  fluidRow(
    uiOutput("plot_geo_if")
  ),
  fluidRow(
    tags$br(),
    tags$p('Note: The scale is between 0 and the 99.5% percentile')
  )
)