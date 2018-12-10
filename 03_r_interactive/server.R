function(input, output, session) {
  
  ### Filters
  output$input_keyword_time <- renderUI({
    selectInput("keyword_time", 
                "Keywords:",
                casetypes_time$label,
                selected = 'Sexual Harassment',
                multiple = FALSE)
  })
  
  output$input_keyword_geo <- renderUI({
    selectInput("keyword_geo", 
                "Keywords:",
                casetypes_geo$label,
                selected = 'Sexual Harassment',
                multiple = FALSE)
  })
  
  output$input_searchterm <- renderUI({
    textInput("searchterm", 
              "Searchterm:", 
              value = "murder", 
              )
  })
  
  output$input_year_slider <- renderUI({
    sliderInput("input_year_slider", "Year:", 
                min=min_year, 
                max=max_year, 
                value=c(1980,max_year),
                sep="")
  })
  
  output$input_states <- renderUI({
    selectInput("state", 
                "State:",
                filter_states,
                multiple = TRUE)
  })
  
  ### Data
  jsonDeepDive <- reactive({
    req(input$searchterm)
    return(getCase(input$searchterm, input$input_year_slider[1], input$input_year_slider[2]))
  })
  
  data_total_filtered <- reactive({
    req(input$input_year_slider)
    return(df_total %>%
             filter(year >= input$input_year_slider[1] & year <= input$input_year_slider[2]) %>%
             filter(state %in% input$state | is.null(input$state))
    )
  })
  
  data_cases_filtered_time <- reactive({
    req(input$input_year_slider)
    filter_str = ''
    if (!is.null(input$keyword_time)) {
      for (keyword in c(input$keyword_time)) {
        filter_str = paste0(filter_str, '|', casetypes[which(casetypes$label==keyword), 'columnname'], '==1')
      }
    }
    filter_str = substr(filter_str, 2, nchar(filter_str))
    return(df_cases %>%
             filter(year >= input$input_year_slider[1] & year <= input$input_year_slider[2]) %>%
             filter(state %in% input$state | is.null(input$state)) %>%
             filter_(.dots = filter_str)
    )
  })
  
  data_total_filtered_geo <- reactive({
    req(input$input_year_slider)
    return(df_total %>%
             filter(year >= input$input_year_slider[1] & year <= input$input_year_slider[2])
    )
  })
  
  data_cases_filtered_geo <- reactive({
    req(input$input_year_slider)
    filter_str = ''
    if (!is.null(input$keyword_geo)) {
      for (keyword in c(input$keyword_geo)) {
        filter_str = paste0(filter_str, '|', casetypes[which(casetypes$label==keyword), 'columnname'], '==1')
      }
    }
    filter_str = substr(filter_str, 2, nchar(filter_str))
    return(df_cases %>%
             filter(year >= input$input_year_slider[1] & year <= input$input_year_slider[2]) %>%
             filter_(.dots = filter_str)
    )
  })
  
  data_filtered_geo <- reactive({
    req(input$input_year_slider)
    req(input$keyword_geo)
    df_total_tmp <- data_total_filtered_geo() 
    df_total_cases_tmp <- data_cases_filtered_geo()
    if(dim(df_total_cases_tmp)[1]==0) {
      return('EMPTY')
    } else {
      df_total_tmp <- df_total_tmp %>% 
        group_by(year, state, States_Code) %>% 
        summarise(count = sum(count, na.rm = TRUE)) %>% 
        ungroup()
      df_total_cases_tmp <- df_total_cases_tmp %>% 
        group_by(year, state, States_Code) %>% 
        summarise(count = sum(count, na.rm = TRUE)) %>% 
        ungroup()
      colnames(df_total_tmp) <- c('year', 'state', 'States_Code', 'total_count')
      df_total_tmp <- df_total_tmp %>%
        left_join(df_total_cases_tmp, by=c('year'='year', 'state'='state', 'States_Code'='States_Code'))
      df_total_tmp <- df_total_tmp %>%
        mutate(count = ifelse(is.na(count),0,count)) %>%
        mutate(df_total_tmp = ifelse(is.na(total_count),0,total_count))
      df_total_tmp$perc <- df_total_tmp$count/df_total_tmp$total_count
      df_total_tmp$Percent <- round(df_total_tmp$perc*100,1)
      df_total_tmp$perc_label <- paste0(round(df_total_tmp$perc*100,1),'%')
      df_total_tmp$plot_label <- paste0('State: ', df_total_tmp$state, '<br>', 'Share: ', df_total_tmp$perc_label)
      return(df_total_tmp)
    }
  })
  
  ### Plots
  output$plot_geo_if <- renderUI({
    req(input$keyword_geo)
    df2 <- data_filtered_geo()
    if(df2 == 'EMPTY') {
      return("Current selection of filters has no data")
    } else {
      return(plotlyOutput("plot_geo"))
    }
  })
  
  output$plot_geo <- renderPlotly({
    df2 <- data_filtered_geo()
    plot_geo(df2, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Percent, 
        text = ~plot_label, 
        locations = ~States_Code, 
        frame= ~year,
        color = ~Percent, 
        colors = 'Blues',
        zmin=0, 
        zmax=quantile(df2$Percent,0.995)
      )  %>%
      layout(
        geo = g
      ) %>%
      animation_opts(frame = 200, transition = 200) %>%
      animation_slider(
        currentvalue = list(prefix = "Year ")
      )
  })
  
  output$plot_time_if <- renderUI({
    req(input$keyword_time)
    if (input$keyword_time == 'All') {
      df1 <- data_total_filtered()
    } else {
      df1 <- data_cases_filtered_time()
    }
    if(dim(df1)[1]==0) {
      return("Current selection of filters has no data")
    } else {
      return(plotlyOutput("plot_time"))
    }
  })
  
  output$plot_time <- renderPlotly({
    req(input$keyword_time)
    if (input$keyword_time == 'All') {
      df1 <- data_total_filtered()
    } else {
      df1 <- data_cases_filtered_time()
    }
    plot_ly(df1 %>% 
              group_by(year) %>% 
              summarise(count = sum(count, na.rm = TRUE)) %>% 
              ungroup() %>%
              accumulate_by(~year), 
            x = ~year, 
            y = ~count, 
            frame = ~frame, 
            type = 'scatter',
            mode = 'lines', 
            line = list(simplyfy = F)) %>%
      layout(xaxis = list(range = c(min(df1$year), max(df1$year)),
                          title = 'Year'),
             yaxis = list(title = 'No. of cases')) %>%
      animation_opts(frame = 200, transition = 200) %>%
      animation_slider(
        currentvalue = list(prefix = "Year ")
      )
  })
  
  output$plot_deep_if <- renderUI({
    req(input$searchterm)
    if(input$searchterm == '') {
      return('Please enter a keyword')
    } else {
      return(reactjsonOutput( "rjed" ))
    }
  })
  
  output$rjed <- renderReactjson({
    reactjson(
      jsonDeepDive(),
      name = 'case',
      theme = 'colors',
      width = 400,
      height = 800,
      onEdit = TRUE,
      onAdd = TRUE, 
      onDelete = TRUE, 
      onSelect = TRUE,
      enableClipboard = FALSE
    )
  })
  
  ## Header
  output$header_time <- renderUI({
    req(input$keyword_time)
    h3(paste0('No of law cases for: ', input$keyword_time))
  })
  
  output$header_geo <- renderUI({
    req(input$keyword_geo)
    h3(paste0('Share of law cases with keyword ', input$keyword_geo, ' per state'))
  })
  
  output$header_deep <- renderUI({
    req(input$searchterm)
    h3(paste0('Example case for keyword: ', input$searchterm))
  })
}
