server <- function(input, output, session) {

  # Hide the text box telling user to start exploring until data is compiled
  hide('done')

  # Where I will save all of the compiled datasets
  rv <- reactiveValues()

  # TODO: Shouldn't need this...
  rv$mcp <- NULL
  rv$tmy_adjusted <- NULL

  # Zero tab 'Inputs' ----
  output$sms_preview <- renderTable({
    # input$sms_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    if(is.null(input$sms_upload)) {
      df <- data.frame(Instructions =
                         'Please upload all of the SMS files at once.  These
                       should be CSVs and are typically named with an ending
                       "QCData".')
    } else {
      if(str_to_lower(tools::file_ext(input$sms_upload$datapath)) != 'csv') {
        df <- data.frame(Error =
                           'Please make sure the file is a CSV!')
      } else {
        # Run custom function to parse SMS CSVs
        # The result will be the sms_flagged dataframe, which we want to save
        rv$sms_flagged <- read_groundwork(input$sms_upload$datapath)

        df <- tibble('Number Of Files' = length(input$sms_upload$datapath),
                     'Period of Record (Months)' = time_length(
                       max(rv$sms_flagged$datetime) -
                         min(rv$sms_flagged$datetime), 'month'),
                     'End Date' = as.character(max(rv$sms_flagged$datetime))
        )

      }
    }

    return(df)
  })


  output$cpr_preview <- renderTable({
    # Similar structure to SMS
    if(is.null(input$cpr_upload)) {
      df <- data.frame(Instructions =
                         'Please upload all of the CPR TimeSeries files at once.')
    } else {
      #TODO: Bug: Only checks first file in list.  Should check all of them.
      if(str_to_lower(tools::file_ext(input$cpr_upload$datapath)) != 'csv') {
        df <- data.frame(Error =
                           'Please make sure the file is a CSV!')
      } else {
        rv$cpr_list <- read_cpr(input$cpr_upload$datapath)

        df <- tibble(Versions = names(rv$cpr_list),
                     'Start Date' = map(rv$cpr_list,
                                        function(x)
                                          as.character(min(x$datetime))),
                     'End Date' = map(rv$cpr_list,
                                      function(x) as.character(max(x$datetime)))
        )
      }
    }

    return(df)
  })


  output$vaisala_preview <- renderTable({
    # Similar structure to SMS
    if(is.null(input$vaisala_upload)) {
      df <- data.frame(Instructions =
                         'Please upload all of the vaisala TimeSeries files at
                       once.')
    } else {
      #TODO: Bug: Only checks first file in list.  Should check all of them.
      if(str_to_lower(tools::file_ext(input$vaisala_upload$datapath)) != 'csv') {
        df <- data.frame(Error =
                           'Please make sure the file is a CSV!')
      } else {
        rv$vaisala_list <- read_vaisala(input$vaisala_upload$datapath)

        df <- tibble(Versions = names(rv$vaisala_list),
                     'Start Date' = map(rv$vaisala_list,
                                        function(x)
                                          as.character(min(x$datetime))),
                     'End Date' = map(rv$vaisala_list,
                                      function(x) as.character(max(x$datetime)))
        )
      }
    }

    return(df)
  })


  observeEvent(input$compile_megaframe, {

    showNotification('Filtering SMS Data...')
    megaframe <- prepare_sms_flagged_for_megaframe(rv$sms_flagged) %>%
      compile_megaframe(rv$cpr_list, rv$vaisala_list)

    showNotification('Running the Linear Regression...')
    rv$mcp <- MCP(megaframe)
    rv$mcp$run()

    show('done')

    # Trying to keep things light.  I dont need these saved anymore,
    # so, I'm going to remove them from memory.
    rv$cpr_list <- NULL
    rv$vaisala_list <- NULL
  })

  # First tab 'Flags' ----

  output$rv_sensor_ts <- renderUI({
    selectInput('sensor_ts', label = NULL,
                # List of variables that can be plotted
                intersect(c('ghi1', 'ghi2', 'ghi3', 'ghi4', 'temp', 'ws'), names(rv$sms_flagged)))
  })


  output$rv_date_flags <- renderUI({
    dateRangeInput('date_flags', label = NULL,
                   start = rv$sms_flagged$datetime[1], end = rv$sms_flagged$datetime[24*7*60],
                   min = rv$sms_flagged$datetime[1], max = rv$sms_flagged$datetime[length(rv$sms_flagged$datetime)])
  })


  output$plot_flags <- renderPlot({
    sensor <- input$sensor_ts
    flag <- paste(sensor, '_flag', sep = '')
    date_start <- as_datetime(input$date_flags[1])
    date_end <- as_datetime(input$date_flags[2])

    plot_time_series_range(rv$sms_flagged, sensor, flag, date_start, date_end,
                           ylab = sensor, color_lab = 'Flag')
  })


  observeEvent(input$date_previous, {
    current_start <- as_datetime(input$date_flags[1])
    current_end <- as_datetime(input$date_flags[2])
    current_interval <- current_end - current_start
    new_start <- current_start - current_interval

    if (new_start < rv$sms_flagged$datetime[1]) {
      new_start <- rv$sms_flagged$datetime[1]
    }

    new_end <- new_start + current_interval

    updateDateRangeInput(session, 'date_flags',
                         end = new_end,
                         start = new_start)
  })


  observeEvent(input$date_next, {
    current_start <- as_datetime(input$date_flags[1])
    current_end <- as_datetime(input$date_flags[2])
    current_interval <- current_end - current_start
    new_end <- current_end + current_interval

    if (new_end > rv$sms_flagged$datetime[length(rv$sms_flagged$datetime)]) {
      new_end <- rv$sms_flagged$datetime[length(rv$sms_flagged$datetime)]
    }

    new_start <- new_end - current_interval

    updateDateRangeInput(session, 'date_flags',
                         start = new_start,
                         end = new_end
    )
  })


  output$bar_flags <- renderPlot({
    flag_count <- count_flags(rv$sms_flagged)
    plot_flag_count_bar_chart(flag_count)
  }, height = 600)


  output$flag_defs <- renderTable(
    expr = read_csv(system.file("app", "Groundwork_Flags.csv", package = "powerResource")),
    align = 'c',
    digits = 0
  )


  output$rv_sensorx_scatter <- renderUI({
    selectInput('sensorx_scatter', label = 'x-axis',
                intersect(c('ghi1', 'ghi2', 'ghi3', 'ghi4'), names(rv$sms_flagged)))
  })


  output$rv_sensory_scatter <- renderUI({
    selectInput('sensory_scatter', label = 'y-axis',
                # Reverse order to make default selected more interesting
                intersect(c('ghi4', 'ghi3', 'ghi2', 'ghi1'), names(rv$sms_flagged)))
  })


  output$rv_sensorflag_scatter <- renderUI({
    selectInput('sensorflag_scatter', label = 'color',
                intersect(c('ghi1_flag', 'ghi2_flag', 'ghi3_flag', 'ghi4_flag'), names(rv$sms_flagged)))
  })


  output$rv_year_scatter <- renderUI({
    selectInput('year_scatter', label = 'year',
                unique(rv$sms_flagged$year))
  })


  output$rv_month_scatter <- renderUI({
    selectInput('month_scatter', label = 'month',
                unique(rv$sms_flagged$month))
  })


  output$sms_scatter <- renderPlot({
    y <- input$year_scatter
    m <- as.numeric(input$month_scatter)

    rv$sms_flagged %>%
      filter(year == y, month == m) %>%
      plot_sensor_v_sensor_scatter(input$sensorx_scatter,
                                   input$sensory_scatter,
                                   input$sensorflag_scatter,
                                   title = paste(month.name[m], y))
  })


  # Second tab 'Stats' ----

  output$rv_box_year <- renderUI({
    sliderInput('box_year', 'Year of Boxplot', min(rv$mcp$megaframe$year), max(rv$mcp$megaframe$year), 2018, step = 1, sep = '')
  })


  output$lt_ghi_monthly <- renderPlot({
    rv$mcp$megaframe %>%
      select_longterm_models() %>%
      summarise_monthly_total_ghi() %>%
      plot_monthly_ghi_summary(title = 'Long Term Average GHI')
  })


  output$annual_ghi <- renderPlot({
    if (input$annual_normalized) {
      data <- 'normalized'
      data_label <- 'Normalized GHI'
    } else {
      data <- 'ghi'
      data_label <- 'Annualized Total GHI'
    }

    rv$mcp$megaframe %>%
      select_longterm_models() %>%
      summarise_annual_total_ghi() %>%
      add_normalized_ghi() %>%
      plot_annual_model_values(data, data_label)
  })

  output$box_ghi <- renderPlot({
    box_year <- input$box_year

    rv$mcp$megaframe %>%
      filter(ghi > 0, year == box_year) %>%
      group_by(month, model) %>%
      plot_monthly_ghi(title = box_year)
  })


  # Third tab 'Concurrent' ----

  output$rv_model_scatter <- renderUI({
    selectInput('model_scatter', 'Model', unique(rv$mcp$megaframe$model)[-1])
  })


  output$concur_ghi_monthly <- renderPlot({
    rv$mcp$megaframe %>%
      filter_out_missing_sms_ghi() %>%
      summarise_monthly_total_ghi() %>%
      plot_monthly_ghi_summary(title = 'Concurrent Data Monthly Average')
  })


  output$scatter_concurrent <- renderPlot({
    model_input <- input$model_scatter
    sensor <- input$sensor_scatter

    rv$mcp$megaframe %>%
      spread_models_vs_sms_for_sensor(sensor) %>%
      monthly_scatter_plots_sms_vs_model(model_input, sensor)
  })


  output$boxplot_concurrent <- renderPlot({
    rv$mcp$megaframe %>%
      prepare_megaframe_for_concurrent_monthly_plots() %>%
      plot_monthly_ghi(title = '')
  })


  # Fourth tab 'Model' ----

  output$download_report_btn <- downloadHandler(
    filename = 'mcp_report.html',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- system.file("app", "reports/report.Rmd", package = "powerResource")
      file.copy('reports/report.Rmd', tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(site_name = input$site_name,
                     sms_flagged = rv$sms_flagged,
                     mcp = rv$mcp)

      showNotification('Creating Report.  A PDF will open upon completion.  This may take a minute...')

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  output$day_table <- renderTable(
    expr = rv$mcp$get_daily_results_table(),
    digits = 3,
    align = 'c'
  )


  output$hour_table <- renderTable(
    expr = rv$mcp$get_hourly_results_table(),
    digits = 3,
    align = 'c'
  )


  # Fitted model vs SMS for all models
  output$fits_hourly <- renderPlot({
    timestep <- 'hourly'
    value <- 'fitted.values'

    grab_model_list(rv$mcp$model_fits, timestep, input$model_plot_sensor, value) %>%
      plot_model_comparison(value, title = paste(timestep, input$model_plot_sensor, value))
  })


  output$fits_daily <- renderPlot({
    timestep <- 'daily'
    value <- 'fitted.values'

    grab_model_list(rv$mcp$model_fits, timestep, input$model_plot_sensor, value) %>%
      plot_model_comparison(value, title = paste(timestep, input$model_plot_sensor, value))
  })


  # Residuals of fitted model vs SMS for all models
  output$residuals_hourly <- renderPlot({
    timestep <- 'hourly'
    value <- 'residuals'

    grab_model_list(rv$mcp$model_fits, timestep, input$model_plot_sensor, value) %>%
      plot_model_comparison(value, title = paste(timestep, input$model_plot_sensor, value))
  })


  output$residuals_daily <- renderPlot({
    timestep <- 'daily'
    value <- 'residuals'

    grab_model_list(rv$mcp$model_fits, timestep, input$model_plot_sensor, value) %>%
      plot_model_comparison(value, title = paste(timestep, input$model_plot_sensor, value))
  })


  # Fifth tab 'TMY' ----

  output$rv_tmy_adj_model <- renderUI({
    req(rv$mcp)

    if (input$tmy_adj_timestep == 'daily') {
      ltra_table <- rv$mcp$get_daily_results_table()
    } else {
      ltra_table <- rv$mcp$get_hourly_results_table()
    }
    options <- ltra_table %>%
      filter(sensor == 'ghi') %>%
      select(model)

    radioButtons('tmy_adj_model', 'Which model should be used for the adjustment?',
                 options$model
    )
  })


  output$rv_download_tmy_btn <- renderUI({
    req(rv$tmy_adjusted)
    downloadButton('download_tmy_btn', tags$b('Download the Adjusted TMY'))
  })


  output$tmy_preview <- renderTable({
    # input$tmy_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    if(is.null(input$tmy_upload)) {
      df <- data.frame(Instructions =
                         'Please upload a Typical Meteorological Year from the
                       same satellite model that you plan to use for the
                       adjustment.')
    } else {
      if(str_to_lower(tools::file_ext(input$tmy_upload$datapath)) != 'csv') {
        df <- data.frame(Error =
                           'Please make sure the file is a CSV!')
      } else {

        if(input$tmy_format == 'vaisala') {
          rv$tmy_unadjusted <- read_csv(input$tmy_upload$datapath, skip = 11) %>%
            tidy_vaisala
        } else {
          rv$tmy_unadjusted <- read_csv(input$tmy_upload$datapath, skip = 1) %>%
            tidy_cpr
        }

        if(length(rv$tmy_unadjusted$datetime) == 8760) {
          df <- head(rv$tmy_unadjusted) %>%
            mutate(datetime = as.character(datetime)) %>%
            select(-year, -month)
        } else{
          df <- data.frame(Error = 'Please make sure the length of this TMY is
                          8760 hours.  You might have selected a time series file.')
        }

      }
    }

    return(df)
  })


  observeEvent(input$tmy_adj_btn, {
    rv$tmy_adjusted <- scale_tmy(rv$mcp$model_fits, rv$mcp$megaframe,
                                 rv$tmy_unadjusted, input$tmy_adj_model,
                                 input$tmy_adj_timestep, input$tmy_adj_sensors)
  })


  output$adjusted_tmy_preview <- renderTable({
    if(is.null(rv$tmy_adjusted)) {
      df <- data.frame(Instructions =
                         'Once you have input a TMY and selected the correct model, timestep, and sensors to adjust, please
                       press the Adjust TMY button.')
    } else {
      df <- head(rv$tmy_adjusted) %>%
        mutate(datetime = as.character(datetime)) %>%
        select(-month, -year)
    }

    return(df)
  })


  output$download_tmy_btn <- downloadHandler(
    filename = function() {
      paste0('scaled_tmy_', input$tmy_adj_model, ".csv")
    },
    content = function(file) {
      write_csv(select(rv$tmy_adjusted, -month, -year), file)
    }
  )


  output$tmy_adj_table <- renderTable({
    req(rv$tmy_adjusted)

    tmy <- select(rv$tmy_adjusted, ghi, dhi, dni, temp, ws)

    df <- bind_rows(
      sapply(tmy, max),
      sapply(tmy, min),
      sapply(tmy, mean),
      sapply(tmy, function(x) sum(x) / 1000)
    ) %>%
      mutate(metric = c('max', 'min', 'mean', 'sum (kWh)')) %>%
      select(metric, everything())

    df$temp[4] <- NA
    df$ws[4] <- NA

    return(df)
  })


  output$tmy_adj_plot <- renderPlot({
    req(rv$tmy_adjusted)

    rv$tmy_adjusted %>%
      mutate(hour = hour(datetime)) %>%
      group_by(month, hour) %>%
      summarise_all(mean) %>%
      gather('key', 'irradiance', ghi, dhi, dni) %>%
      ggplot(aes(x = hour, y = irradiance, color = key)) +
      geom_path() +
      facet_wrap(~month)
  })


}
