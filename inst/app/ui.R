require(shinydashboard)
require(shinyjs)
header <- dashboardHeader(title = 'Invenergy')

#Icons:  http://fontawesome.io/icons/ or http://getbootstrap.com/components/#glyphicons
# Good ones:  sun, cloud-sun, cloud-sun-rain, solar-panel
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem('File Upload', tabName = 'uploads', icon = icon('angle-double-up')),
  menuItem('Onsite Flags', tabName = 'flags', icon = icon('solar-panel')),
  menuItem('Historical Statistics', tabName = 'stats', icon = icon('sun')),
  menuItem('Concurrent Statistics', tabName = 'concurrent', icon = icon('cloud')),
  menuItem('Model Statistics', tabName = 'model', icon = icon('snowflake')),
  menuItem('Adjusted TMY', tabName = 'tmy', icon = icon('bolt'))
))

body <- dashboardBody(tabItems(
  # Zero tab 'Uploads' ----
  tabItem(tabName = 'uploads',
          fluidRow(
            box(width = 4, status = 'info',
                fileInput("sms_upload", "Upload Groundwork Timeseries Data",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            box(width = 4, status = 'info',
                fileInput("cpr_upload", "Upload CPR Timeseries Data",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            box(width = 4, status = 'info',
                fileInput("vaisala_upload", "Upload Vaisala Timeseries Data",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            )
          ),
          fluidRow(
            box(width = 4, status = 'warning',
                tableOutput('sms_preview')
            ),
            box(width = 4, status = 'warning',
                tableOutput('cpr_preview')
            ),
            box(width = 4, status = 'warning',
                tableOutput('vaisala_preview')
            )
          ),
          fluidRow(
            div(align = 'center',
                actionButton('compile_megaframe', h4(tags$b('Start The Party')), class = 'btn btn-warning'),
                h4('Please make sure the modeled datasets cover the period of record of the SMS.', tags$br(),
                   'If so, hit the button.', tags$br(),
                   tags$b('You know you want to.')),
                # Div is hidden until compile_megaframe button is done running.
                div(id='done', h2('Done Compiling!  Start Exploring!'))
            )
          )
  ),
  # First tab 'Flags' ----
  tabItem(tabName = 'flags',
          fluidRow(
            box(width = 12, status = 'primary',
                h1('What is the quality of the measured data?')
            )
          ),
          # Boxes need to be put in a row (or column)
          fluidRow(
            box(plotOutput('plot_flags'), status = 'success', width = 10),
            box(width = 2, status = 'warning',
                h4('Look at the time series data to see if anything appears out of the ordinary.'),
                h4('Pay close attention to the flagged data, which is any color other than 1 in the key.
                   The bar chart below shows which months have the most flagged data.'),
                h4('Were all the bad data points flagged?  Can you find something that looks like a systemic error?
                   (Maybe the station is slightly facing the East, causing higher morning irradiance and lower evening irradiance than expected.')
            )
          ),
          # Row of inputs to plot_flags
          fluidRow(
            box(title = 'Time Series Inputs', status = 'info', width = 6,
                column(3, uiOutput('rv_sensor_ts')),
                column(2), # Just to create separation
                column(1, tags$span(style = 'float: right', actionButton('date_previous', '<'))),
                column(5, uiOutput('rv_date_flags')),
                column(1, tags$span(style = 'float: left', actionButton('date_next', '>')))
            ),
            box(title = 'Scatter Plot Inputs', status = 'info', width = 6,
                column(2, uiOutput('rv_year_scatter')),
                column(2, uiOutput('rv_month_scatter')),
                column(2, uiOutput('rv_sensorx_scatter')),
                column(2, uiOutput('rv_sensory_scatter')),
                column(2, uiOutput('rv_sensorflag_scatter'))
            )
          ),
          # Bar plots showing percent of flags per month
          fluidRow(
            box(plotOutput('bar_flags'), status = 'success', width = 5, height = 650),
            box(plotOutput('sms_scatter'), status = 'success', width = 5, height = 650),
            box(width = 2, status = 'warning', height = 650,
                h4('How do the sensors compare?'),
                h4('Plot different sensors against each other and look at the different flags (shown by color).'),
                h4('Is there a lot of variability between sensors?'),
                h4('Are bad datapoints being flagged?'),
                h4('Are flags applied to the correct sensor?'),
                h4('How do they differ during months with lots of flags and months with few flags?')
            )
          ),
          # Flag info sections
          fluidRow(
            box(tableOutput('flag_defs'), status = 'warning', width = 12, align = 'center')
          ),
          fluidRow(
            box(title = 'Flag Definitions', status = 'warning', width = 12, align = 'center',
                tags$b('Kt Test:'), '  Kt is the ratio GHI/extraterrestrial horizontal radiation. When solar elevation angle was > 10 degrees, a QC code of 10 was assigned when Kt fell outside the range of 0 to 1.20. For solar elevation angles between 0 and 10 degrees, a QC code of 10 was assigned when Kt fell outside the range of 0 to 0.80. A QC code of 10 was assigned to any irradiance values < -10 W/m2, or when irradiance was > 10 W/m2 for solar elevation angles < 0. Observations passing each of these tests were given a QC code of 1.',
                tags$br(),
                tags$b('Regression (r2) Test:'), '  Two GHI values were compared using linear regression. GHI observations with a value of the residuals from regression greater than 15% different from the regression line had 11 added to their QC code value.',
                tags$br(),
                tags$b('Boxplot (Ratio) Test:'), '  The ratio of two GHI values was evaluated according to whether they were within 10% (low solar elevation angles, 5 to 15 degrees), 5% (solar elevation angles 15 to 30 degrees), or 3.5% (solar elevation angle > 30 degrees) of one another. If they were outside of these defined ranges, 12 was added to the QC code for both GHI measurements.',
                tags$br(),
                tags$b('Missing Values:'), '  Missing values were assigned a code of 99.',
                tags$br(),
                tags$b('Invenergy Flags:'), '  Any outlier flags Invenergy applies will be within the range of 100-999.',
                tags$br(),
                tags$b('Missing Data Points:'), '  Any lost data points (gaps in the time series) are flagged 1000.'
            ))
  ),

  # Second tab 'Stats' ----
  tabItem(tabName = 'stats',
          fluidRow(
            box(h1('How have the satellite-based models compared historically?'), width = 12, status = 'primary')
          ),
          fluidRow(
            box(width = 9, status = 'success',
                plotOutput('lt_ghi_monthly')
            ),
            box(width = 3, status = 'warning',
                h3('How does the average monthly irradiance compare for each model?'),
                h3('Are there some months that a model is way off-base from all the other ones?'),
                h3('If this site gets snow, are some models particularly bad during the winter months?  (Looking at you CPRv3.2 and CPRv3.3.)')
            )
          ),
          fluidRow(
            box(width = 9, status = 'success',
                plotOutput('annual_ghi')),
            box(width = 3, status = 'warning',
                h3('How does the long-term annual performance compare?'),
                h3('Are the some years taht a model is way off-base from all the others?'),
                h3('Do some models have a lot higher Inter-Annual Variability than the others?'),
                h3('Are the models trending upward or downward?  (Hint: Use the normalize feature to divide every model by its average.)'))
          ),
          fluidRow(
            box(checkboxInput('annual_normalized', 'Normalized'),
                width = 3, align = 'center', status = 'info'),
            box(uiOutput('rv_box_year'),
                width = 9, align = 'center', status = 'info')
          ),
          fluidRow(
            box(plotOutput('box_ghi'), width = 9, status = 'success'),
            box(width = 3, status = 'warning',
                h3('If you want a closer look at one of the years, view a boxplot of each month here.'),
                h3('Are there a lot of outliers?  How do the quartiles vary for each model?'))
          )
  ),

  # Third tab 'Concurrent' ----
  tabItem(tabName = 'concurrent',
          fluidRow(
            box(width = 12, status = 'primary',
                h1('How do the satellite-based models compare to the measured data?')
            )
          ),
          fluidRow(
            box(width = 9, status = 'success',
                plotOutput('concur_ghi_monthly')
            ),
            box(width = 3, status = 'warning',
                h3('How does the average monthly irradiance compare between models and onsite data?'),
                h3('Is there a consistent bias with each model, or does the model both overpredict and underpredict?'),
                h3('Does this site get snow?  How do the datasets compare in the winter months?'),
                h3('Do any of these months line up with months that had poor measured-data quality?')
            )
          ),
          fluidRow(
            box(title = 'Scatter Plot Inputs', width = 12, status = 'info',
                column(width = 4,
                       selectInput('sensor_scatter', 'Sensor', c('ghi', 'temp', 'ws'))
                ),
                column(width = 5,
                       uiOutput('rv_model_scatter')
                )
            )
          ),
          fluidRow(
            box(plotOutput('scatter_concurrent'), width = 9, status = 'success'),
            box(width = 3, status = 'warning',
                h3('Look at each model performance in more detail.'),
                h3('How well does the model corelate to the onsite data?  Is there a tight variance (r2)?'),
                h3('Is there a consistent bias to underpredict or overpredict?'),
                h3('Do some months do better than others?'),
                h3('Do any of these months have poor measured-data quality?')
            )
          ),
          fluidRow(
            box(plotOutput('boxplot_concurrent'), width = 12, status = 'success')
          )
  ),

  # Fourth tab 'Model Statistics' ----
  tabItem(tabName = 'model',
          fluidRow(
            box(width = 12, status = 'primary',
                h1('How well do the adjusted models predict the measured data?')
            )
          ),
          fluidRow(
            box(tableOutput('day_table'), title = 'Day Time Step', width = 5, align = 'center', status = 'warning'),
            box(tableOutput('hour_table'), title = 'Hour Time Step', width = 5, align = 'center', status = 'warning'),
            box(width = 2, status = 'warning',
                h4('The goal is to pick the model adjustment that is the most consistent.  This means it has variance
                   (r.squared) close to 1 and few outliers.'),
                h4('Each sensor is ordered by rank of the variance of the model fits (r.squared).'),
                h4('All things equal, favor should be given to models that are the latest version as they have the most support.'),
                h4('Any models that behaved poorly on any previous metrics should be ruled out.')
            )
          ),
          fluidRow(
            box(width = 10, status = 'info',
                selectInput('model_plot_sensor', 'Model Sensor for Plot', c('ghi', 'temp', 'ws'))
            ),
            box(width = 2, status = 'warning', align = 'center',
                textInput('site_name', 'Project Name'),
                downloadButton('download_report_btn', tags$b('Download the MCP Report'))
            )
          ),
          fluidRow(
            box(plotOutput('fits_daily'), width = 5, status = 'success'),
            box(plotOutput('fits_hourly'), width = 5, status = 'success'),
            box(width = 2, status = 'warning',
                h4('These plots show the adjusted model predictions of the onsite data.'),
                h4('Look for any unreasonable predictions.'),
                h4('The spread should be very tight around a line going through the origin with slope = 1.'))
          ),
          fluidRow(
            box(plotOutput('residuals_daily'), width = 5, status = 'success'),
            box(plotOutput('residuals_hourly'), width = 5, status = 'success'),
            box(width = 2, status = 'warning',
                h4('These plots show the error of each adjusted model prediction of the onsite data.'),
                h4('Look for really big values or patterns that represent the model is missing something.'),
                h4('The spread should be tight and centered around zero.')
            )
          )

  ),

  # Fifth tab 'TMY' ----
  tabItem(tabName = 'tmy',
          fluidRow(
            box(width = 12, status = 'primary',
                h1('Pick the best model and use it to adjust our typical meteorological year.')
            )
          ),
          fluidRow(
            box(width = 3, status = 'info',
                radioButtons('tmy_format', 'TMY Format', c('vaisala', 'cpr')),
                fileInput("tmy_upload", "Upload Unadjusted Typical Meteorological Year",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            box(width = 3, status = 'warning',
                tableOutput('tmy_preview')
            ),
            box(width = 3, status = 'info',
                uiOutput('rv_tmy_adj_model'),
                radioButtons('tmy_adj_timestep', 'Which timestep should be used?',
                             c('daily', 'hourly')),
                checkboxGroupInput('tmy_adj_sensors', 'Which sensors should be adjusted?',
                                   c('ghi', 'temp', 'ws'), selected = c('ghi', 'temp')),
                div(style = 'float: right', actionButton('tmy_adj_btn', h4(tags$b('Adjust TMY!')), class = 'btn btn-warning'))
            ),
            box(width = 3, status = 'warning', align = 'center',
                tableOutput('adjusted_tmy_preview'),
                uiOutput('rv_download_tmy_btn')
            )
          ),
          fluidRow(
            box(width = 6, status = 'success',
                plotOutput('tmy_adj_plot')
            ),
            box(width = 3, status = 'warning',
                tableOutput('tmy_adj_table')
            ),
            box(width = 3, status = 'warning',
                h3('Does the adjusted model match expectations?'),
                h3('Check that the annual sum/mean matches the table from the previous tab.'),
                h3('Check the plots for any odd behavior or see if there are any strange values in the table.')
            )
          )
  )
))

# Sixth no more tabs ----

ui <- dashboardPage(header, sidebar, body, skin = 'green', useShinyjs())
