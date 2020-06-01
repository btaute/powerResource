#' Calculate the results of the MCP for all timesteps and sensors
#'
#' Build the dataframe that is saved in MCP$results
#' Gets longterm metrics from the satellite models, gets performance metrics of the model fits, and calculates longterm adjustments
#' @param mcp_list A named list of model fits to onsite data; The output of run_mcp_for_all_timesteps_and_sensors()
#' @param megaframe A megaframe with at least one satellite model and the sms model
#' @param model_coefficient_is_0 Whether or not the models in mcp_list forced the intercept coefficient to zero
#' @return A dataframe that contains all of the result metrics for the mcp runs in mcp_list
#' @examples
#' \dontrun{
#' my_mcp <- run_mcp(megaframe)
#' get_mcp_restults(my_mcp, megaframe)
#' }
#' @export
get_mcp_results <- function(mcp_list, megaframe, model_coefficient_is_0 = FALSE) {
  lt_resource <- get_longterm_resource(megaframe)
  adj_resource <- get_longterm_adjustment(mcp_list, lt_resource, model_coefficient_is_0)
  model_metrics <- get_model_metrics(mcp_list)
  lt_metrics <- get_longterm_metrics(megaframe)

  full_df <- combine_mcp_results_df(lt_resource, adj_resource, model_metrics, lt_metrics) %>%
    add_rom_uncertainty()

  return(full_df)
}


#' Get Longterm Resource
#'
#' Get the longterm resource for all the models and sensors in a megaframe
#' @param megaframe The dataframe containing the longterm satellite models and resources
#' @return Returns the longterm average for wind speed or temperature and the average annual sum in kWh/m2 for ghi
#' @export
get_longterm_resource <- function(megaframe) {
  sensor_list <- intersect(c('ghi', 'temp', 'ws'), names(megaframe))
  df <- map(sensor_list,
            function(sensor) get_longterm_resource_for_sensor(megaframe, sensor)) %>%
    bind_rows()

  return(df)
}


get_longterm_resource_for_sensor <- function(megaframe, model_sensor = 'ghi') {
  if(model_sensor == 'ghi') {
    return <- sum_monthly_means(megaframe, model_sensor)
  } else {
    return <- mean_monthly_means(megaframe, model_sensor)
  }

  return <- set_cols_as_model_sensor_longterm_resource(return, model_sensor)

  return(return)
}


sum_monthly_means <- function(mf, model_sensor) {
  MONTH_DAYS <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  df <- mf %>%
    group_by(model, month) %>%
    summarise(ltra = mean(get(model_sensor), na.rm = T)) %>%
    spread(model, ltra) %>%
    mutate(month_hours = MONTH_DAYS[month] * 24) %>%
    mutate_at(vars(-month, -month_hours), funs(. * month_hours / 1000)) %>%
    summarise_at(vars(-month, -month_hours), sum)

  return(df)
}


mean_monthly_means <- function(mf, model_sensor) {
  df <- mf %>%
    group_by(model, month) %>%
    summarise(ltra = mean(get(model_sensor), na.rm = T)) %>%
    spread(model, ltra) %>%
    summarise_at(vars(-month), mean, na.rm = T)

  return(df)
}


set_cols_as_model_sensor_longterm_resource <- function(df, model_sensor) {
  return <- df %>%
    gather(key = 'model', value = 'longterm_resource') %>%
    mutate(sensor = model_sensor)

  return(return)
}


#' Adjust Longterm Resource for all timesteps and sensors
#'
#' Adjust Longterm Resource for all timesteps and sensors
#' @param mcp_list The results of run_mcp_for_all_timesteps_and_sensors()
#' @param longterm_df The results of build_longterm_resource_with_all_sensors
#' @param model_coefficient_is_0 Whether or not the models in mcp_list were forced to have a zero intercept coefficient
#' @return dataframe with longterm adjusted resource for each model and sensor
#' @export
get_longterm_adjustment <- function(mcp_list, longterm_df, model_coefficient_is_0 = FALSE) {
  ltra_builder <- NULL

  for(timestep in names(mcp_list)) {
    for(model_sensor in names(mcp_list[[timestep]])) {
      model_list <- mcp_list[[timestep]][[model_sensor]]
      model_names <- names(model_list)

      adjusted_resource_for_models <- map(model_names,
                                          function(model_name) adjust_longterm_resource_by_model(model_list, model_name,
                                                                                                 model_sensor, timestep,
                                                                                                 model_coefficient_is_0,
                                                                                                 longterm_df)) %>%
        format_ltra_list_as_tibble(model_names, model_sensor, timestep)

      ltra_builder <- add_rows_to_return_df(adjusted_resource_for_models, ltra_builder)
    }
  }

  return(ltra_builder)
}


adjust_longterm_resource_by_model <- function(model_list, model_name, model_sensor, timestep,
                                              model_coefficient_is_0 = FALSE,
                                              longterm_df) {
  lt <- get_longterm_resource_from_df(longterm_df, model_name, model_sensor)
  model <- model_list[[model_name]]
  ltra <- adjust_longterm_resource(model, lt, timestep, model_sensor, model_coefficient_is_0)

  return(ltra)
}


get_longterm_resource_from_df <- function(longterm_df, model_name, model_sensor) {
  lt <- longterm_df %>%
    filter(model == model_name, sensor == model_sensor) %>%
    select(longterm_resource)

  return(lt[[1]])
}


adjust_longterm_resource <- function(model, unadjusted_value, model_timestep = 'daily', sensor = 'ghi',
                                     model_coefficient_is_0 = FALSE) {
  if (model_coefficient_is_0) {
    ltra <- model$coefficients[[1]] * unadjusted_value
  } else {
    intercept_factor <- get_intercept_factor(sensor, model_timestep)
    ltra <- model$coefficients[[1]] * intercept_factor + model$coefficients[[2]] * unadjusted_value
  }

  return(ltra)
}


get_intercept_factor <- function(sensor, model_timestep) {
  # Intercept factor only applies to GHI because it's a sum and not an average longterm value
  if (sensor != 'ghi') {
    intercept_factor <- 1
  } else {
    if(model_timestep == 'daily') {
      # Convert Wh/Day to kWh/year
      intercept_factor <- 365 / 1000
    } else {
      # Convert Wh/hour to kWh/year with .54 factor since only 54% of hours have sunlight
      intercept_factor <- 8760 / 1000 * .54
    }
  }

  return(intercept_factor)
}


format_ltra_list_as_tibble <- function(ltra_list, model_names, model_sensor, model_timestep) {
  names(ltra_list) <- model_names

  ltra <- as_tibble(ltra_list) %>%
    gather(key = 'model', value = 'longterm_adjustment') %>%
    mutate(sensor = model_sensor, timestep = model_timestep)

  return(ltra)
}


add_rows_to_return_df <- function(rows_to_add, return_df) {
  if(is.null(return_df)) {
    return_df <- rows_to_add
  } else {
    return_df <- full_join(return_df, rows_to_add)
  }

  return(return_df)
}


get_model_metrics <- function(mcp_list) {
  model_metrics_builder <- NULL

  for(timestep in names(mcp_list)) {
    for(sensor in names(mcp_list[[timestep]])) {
      model_list <- mcp_list[[timestep]][[sensor]]

      model_metrics <- sapply(model_list, get_model_metrics_for_single_model) %>%
        format_model_metrics_list_as_tibble(sensor, timestep)

      model_metrics_builder <- add_rows_to_return_df(model_metrics, model_metrics_builder)
    }
  }

  return(model_metrics_builder)
}


get_model_metrics_for_single_model <- function(model) {
  r.squared <- summary(model)$r.squared
  adj.r.squared <- summary(model)$r.squared
  sigma <- summary(model)$r.squared

  return(list(r.squared = r.squared,
              adj.r.squared = adj.r.squared,
              sigma = sigma))
}


format_model_metrics_list_as_tibble <- function(model_metrics_list, model_sensor, model_timestep) {
  df <- model_metrics_list %>%
    as.data.frame() %>%
    rownames_to_column('metric') %>%
    gather('model', 'metric_value', -metric) %>%
    mutate(metric_value = as.numeric(metric_value),
           sensor = model_sensor,
           timestep = model_timestep)

  return(df)
}


#' Get Longterm Metrics
#'
#' Get the longterm metrics for all the models and sensors in a megaframe
#' @param megaframe The dataframe containing the longterm models and resources
#' @return A dataframe with the variabels
#' @return por: period of record of all models and sensors
#' @return sms_por: period of record for the onsite measurements
#' @return iav: Inter-annual variability for each model and resource
#' @export
get_longterm_metrics <- function(megaframe) {
  por <- get_years_of_record_for_all_models(megaframe)
  sms_por <- get_years_of_onsite_data_for_all_sensors(por)
  iav <- get_iav_for_all_models_and_sensors(megaframe)

  lt_metrics <- full_join(por, iav) %>%
    mutate(sms_por = sms_por[sensor])

  return(lt_metrics)
}


get_years_of_record_for_all_models <- function(megaframe) {
  sensor_list <- intersect(c('ghi', 'temp', 'ws'), names(megaframe))

  por <- megaframe %>%
    gather('sensor', 'value', all_of(sensor_list)) %>%
    filter(!is.na(value)) %>%
    count(model, sensor) %>%
    mutate(por = n/8760) %>%
    select(model, sensor, por)

  return(por)
}


get_years_of_onsite_data_for_all_sensors <- function(por) {
  sms_por <- filter(por, model == 'sms')$por
  names(sms_por) <- filter(por, model == 'sms')$sensor

  return(sms_por)
}


get_iav_for_all_models_and_sensors <- function(megaframe) {
  sensor_list <- intersect(c('ghi', 'temp', 'ws'), names(megaframe))

  iav <- megaframe %>%
    gather('sensor', 'value', all_of(sensor_list)) %>%
    group_by(year, model, sensor) %>%
    add_tally() %>%
    filter(n > 8700, model != 'sms') %>%
    summarise(value = mean(value, na.rm = T)) %>%
    group_by(model, sensor) %>%
    summarise(ave = mean(value), sd = sd(value)) %>%
    mutate(iav = sd / ave) %>%
    select(model, sensor, iav)

  return(iav)
}


combine_mcp_results_df <- function(lt_resource, adj_resource, model_metrics, lt_metrics) {
  full_df <- full_join(lt_resource, adj_resource) %>%
    full_join(model_metrics) %>%
    full_join(lt_metrics) %>%
    spread(metric, metric_value) %>%
    select(-`<NA>`) %>%
    arrange(sensor, model, timestep)

  return(full_df)
}


add_rom_uncertainty <- function(full_df) {
  df <- full_df %>%
    mutate(rom = sqrt(r.squared * (iav^2 / por) + (1 - r.squared) * (iav^2 / sms_por)))

  return(df)
}

#' Build LTRA Table
#'
#' Modifies the results of an MCP to fit better with a table for a specific timestep
#' @param mcp_results_df The results of an MCP
#' @param model_timestep The timestep you want to include in the table
#' @return A df the is "wider" with metrics more easily identifiable
#' @export
get_timestep_results_table <- function(mcp_results_df, model_timestep = 'daily') {
  ltra_table <-
    mcp_results_df %>%
    filter(timestep == model_timestep) %>%
    arrange(sensor, desc(r.squared)) %>%
    select(sensor, model, timestep, longterm_resource, longterm_adjustment, r.squared, iav, rom)

  return(ltra_table)
}
