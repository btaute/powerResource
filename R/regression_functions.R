#' Run MCP for all Timesteps and Sensors
#'
#' The function called by the run() method of the MCP class.
#' @param megaframe A megaframe with at least one satellite model and the sms model
#' @param minimum_required_hours_for_daily_inclusion The number of data points required for a day to be included in the regression
#' @param model_coefficient_is_0 Whether or not the linear regression should be forced to have a zero intercept
#' @return A list of model fits to the onsite data, organized by timestep->sensor->model
#' @examples
#' \dontrun{
#' my_mcp <- run_mcp(megaframe)
#' get_mcp_restults(my_mcp)
#'
#' ## Run MCP for a specific sensor
#' mf <- filter(megaframe, sensor == 'ghi')
#' my_mcp <- run_mcp(mf)
#' get_mcp_results(my_mcp, mf)
#' }
#' @export
run_mcp <- function(megaframe, minimum_required_hours_for_daily_inclusion = 6, model_coefficient_is_0 = FALSE) {
  sensor_list <- intersect(c('ghi', 'temp', 'ws'), names(megaframe))
  f <- function(sensor, daily) fit_models(megaframe, sensor, daily,
                                                  minimum_required_hours_for_daily_inclusion,
                                                  model_coefficient_is_0)

  daily <- map(sensor_list, function(sensor) f(sensor, daily = TRUE))
  hourly <- map(sensor_list, function(sensor) f(sensor, daily = FALSE))

  names(daily) <- sensor_list
  names(hourly) <- sensor_list

  return(list(daily = daily, hourly = hourly))
}

#' Run MCP for all Sensors at one timestep
#'
#' run_mcp() but for either daily or hourly
#' @param megaframe A megaframe with at least one satellite model and the sms model
#' @param daily Logical; daily timestep if true, hourly if false
#' @param minimum_required_hours_for_daily_inclusion The number of data points required for a day to be included in the regression
#' @param model_coefficient_is_0 Whether or not the linear regression should be forced to have a zero intercept
#' @return A list of model fits to the onsite data, organized by timestep->sensor->model
#' @examples
#' \dontrun{
#' daily_mcp <- run_mcp_for_single_timestep(megaframe, daily = TRUE)
#' get_mcp_results(daily_mcp, megaframe)
#'
#' hourly_mcp <- run_mcp_for_single_timestep(megaframe, daily = FALSE)
#' get_mcp_results(hourly_mcp, megaframe)
#'
#' ## Run MCP for a specific sensor and timestep
#' mf <- select(megaframe, -temp, -ws)
#' my_mcp <- run_mcp_for_single_timestep(mf)
#' get_mcp_results(my_mcp, mf)
#' }
#' @export
run_mcp_for_single_timestep <- function(megaframe, daily = TRUE, minimum_required_hours_for_daily_inclusion = 6, model_coefficient_is_0 = FALSE) {
  sensor_list <- intersect(c('ghi', 'temp', 'ws'), names(megaframe))
  f <- function(sensor) fit_models(megaframe, sensor, daily,
                                          minimum_required_hours_for_daily_inclusion,
                                          model_coefficient_is_0)

  mcp <- map(sensor_list, function(sensor) f(sensor))
  names(mcp) <- sensor_list

  return_list <- list(mcp)
  if (daily) {
    names(return_list) <- 'daily'
  } else {
    names(return_list) <- 'hourly'
  }

  return(return_list)
}


fit_models <- function(megaframe, model_sensor = 'ghi', daily = TRUE, minimum_required_hours_for_daily_inclusion = 6,
                model_coefficient_is_0 = FALSE) {
  model_list <- megaframe %>%
    prepare_megaframe_for_models(model_sensor, daily, minimum_required_hours_for_daily_inclusion) %>%
    fit_linear_regression_df(model_coefficient_is_0)

  return(model_list)
}


prepare_megaframe_for_models <- function(df, model_sensor, daily,
                                         minimum_required_hours_for_daily_inclusion) {
  df_prepared_for_models <- df %>%
    prepare_models_as_vars(model_sensor) %>%
    configure_values_for_timestep(model_sensor, daily, minimum_required_hours_for_daily_inclusion)

  return(df_prepared_for_models)
}


prepare_models_as_vars <- function(df, model_sensor) {
  df_models_as_cols <- df %>%
    select(datetime, model, model_sensor) %>%
    distinct() %>%
    spread(model, model_sensor) %>%
    filter(!is.na(sms))

  return(df_models_as_cols)
}


configure_values_for_timestep <- function(df, model_sensor, daily, minimum_required_hours_for_daily_inclusion) {
  if (daily) {
    configured_df <- group_hourly_values_into_daily(df, model_sensor, minimum_required_hours_for_daily_inclusion)
  } else {
    if (model_sensor == 'ghi') {
      configured_df <- remove_skewed_data_from_hourly_ghi(df)
    } else{
      configured_df <- df
    }
  }

  return(configured_df)
}


group_hourly_values_into_daily <- function(df, model_sensor, minimum_required_hours_for_daily_inclusion) {
  if (model_sensor == 'ghi') {
    summarise_function <- sum
  } else {
    summarise_function <- mean
  }

  df_daily <- df %>%
    group_by(datetime = date(datetime)) %>%
    add_tally() %>%
    filter(n > minimum_required_hours_for_daily_inclusion) %>%
    summarise_all(summarise_function) %>%
    select(-n)

  return(df_daily)
}


remove_skewed_data_from_hourly_ghi <- function(df) {
  # Hourly GHI needs to only run MCP for hours with daylight
  # Don't want regression models to be skewed by zeroes.
  df_unskewed <- filter(df, sms > 0)

  return(df_unskewed)
}


fit_linear_regression_df <- function(df_prepared_for_models, model_coefficient_is_0) {
  model_df <- select(df_prepared_for_models, -datetime, -sms)
  sms <- df_prepared_for_models$sms

  if (model_coefficient_is_0) {
    return_models <- map(model_df, function(model) lm(sms ~ 0 + model, na.action = na.exclude))
  } else {
    return_models <- map(model_df, function(model) lm(sms ~ model, na.action = na.exclude))
  }

  names(return_models) <- names(model_df)

  return(return_models)
}





