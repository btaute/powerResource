#' Launch App
#'
#' Launch a graphical user interface that allows you to upload measured data and longterm satellite models and run an MCP.  Has the ability to generate reports and export adjusted TMYs.
#'
#' @examples
#' \dontrun{
#' launch_app()
#' }
#' @export
launch_app <- function() {
  appDir <- system.file("app", package = "powerResource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  options(shiny.maxRequestSize = 400*1024^2)

  shiny::runApp(appDir, display.mode = "normal")
}



#### Functions used in the app ####

select_longterm_models <- function(megaframe) {
  df <- megaframe %>%
    filter(model != 'sms')

  return(df)
}


summarise_monthly_total_ghi <- function(megaframe) {
  MONTH_DAYS <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  df <- megaframe %>%
    group_by(month, model) %>%
    summarise(ghi = mean(ghi, na.rm = T)) %>%
    mutate(month_hours = MONTH_DAYS[month] * 24,
           ghi = ghi * month_hours / 1000)

  return(df)
}


plot_monthly_ghi_summary <- function(monthly_summarised_mf, title) {
  p <- monthly_summarised_mf %>%
    ggplot(aes(month, ghi, color = model, fill = model)) +
    geom_bar(stat = 'identity', alpha = .4, position = 'dodge') +
    scale_x_discrete(limits = as.character(1:12)) +
    geom_line(size = .8) +
    ylab('kWh/m2') +
    xlab('Month') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    plot_theme()

  return(p)
}


summarise_annual_total_ghi <- function(megaframe) {
  # Want to standardize years to always have 8760 hours,
  # only including years with few missing data points

  df <- megaframe %>%
    group_by(year, model) %>%
    # Want years to be mostly full
    filter(n() > 8700) %>%
    # Mean * 8760 normalizes leap years and slightly short years
    summarise_at(vars(ghi), funs(mean(.) * 8760/1000))

  return(df)
}


add_normalized_ghi <- function(megaframe) {
  df <- megaframe %>%
    group_by(model) %>%
    mutate(normalized = ghi / mean(ghi))

  return(df)
}


plot_annual_model_values <- function(annual_summarised_df, data, data_label) {
  p <- annual_summarised_df %>%
    ggplot(aes(x = year, y = get(data), color = model)) +
    geom_line(size = 1) +
    ylab(data_label) +
    geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed') +
    plot_theme()

  return(p)
}


plot_monthly_ghi <- function(megaframe, title) {
  p <- megaframe %>%
    ggplot(aes(x = as.factor(month), y = ghi, fill = model)) +
    geom_boxplot() +
    ylab('Hourly Irradiance (Wh/m2)') +
    xlab('Month') +
    labs(fill = 'Model') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    plot_theme()

  return(p)
}


filter_out_missing_sms_ghi <- function(megaframe) {
  df <- megaframe %>%
    filter(!((model == 'sms') & is.na(ghi)))

  return(df)
}


spread_models_vs_sms_for_sensor <- function(megaframe, sensor) {
  df <- megaframe %>%
    select(datetime, month, year, model, all_of(sensor)) %>%
    spread(model, sensor) %>%
    filter(sms > 0)

  return(df)
}


monthly_scatter_plots_sms_vs_model <- function(sms_vs_model_df, model_input, sensor) {
  p <- sms_vs_model_df %>%
    ggplot(aes(x = sms, y = get(model_input))) +
    geom_point(aes(color = as.factor(month)), alpha = .3) +
    geom_smooth(aes(color = as.factor(month))) +
    stat_poly_eq(aes(color = as.factor(month), label = paste(..eq.label.., ..rr.label.., sep = '~~~')),
                 formula = y ~ x, label.y = 4.2, parse = TRUE) +
    facet_wrap(~as.factor(month)) +
    ylab(model_input) +
    xlab('SMS') +
    ggtitle(paste('Monthly Bias:  Hourly ', str_to_upper(sensor), sep = '')) +
    plot_theme() +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}


prepare_megaframe_for_concurrent_monthly_plots <- function(megaframe) {
  df <- megaframe %>%
    filter(datetime >= min(filter(megaframe, model == 'sms')$datetime),
           datetime <= max(filter(megaframe, model == 'sms')$datetime),
           ghi > 0) %>%
    mutate(month = as.factor(paste(year, month, sep='/')))

  return(df)
}


grab_model_list <- function(mcp_list, timestep = 'hourly', sensor = 'ghi', model_value = 'fitted.values') {
  df <- bind_rows(lapply(names(mcp_list[[timestep]][[sensor]]),
                         FUN = function(x) grab_model(mcp_list, timestep, x, sensor, model_value))) %>%
    gather('model', 'value', -sms) %>%
    drop_na %>%
    rename(!!model_value := value)

  return(df)
}


grab_model <- function(mcp_list, timestep, model_name, sensor = 'ghi', model_value = 'fitted.values') {
  model <-
    mcp_list[[timestep]][[sensor]][[model_name]]$model %>%
    as_tibble() %>%
    select(sms) %>%
    mutate(!!model_name := mcp_list[[timestep]][[sensor]][[model_name]][[model_value]])
  # !! and := because I want the string within variable model_name, not the string "model_name"

  return(model)
}


plot_model_comparison <- function(models_df, model_value = 'fitted.values', title) {
  # model_value can be fitted.values or residuals
  p <- models_df %>%
    ggplot(aes_string(x = 'sms', y = model_value, color = 'model')) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~model) +
    labs(title = title) +
    plot_theme()

  return(p)
}


plot_campaign_ghi_longterm_adjustments <- function(campaign_ghi_results) {
  p <- campaign_ghi_results %>%
    ggplot(aes(x = sms_months, y = longterm_adjustment, color = model)) +
    geom_path() +
    ylab('Adjusted Longterm Average GHI (kWh/m2/year)') +
    xlab('Months of SMS Data') +
    plot_theme()

  return(p)
}


plot_campaign_ghi_uncertainty <- function(campaign_ghi_results) {
  p <- campaign_ghi_results %>%
    ggplot(aes(x = sms_months, y = rom, color = model)) +
    geom_path() +
    ylab('Representativeness of Monitoring Period Uncertainty') +
    xlab('Months of SMS Data') +
    plot_theme()

  return(p)
}

# Not used:
# Plots
# Function to get number of observations and place it at mean of a boxplot
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}
