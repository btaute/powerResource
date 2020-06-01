plot_theme <- function() {
  plot_theme <- theme_bw()

  return(plot_theme)
}


plot_time_series_range <- function(df, y, color, date_start, date_end, ylab, color_lab) {
  p <- df %>%
    filter(datetime > date_start, datetime < date_end) %>%
    ggplot(aes(x=datetime, y=get(y), color=as.factor(get(color)))) +
    geom_point() +
    labs(y = ylab, color = color_lab) +
    plot_theme()

  return(p)
}

#' Count Flags
#'
#' Count the number of flags per month for the onsite measurements
#' @param sms_flagged A dataframe of onsite measurements and flags with tidy variables
#' @return A dataframe with variables month, sensor, flag_type, n, perc where n is the
#' count of flags and perc is the percent of datapoints for that month/sensor
#' @examples
#' \dontrun{
#' flag_count <- count_flags(sms_flagged)
#'
#' plot_flag_count_bar_chart(flag_count)
#' }
#' @export
count_flags <- function(sms_flagged) {
  flag_count <- sms_flagged %>%
    select(month, contains('flag')) %>%
    rename_at(vars(contains('flag')), funs(str_replace(., '_flag', ''))) %>%
    gather(key = 'sensor', value = 'flag_type', -month) %>%
    group_by(month) %>%
    count(sensor, flag_type) %>%
    group_by(month, sensor) %>%
    mutate(perc=n/sum(n))

  return(flag_count)
}


#' Plot Flag Count
#' @param flag_count Dataframe, the output of count_flags()
#' @return ggplot bar chart showing percent breakdown of flag_types by month and sensor
#' @examples
#' \dontrun{
#' flag_count <- count_flags(sms_flagged)
#'
#' plot_flag_count_bar_chart(flag_count)
#' }
#' @export
plot_flag_count_bar_chart <- function(flag_count) {
  p <- flag_count %>%
    ggplot(aes(x=sensor, y=perc*100, fill=as.factor(flag_type))) +
    geom_bar(stat='identity') +
    facet_wrap(~month) +
    xlab('Sensor by Month') +
    ylab('Percent of Data Points Flagged') +
    labs(fill = 'Flag') +
    plot_theme()

  return(p)
}


#' Plot Campaign GHI Longterm Adjustments
#'
#' @param campaign_ghi_results output of run_sms_campaign()
#' @return ggplot lineplot that shows change in GHI Longterm Adjustment for each model after each month
#' of the SMS campaign
#' @examples
#' \dontrun{
#' campaign_results <- run_sms_campaign(megaframe)
#' plot_campaign_ghi_longterm_adjustments(campaign_results)
#' }
#' @export
plot_campaign_ghi_longterm_adjustments <- function(campaign_ghi_results) {
  p <- campaign_ghi_results %>%
    ggplot(aes(x = sms_months, y = longterm_adjustment, color = model)) +
    geom_path() +
    ylab('Adjusted Longterm Average GHI (kWh/m2/year)') +
    xlab('Months of SMS Data') +
    plot_theme()

  return(p)
}


#' Plot Campaign GHI Uncertainty
#'
#' @param campaign_ghi_results output of run_sms_campaign()
#' @return ggplot lineplot that shows change in Representativeness of Modeling Period Uncertainty after each month
#' of the SMS campaign
#' @examples
#' \dontrun{
#' campaign_results <- run_sms_campaign(megaframe)
#' plot_campaign_rom_uncertainty(campaign_results)
#' }
#' @export
plot_campaign_rom_uncertainty <- function(campaign_ghi_results) {
  p <- campaign_ghi_results %>%
    ggplot(aes(x = sms_months, y = rom, color = model)) +
    geom_path() +
    ylab('Representativeness of Monitoring Period Uncertainty') +
    xlab('Months of SMS Data') +
    plot_theme()

  return(p)
}
