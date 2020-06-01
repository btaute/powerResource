#' Run SMS Campaign
#'
#' Run an MCP at monthly intervals throughout SMS Campaign.  With each additional month of data, you can see how the results of an MCP change.
#'
#' @param megaframe A megaframe with at least one satellite model and the sms model
#' @return Dataframe that contains all of the results with a variable to identify the year and month of the last datapoint for the MCP
#' @examples
#' \dontrun{
#' run_sms_campaign(megaframe)
#' }
#' @export
run_sms_campaign <- function(megaframe) {
  df <- megaframe %>%
    remove_nonGHI_sensors() %>%
    prepare_megaframes_with_monthly_increases_in_onsite_measurement_period() %>%
    run_mcps_in_list() %>%
    compile_mcp_runs_results()

  return(df)
}


remove_nonGHI_sensors <- function(megaframe) {
  mf <- megaframe %>%
    select(-dni, -dhi, -temp, -ws)

  return(mf)
}


prepare_megaframes_with_monthly_increases_in_onsite_measurement_period <- function(megaframe) {
  start <- min(filter(megaframe, model == 'sms')$datetime)
  end <- max(filter(megaframe, model == 'sms')$datetime)

  mf_list <- list()
  for(interval_end in seq.POSIXt(end, start, '-1 month')) {
    mf <- megaframe %>%
      filter(datetime <= interval_end)

    dt <- as_datetime(interval_end)
    index <- paste(year(dt), month(dt), sep = '_')
    mf_list[[index]] <- mf
  }

  return(mf_list)
}


run_mcps_in_list <- function(mf_list) {
  results_list <- list()

  for(end in names(mf_list)) {
    mcp <- run_mcp_for_single_timestep(mf_list[[end]])
    results_list[[end]] <- get_mcp_results(mcp, mf_list[[end]])
  }

  return(results_list)
}


compile_mcp_runs_results <- function(results_list) {
  df <- map(names(results_list), function(n) mutate(results_list[[n]], 'interval' = n)) %>%
    bind_rows() %>%
    mutate(sms_months = sms_por * 12)

  return(df)
}

