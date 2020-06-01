#' Scale TMY
#'
#' Adjust the tmy to reflect the longterm resource adjustment calculated by the MCP
#' @param mcp_results Results of an MCP
#' @param tmy a Tidy tmy
#' @param model_name the model in mcp_results to be used as the long-term resource adjustment
#' @param model_timestep the timestep in mcp_results to be used as the long-term resource adjustment
#' @param sensor_list the sensors of the tmy to be scaled
#' @return The original tmy with the sensors scaled to the long-term adjusted values.
#' If 'ghi' is one of the sensors, dhi and dni will be scaled by the same ratio as ghi.
#' @examples
#' \dontrun{
#' my_mcp <- MCP(megaframe)
#' my_mcp$run()
#'
#' tmy <- reac_csv('tmy.csv') #columns = datetime, ghi, dni, dhi, temp, ws
#'
#' adjusted_tmy <- scale_tmy(my_mcp$get_results(), tmy, 'cpr3.4', 'daily', c('ghi', 'temp'))
#' }
#' @export
scale_tmy <- function(mcp_results, tmy, model_name, model_timestep, sensor_list) {
  tmy_table <- summarise_tmy_table(tmy)

  for(model_sensor in sensor_list) {
    adj_factor <- calculate_tmy_adjustment_factor(mcp_results, tmy_table, model_name, model_sensor, model_timestep)
    if(model_sensor == 'ghi') {
      tmy <- scale_all_irradiance_variables(tmy, adj_factor)
    } else {
      tmy[model_sensor] <- tmy[model_sensor] * adj_factor
    }
  }

  return(tmy)
}


calculate_tmy_adjustment_factor <- function(mcp_results, tmy_table, model_name, model_sensor, model_timestep) {
  ltra <- mcp_results %>%
    distinct() %>%
    filter(model == model_name, sensor == model_sensor, timestep == model_timestep)

  adj_factor <- ltra$longterm_adjustment / tmy_table[[model_sensor]]

  return(adj_factor)
}


summarise_tmy_table <- function(tmy) {
  tmy_table <- tmy %>%
    summarise(ghi = sum(ghi) / 1000, dni = sum(dni) / 1000, dhi = sum(dhi) / 1000,
              temp = mean(temp), ws = mean(ws))

  return(tmy_table)
}


scale_all_irradiance_variables <- function(tmy, adj_factor) {
  scaled_tmy <- tmy %>%
    mutate(ghi = ghi * adj_factor,
           dhi = dhi * adj_factor,
           dni = dni * adj_factor,
           dhi = if_else(dhi > ghi, ghi, dhi)) # Fixing an error that is present in some tmy inputs

  return(scaled_tmy)
}
