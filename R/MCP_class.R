#' MCP Class
#'
#' This class contains all of the inputs and results related to an MCP.
#' An "MCP" is an acronym for Measure Correlate Predict, which is a common approach to correct the bias between
#' measured data and longterm, modeled data.  The methodology is to fit a linear regression of the concurrent
#' measured and modeled data and then use that linear fit to predict future data.
#' @param megaframe A dataframe that contains all of the measured and modeled data in a specific, tidy format
#' @param minimum_required_hours_for_daily_inclusion The number of hourly data points required to include a day in the analysis
#' @param model_coefficient_is_0 Should the linear regression force the intercept to zero?
#' @field results A dataframe containing the results of an MCP run
#' @field daily_results_table A dataframe of the results formatted to show daily timestep results more clearly
#' @field hourly_results_table A dataframe of the results formatted to show hourly timestep results more clearly
#' @examples
#' \dontrun{
#' my_mcp <- MCP(megaframe)
#' my_mcp$run()
#' my_mcp$get_results()
#' }
#' @export MCP
MCP <- setRefClass('MCP',
         fields = list(
            megaframe = 'data.frame',
            minimum_required_hours_for_daily_inclusion = 'numeric',
            model_coefficient_is_0 = 'logical',
            model_fits = 'list',
            results = 'data.frame',
            daily_results_table = 'data.frame',
            hourly_results_table = 'data.frame'
         ),

         methods = list(

           initialize = function(megaframe = megaframe,
                                minimum_required_hours_for_daily_inclusion = 6,
                                model_coefficient_is_0 = FALSE) {
            'Creates an instance of the class.  Call with MCP().'
            megaframe <<- megaframe
            minimum_required_hours_for_daily_inclusion <<- minimum_required_hours_for_daily_inclusion
            model_coefficient_is_0 <<- model_coefficient_is_0
          },

          run = function() {
            'Run an MCP for all timesteps (daily and hourly) and sensors (ghi, ws, temp).'
            model_fits <<- run_mcp(megaframe,
                                   minimum_required_hours_for_daily_inclusion,
                                   model_coefficient_is_0)
            results <<- get_mcp_results(model_fits, megaframe, model_coefficient_is_0)
            daily_results_table <<- get_timestep_results_table(results, model_timestep = 'daily')
            hourly_results_table <<- get_timestep_results_table(results, model_timestep = 'hourly')
          },

          get_results = function() {
            'Get Results of all MCP runs'
            results
          },

          get_daily_results_table = function() {
            'Get Results of daily-timestep MCP runs, formatted in an easy-to-read table format.'
            daily_results_table
          },

          get_hourly_results_table = function() {
            'Get Results of hourly-timestep MCP runs, formatted in an easy-to-read table format.'
            hourly_results_table
          }
         )
       )
