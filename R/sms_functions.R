#' Import and Tidy Groundwork Onsite Data
#'
#' Import Groundwork QC Data CSVs into a single dataframe with clean variable names
#'
#' Variable naming convention: datetime, month, year, ghi1, ghi2, ghi3, ws, temp, ghi1_flag, ghi2_flag, ghi3_flag, ws_flag, temp_flag
#' @param sms_files A list of Groundwork CSVs to import
#' @return A dataframe containing all of the raw onsite data and flags that can be used to apply filters
#' @examples
#' \dontrun{
#' ## Read in a list of files
#' sms_flagged <- read_groundwork(c('SiteDecember2019QCData.csv', 'SiteJanuary2020QCData.csv'))
#' head(sms_flagged)
#'
#' ## Read in a single file
#' sms_flagged <- read_groundwork('Site2019QCData.csv')
#'
#' ## Read in all files in a folder that end in csv
#' sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))
#' sms_flagged <- read_groundwork(sms_files)
#' summary(sms_flagged)
#' }
#' @export
read_groundwork <- function(sms_files) {
  sms_flagged <- sms_files %>%
    import_groundwork_files() %>%
    tidy_groundwork() %>%
    flag_sms()

  return(sms_flagged)
}


#' Import Groundwork Onsite Data
#'
#' Import Groundwork QC Data CSVs without doing any variable tidying
#'
#' @param sms_files A list of Groundwork CSVs to import
#' @return A dataframe containing all of the raw onsite data
#' @examples
#' \dontrun{
#' ## Get the files to import
#' sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))
#'
#' ## Import the files
#' groundwork_df <- import_groundwork_files(sms_files)
#'
#' ## Tidy the Dataframe
#' tidy_sms <- tidy_groundwork(groundwork_df)
#'
#' ## Flag the Dataframe
#' sms_flagged <- flag_sms(tidy_sms)
#'
#' }
#' @export
import_groundwork_files <- function(sms_files) {
  # CSVs have row of header, row of labels, then data
  # So read in CSV with column types called out explicitly
  # and get rid of that first row in the df (the labels)
  check_if_files_exist(sms_files)
  check_if_files_are_csv(sms_files)

  df <- sms_files %>%
    map(read_csv,
        col_names = TRUE,
        col_types = cols(.default = col_double(),
                         TIMESTAMP = col_character(),
                         TIMESTAMP_1 = col_character(),
                         TIMESTAMP_2 = col_character(),
                         TIMESTAMP_3 = col_character())
    ) %>%
    lapply(function(x) x[-1,]) %>%
    reduce(bind_rows)

  return(df)
}


check_if_files_exist <- function(files) {
  for (file in files) {
    if (!file.exists(file)) {
      stop(paste0(getwd(), file, ' cannot be found.'))
    }
  }
}


check_if_files_are_csv <- function(files) {
  for (file in files) {
    if (!file_ext_is_csv(file)) {
      stop('File is not a CSV.')
    }
  }
}


file_ext_is_csv <- function(file) {
  return(str_ends(file, '.csv'))
}


#' Tidy Groundwork Data Frame
#'
#' This function tidies imported Groundwork data of onsite measurements by selecting only the relevant variables and renaming them to a standard convention.
#'
#' Variable naming convention: datetime, month, year, ghi1, ghi2, ghi3, ws, temp, ghi1_flag, ghi2_flag, ghi3_flag, ws_flag, temp_flag
#' @param groundwork_df The groundwork dataframe
#' @return A dataframe that includes all of the raw groundwork data and flags that can be used for filtering
#' @examples
#' \dontrun{
#' ## Get the files to import
#' sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))
#'
#' ## Import the files
#' groundwork_df <- import_groundwork_files(sms_files)
#'
#' ## Tidy the Dataframe
#' tidy_sms <- tidy_groundwork(groundwork_df)
#'
#' ## Flag the Dataframe
#' sms_flagged <- flag_sms(tidy_sms)
#'
#' }
#' @export
tidy_groundwork <- function(groundwork_df) {
  tidy_sms <- groundwork_df %>%
    rename_all(tolower) %>%
    convert_sms_timestamp_char_to_datetime() %>%
    select_sms_vars() %>%
    rename_sms_vars() %>%
    order_sms_vars()

  return(tidy_sms)
}


convert_sms_timestamp_char_to_datetime <- function(sms_df_lowercase) {
  sms_df <- sms_df_lowercase %>%
    mutate(datetime = mdy_hms(paste(timestamp, timestamp_1)),
           month = month(datetime),
           year = year(datetime))

  return(sms_df)
}


select_sms_vars <- function(sms_df_lowercase) {
  sms_df <- sms_df_lowercase %>%
    select(datetime, month, year,
           contains('ghi_sr20'), # For old files
           contains('ghi-sr20'), # For old files
           contains('ghi_tc'),
           contains('wind_speed_mean'),
           contains('ambient'))

  return(sms_df)
}


rename_sms_vars <- function(sms_df_lowercase) {
  sms_df <- sms_df_lowercase %>%
    rename_at(vars(ends_with('_1')), funs(str_replace(.,'1$', 'flag'))) %>%
    rename_at(vars(contains('ghi')), funs(str_replace(., '_tc_', ''))) %>%
    rename_at(vars(contains('ghi')), funs(str_replace(., '_sr20_', ''))) %>% # For old files
    rename_at(vars(contains('ghi')), funs(str_replace(., '-sr20_', ''))) %>% # For old files
    rename_at(vars(contains('ghi')), funs(str_replace(., '_avg', ''))) %>%
    rename_at(vars(contains('wind_speed')),
              funs(str_replace(., 'wind_speed_mean', 'ws'))) %>%
    rename_at(vars(contains('ambient')),
              funs(str_replace(., 'ambient_temp_avg', 'temp')))

  return(sms_df)
}


order_sms_vars <- function(sms_df_lowercase) {
  sms_df <- sms_df_lowercase %>%
    select(-contains('flag'), everything()) %>%
    arrange(datetime)

  return(sms_df)
}


#' Flag SMS Data Frame
#'
#' Flag any missing datetimes within the onsite data Period of Record
#' @param tidy_sms A dataframe of the onsite data, in the tidy variable format
#' @return A dataframe that has the missing datetimes filled in with NAs at the measured values and flagged 1000
#' @examples
#' \dontrun{
#' ## Get the files to import
#' sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))
#'
#' ## Import the files
#' groundwork_df <- import_groundwork_files(sms_files)
#'
#' ## Tidy the Dataframe
#' tidy_sms <- tidy_groundwork(groundwork_df)
#'
#' ## Flag the Dataframe
#' sms_flagged <- flag_sms(tidy_sms)
#'
#' }
#' @export
flag_sms <- function(tidy_sms) {
  sms_flagged <- tidy_sms %>%
    flag_missing_datetimes() %>%
    recalculate_month_and_year()

  return(sms_flagged)
}


flag_missing_datetimes <- function(tidy_sms) {
  # Vars should be:  Datetime column, NAs to all measurements, fill_list values to flag_names
  flag_list <- build_missing_data_flag_list(tidy_sms)

  complete_sms <- complete(tidy_sms,
                           datetime = seq(min(datetime), max(datetime), by = "min"),
                           fill = flag_list)

  return(complete_sms)
}


build_missing_data_flag_list <- function(tidy_sms) {
  # Need a named list to fill in missing data points with NAs and flag them all with 1000
  flag_names <- names(select(tidy_sms, contains('flag')))
  missing_flags <- rep(1000, length(flag_names))
  fill_list <- as.list(set_names(missing_flags, flag_names))

  return(fill_list)
}


recalculate_month_and_year <- function(tidy_sms) {
  df <- mutate(tidy_sms,
         month = month(datetime),
         year = year(datetime))

  return(df)
}


#' Prepare Flagged SMS Dataframe for Megaframe Compilation
#'
#' Converts a tidy sms (solar met station) dataframe to megaframe format
#'
#' megaframe format consists of distinct, hourly variabls:
#' datetime, year, month, model, ghi, dni, dhi, temp, ws
#' @param sms_flagged Tidy SMS dataframe
#' @param ghi_cutoff_min Measured GHI values below this cutoff will be filtered to zero because of poor sensor performance at low values (W/m2)
#' @param ws_cutoff_min Measured wind speed values below this cutoff will be filtered to zero (m/s)
#' @param flagged_data_is_filtered Whether or not we should filter out the flagged data (Recommended)
#' @return dataframe of onsite data formatted for megaframe inclusion
#' @examples
#' \dontrun{
#' ## Get sms_flagged dataframe (onsite data with "tidy" variables)
#' sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))
#' sms_flagged <- read_groundwork(sms_files)
#'
#' ## Prepare for Megaframe
#' sms <- prepare_sms_flagged_for_megaframe(sms_flagged)
#' }
#' @export
prepare_sms_flagged_for_megaframe <- function(sms_flagged, ghi_cutoff_min = 5, ws_cutoff_min = .25,
                                              flagged_data_is_filtered = TRUE) {
  sms <- sms_flagged %>%
    filter_sms(ghi_cutoff_min, ws_cutoff_min, flagged_data_is_filtered) %>%
    average_ghi_sensors() %>%
    average_data_to_hourly() %>%
    format_sms_vars_for_megaframe()

  return(sms)
}


#' Filter SMS data
#'
#' Apply measurement cutoff filters and flag filters
#' @param sms_flagged Dataframe of sms data with "tidy" variables
#' @param ghi_cutoff_min Measured GHI values below this cutoff will be filtered to zero because of poor sensor performance at low values (W/m2)
#' @param ws_cutoff_min Measured wind speed values below this cutoff will be filtered to zero (m/s)
#' @param flagged_data_is_filtered Whether or not we should filter out the flagged data (Recommended)
#' @return SMS Dataframe with zero at all points below the measurement cutoff minimum and NA for all points with flag filters applied
#' @examples
#' \dontrun{
#' ## Get flagged, tidy sms data
#' sms_flagged <- read_groundwork(groundwork_file_list)
#'
#' ## Apply filters
#' sms_filtered <- filter_sms(sms_flagged)
#' no_flag_filters <- filter_sms(sms_flagged, flagged_data_is_filtered = FALSE)
#' change_cutoff_filters <- filter_sms(sms_flagged, ghi_cutoff_min = 10, ws_cutoff_min = 0)
#' }
#' @export
filter_sms <- function(sms_flagged, ghi_cutoff_min = 5, ws_cutoff_min = .25, flagged_data_is_filtered = TRUE) {
  if (flagged_data_is_filtered) {
   sms_filtered <- apply_flag_filter(sms_flagged)
  } else {
    sms_filtered <- sms_flagged
  }

  sms_filtered <- apply_cutoff_filters(sms_filtered, ghi_cutoff_min, ws_cutoff_min)

  return(sms_filtered)
}


apply_cutoff_filters <- function(sms_flagged, ghi_cutoff_min, ws_cutoff_min) {
  sms_filtered <- sms_flagged %>%
    mutate_at(vars(contains('ghi'), -contains('flag')),
              function(ghi) if_else(ghi < ghi_cutoff_min,
                                     0,
                                     ghi)) %>%
    mutate(ws = if_else(ws < ws_cutoff_min,
                        0,
                        ws))

  return(sms_filtered)
}


apply_flag_filter <- function(sms_flagged) {
  flag_names <- names(select(sms_flagged, contains('flag')))
  sms_filtered <- as.data.frame(sms_flagged) # data.frame needed for indexing when applying filter

  for (var_flag in flag_names) {
    var <- str_replace(var_flag, '_flag', '')
    flagged_datapoints <- sms_filtered[var_flag] > 1
    sms_filtered[flagged_datapoints, var] <- NA
  }

  return(as_tibble(sms_filtered))
}


#' Average GHI Sensors
#'
#' Average redundant ghi sensors of onsite dataframe
#'
#' If there are 3 or more sensors, conduct a straight average at each datapoint, ignoring missing data.
#' If there are only 2 sensors, create a reconstructed average, by applying the monthly bias between the two sensors to reconstruct any missing data points before averaging.
#' @param sms_flagged An sms dataframe with tidy variables
#' @return sms_flagged with an additional variable ghi_ave
#' @examples
#' \dontrun{
#' ## To Prepare sms_flagged dataframe for megaframe compliation:
#' sms_flagged %>%
#'   filter_sms() %>%
#'   average_ghi_sensors() %>%
#'   average_data_to_hourly() %>%
#'   format_sms_vars_for_megaframe()
#' }
#' @export
average_ghi_sensors <- function(sms_flagged) {
  if('ghi3' %in% names(sms_flagged)) {
    sms_with_ghi_ave <- add_ghi_ave_var_simple(sms_flagged)
  } else {
    sms_with_ghi_ave <- add_ghi_ave_var_reconstructed(sms_flagged)
  }

  return(sms_with_ghi_ave)
}


add_ghi_ave_var_simple <- function(sms_flagged) {
  ghi_ave <- sms_flagged %>%
    select(datetime, contains('ghi'), -contains('flag')) %>%
    gather('sensor', 'ghi', -datetime) %>%
    group_by(datetime) %>%
    summarise(ghi_ave = mean(ghi, na.rm = T))

  sms_with_ghi_ave <- left_join(sms_flagged, ghi_ave)

  return(sms_with_ghi_ave)
}


add_ghi_ave_var_reconstructed <- function(sms_flagged) {
  sms_with_ghi_ave <- sms_flagged %>%
    get_monthly_ghi_ratios() %>%
    reconstruct_missing_ghi_with_monthly_ratios(sms_flagged) %>%
    add_ghi_ave_var_simple()

  return(sms_with_ghi_ave)
}


get_monthly_ghi_ratios <- function(sms_flagged) {
  monthly_ghi_ratios <- sms_flagged %>%
    select(month, ghi1, ghi2) %>%
    group_by(month) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate(ratio21 = ghi2 / ghi1, ratio12 = ghi1 / ghi2)

  return(monthly_ghi_ratios)
}


reconstruct_missing_ghi_with_monthly_ratios <- function(monthly_ghi_ratios, sms_flagged) {
  ratio21 <- monthly_ghi_ratios$ratio21
  ratio12 <- monthly_ghi_ratios$ratio12

  sms_with_replaced_ghi <- sms_flagged %>%
    create_reconstructed_ghi_vars(ratio21, ratio12) %>%
    replace_missing_ghi_vars_with_reconstructed()

  return(sms_with_replaced_ghi)

}


create_reconstructed_ghi_vars <- function(sms_flagged, ratio21, ratio12) {
  sms_with_reconstructed_ghi <- sms_flagged %>%
    mutate(ghi1_reconst = ghi2 * ratio12[month],
           ghi2_reconst = ghi1 * ratio21[month])

  return(sms_with_reconstructed_ghi)
}


replace_missing_ghi_vars_with_reconstructed <- function(sms_with_reconstructed_ghi) {
  sms_with_replaced_ghi <- sms_with_reconstructed_ghi %>%
    mutate(
      ghi1 = if_else(is.na(ghi1),
                     ghi1_reconst,
                     ghi1),
      ghi2 = if_else(is.na(ghi2),
                     ghi2_reconst,
                     ghi2)
    )

  return(sms_with_replaced_ghi)
}


#' Average Time Series Data To Hourly Intervals
#'
#' Convert minute interval SMS data to hourly by taking the average over the hour
#' @param sms_flagged Dataframe of sms data at minute intervals with "tidy" variables
#' @return sms_flagged with hourly intervals
#' @examples
#' \dontrun{
#' ## To Prepare sms_flagged dataframe for megaframe compliation:
#' sms_flagged %>%
#'   filter_sms() %>%
#'   average_ghi_sensors() %>%
#'   average_data_to_hourly() %>%
#'   format_sms_vars_for_megaframe()
#' }
#' @export
average_data_to_hourly <- function(sms_flagged) {
  sms_hourly <- sms_flagged %>%
    ungroup() %>%
    mutate(day = day(datetime), hour = hour(datetime))%>%
    group_by(year, month, day, hour) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_at(vars(datetime), ceiling_date, unit = 'hour')

  return(sms_hourly)

}


#' Format SMS Variables for Megaframe
#'
#' Selects/creates the variables needed for megaframe, using ghi_ave for ghi
#'
#' Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param sms_flagged SMS dataframe with "tidy" variables, including ghi_ave
#' @return SMS dataframe with variables ready to integrate with Megaframe format
#' @examples
#' \dontrun{
#' ## To Prepare sms_flagged dataframe for megaframe compliation:
#' sms_flagged %>%
#'   filter_sms() %>%
#'   average_ghi_sensors() %>%
#'   average_data_to_hourly() %>%
#'   format_sms_vars_for_megaframe()
#' }
#' @export
format_sms_vars_for_megaframe <- function(sms_flagged) {
  check_if_ghi_ave_in_sms_flagged(sms_flagged)

  sms <- sms_flagged %>%
    ungroup() %>%
    mutate(model = 'sms', ghi = ghi_ave, dni = NA, dhi = NA) %>%
    select(datetime, year, month, model, ghi, dni, dhi, temp, ws) %>%
    distinct(datetime, .keep_all = TRUE)

  return(sms)
}


check_if_ghi_ave_in_sms_flagged <- function(sms_flagged) {
 if (!'ghi_ave' %in% names(sms_flagged)) {
   stop('ghi_ave is not in sms_flagged.  You must call average_ghi_sensors(sms_flagged) first.')
 }
}
