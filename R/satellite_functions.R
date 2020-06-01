#' Read CPR CSVs into a list of tidy dataframes
#'
#' Reads in a list of Clean Power Research Time Series Files and returns a list of data frames named after each model version
#' and formatted to be integrated with a Megaframe.
#'
#' Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param cpr_files A list of strings that provide a path to each CPR Timeseries CSV.
#' @return A list of tidy data frames (technically tibbles), named after the CPR model version
#' @examples
#' \dontrun{
#' cpr_list <- read_cpr(c('path/to/cpr32.csv', 'path/to/cpr33.csv', 'path/to/cpr34.csv'))
#' head(cpr_list$cpr3.3)
#'
#' cpr_list <- read_cpr('cpr34.csv')
#' head(cpr_list$cpr3.4)
#' }
#' @export
read_cpr <- function(cpr_files) {
  df_list <- import_cpr_list(cpr_files) %>%
    tidy_cpr_list()

  return(df_list)
}


#' Read Vaisala CSVs into a list of tidy dataframes
#'
#' Reads in a list of Vaisala Time Series Files and returns a list of data frames named after each model version
#' and formatted to be integrated with a Megaframe.
#'
#' Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param vaisala_files A list of strings that provide a path to each Vaisala Timeseries CSV.
#' @return A list of tidy data frames (technically tibbles), named after the Vaisala model version
#' @examples
#' \dontrun{
#' vaisala_list <- read_vaisala(c('path/to/vaisala123.csv', 'path/to/vaisala213.csv'))
#' head(vaisala_list$vaisala2.1.3)
#'
#' vaisala_list <- read_vaisala('vaisala123.csv')
#' head(vaisala_list$vaisala1.2.3)
#' }
#' @export
read_vaisala <- function(vaisala_files) {
  df_list <- import_vaisala_list(vaisala_files) %>%
    tidy_vaisala_list()

  return(df_list)
}


import_cpr_list <- function(cpr_files) {
  check_if_files_exist(cpr_files)
  check_if_files_are_csv(cpr_files)
  cpr_list <- map(cpr_files, read_csv, skip = 1)
  names(cpr_list) <- get_cpr_versions(cpr_list)

  return(cpr_list)
}


get_cpr_versions <- function(cpr_list) {
  # CPR data version is a variable in data
  cpr_versions <- character(length = length(cpr_list))
  for (i in 1:length(cpr_versions)) {
    cpr_versions[[i]] <- paste('cpr', cpr_list[[i]]$DataVersion[[1]], sep='')
  }

  return(cpr_versions)
}


import_vaisala_list <- function(vaisala_files) {
  check_if_files_exist(vaisala_files)
  check_if_files_are_csv(vaisala_files)
  vaisala_list <- map(vaisala_files, read_csv, skip = 11)
  names(vaisala_list) <- get_vaisala_versions(vaisala_files)

  return(vaisala_list)
}


get_vaisala_versions <- function(vaisala_files) {
  # Vaisala data version is in header of file, so need to read it twice
  vaisala_versions <- vector('character', length(vaisala_files))
  i <- 1
  for (file in vaisala_files) {
    temp <- read_csv(file, n_max = 10)
    vaisala_versions[i] <- temp[[7,2]]
    i <- i + 1
  }

  # Clean up the variable names
  vaisala_versions <- vaisala_versions %>%
    tolower() %>%
    str_remove_all(' ')

  return(vaisala_versions)
}


#' Tidy CPR Data Frame
#'
#' Converts an imported CPR data frame to one that is in the Megaframe format
#'
#' Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param cpr_df A dataframe from importing a CPR timeseries
#' @return A dataframe in the Megaframe format
#' @examples
#' \dontrun{
#' cpr_df <- read_csv('cpr_file.csv', skip = 1)
#' cpr <- tidy_cpr(cpr_df)
#' summary(cpr)
#' }
#' @export
tidy_cpr <- function(cpr_df) {
  # tidy data has hourly variables:
  # datetime, year, month, model, ghi, dni, dhi, temp, ws
  # all distinct (no duplicate hours)

  df <- as_tibble(cpr_df) %>%
    rename_all(tolower) %>%
    clean_cpr_vars() %>%
    distinct(datetime, .keep_all = TRUE)

  return(df)
}


clean_cpr_vars <- function(cpr_df_lowercase) {
  df <- cpr_df_lowercase %>%
    mutate(datetime = ensure_datetime(`observationtime(lst)`),
           month = month(datetime),
           year = year(datetime)) %>%
    select(datetime, year, month,
           ghi = contains('global'),
           dni = contains('direct'),
           dhi = contains('diffuse'),
           temp = contains('deg C'),
           ws = contains('m/s'))

  return(df)
}


#' Tidy Vaisala Data Frame
#'
#' Converts an imported Vaisala data frame to one that is in the Megaframe format
#'
#' Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param vaisala_df A dataframe from importing a Vaisala timeseries
#' @examples
#' \dontrun{
#' vaisala_df <- read_csv('vaisala_file.csv', skip = 1)
#' vaisala <- tidy_vaisala(vaisala_df)
#' summary(vaisala)
#' }
#' @return A dataframe in the Megaframe format
#' @export
tidy_vaisala <- function(vaisala_df) {
  # tidy data has hourly variables:
  # datetime, year, month, model, ghi, dni, dhi, temp, ws
  # all distinct (no duplicate hours)

  df <- as_tibble(vaisala_df) %>%
    rename_all(tolower) %>%
    clean_vaisala_vars() %>%
    distinct(datetime, .keep_all = TRUE)

  return(df)
}


clean_vaisala_vars <- function(vaisala_df_lowercase) {
  df <- vaisala_df_lowercase %>%
    mutate(datetime = ensure_datetime(`date time`),
           month = month(datetime),
           year = year(datetime)) %>%
    select(datetime, year, month,
           ghi = contains('ghi'),
           dni = contains('dni'),
           dhi = contains('dif'),
           temp = contains('temp'),
           ws = contains('windspeed'))

  return(df)
}


ensure_datetime <- function(datetime) {
  if (typeof(datetime) == 'character') {
    datetime = mdy_hm(datetime)
  }

  return(datetime)
}


tidy_cpr_list <- function(cpr_list) {
  return_list <- list()
  for (name in names(cpr_list)) {
    return_list[[name]] <- tidy_cpr(cpr_list[[name]]) %>%
      mutate(model = name)
  }

  return(return_list)
}


tidy_vaisala_list <- function(vaisala_list) {
  return_list <- list()
  for (name in names(vaisala_list)) {
    return_list[[name]] <- tidy_vaisala(vaisala_list[[name]]) %>%
      mutate(model = name)
  }

  return(return_list)
}


#' Build a Megaframe!
#'
#' Compiles tidy sms, cpr, and vaisala data frames into one MEGAFRAME!
#'
#' Megaframe format includes hourly variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
#' @param sms Groundwork data that has been filtered, sensors have been averaged, and minute data has been averaged to hourly
#' @param cpr_list Tidy CPR data
#' @param vaisala_list Tidy Vaisala data
#' @return A single dataframe that includes all of the sms, cpr, and vaisala timeseries data in a tidy format
#' @examples
#' \dontrun{
#' ## Prepare the inputs
#' sms_flagged <- read_groundwork('groundwork.csv')
#' sms <- prepare_sms_flagged_for_megaframe(sms_flagged)
#' cpr_list <- read_cpr(c('path/to/cpr33.csv', 'path/to/cpr34.csv'))
#' vaisala_list <- read_vaisala('path/to/vaisala213.csv')
#'
#' ## Create the Megaframe!
#' mf <- compile_megaframe(sms, cpr_list, vaisala_list)
#' head(mf)
#' }
#' @export
compile_megaframe <- function(sms, cpr_list, vaisala_list) {
  cpr_df <- bind_rows(cpr_list)
  vaisala_df <- bind_rows(vaisala_list)

  megaframe <- bind_rows(sms, cpr_df, vaisala_df) %>%
    distinct # saves us from the chance there are duplicate models in a list.

  return(megaframe)
}
