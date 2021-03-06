% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms_functions.R
\name{average_data_to_hourly}
\alias{average_data_to_hourly}
\title{Average Time Series Data To Hourly Intervals}
\usage{
average_data_to_hourly(sms_flagged)
}
\arguments{
\item{sms_flagged}{Dataframe of sms data at minute intervals with "tidy" variables}
}
\value{
sms_flagged with hourly intervals
}
\description{
Convert minute interval SMS data to hourly by taking the average over the hour
}
\examples{
\dontrun{
## To Prepare sms_flagged dataframe for megaframe compliation:
sms_flagged \%>\%
  filter_sms() \%>\%
  average_ghi_sensors() \%>\%
  average_data_to_hourly() \%>\%
  format_sms_vars_for_megaframe()
}
}
