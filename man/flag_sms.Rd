% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms_functions.R
\name{flag_sms}
\alias{flag_sms}
\title{Flag SMS Data Frame}
\usage{
flag_sms(tidy_sms)
}
\arguments{
\item{tidy_sms}{A dataframe of the onsite data, in the tidy variable format}
}
\value{
A dataframe that has the missing datetimes filled in with NAs at the measured values and flagged 1000
}
\description{
Flag any missing datetimes within the onsite data Period of Record
}
\examples{
\dontrun{
## Get the files to import
sms_files <- paste0('data/Groundwork/', dir(path = "data/Groundwork/", pattern = "*.csv"))

## Import the files
groundwork_df <- import_groundwork_files(sms_files)

## Tidy the Dataframe
tidy_sms <- tidy_groundwork(groundwork_df)

## Flag the Dataframe
sms_flagged <- flag_sms(tidy_sms)

}
}
