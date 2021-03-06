% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms_functions.R
\name{tidy_groundwork}
\alias{tidy_groundwork}
\title{Tidy Groundwork Data Frame}
\usage{
tidy_groundwork(groundwork_df)
}
\arguments{
\item{groundwork_df}{The groundwork dataframe}
}
\value{
A dataframe that includes all of the raw groundwork data and flags that can be used for filtering
}
\description{
This function tidies imported Groundwork data of onsite measurements by selecting only the relevant variables and renaming them to a standard convention.
}
\details{
Variable naming convention: datetime, month, year, ghi1, ghi2, ghi3, ws, temp, ghi1_flag, ghi2_flag, ghi3_flag, ws_flag, temp_flag
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
