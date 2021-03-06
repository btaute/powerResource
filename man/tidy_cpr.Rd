% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/satellite_functions.R
\name{tidy_cpr}
\alias{tidy_cpr}
\title{Tidy CPR Data Frame}
\usage{
tidy_cpr(cpr_df)
}
\arguments{
\item{cpr_df}{A dataframe from importing a CPR timeseries}
}
\value{
A dataframe in the Megaframe format
}
\description{
Converts an imported CPR data frame to one that is in the Megaframe format
}
\details{
Megaframe format variables: datetime, year, month, ghi, dni, dhi, temp, ws, model
}
\examples{
\dontrun{
cpr_df <- read_csv('cpr_file.csv', skip = 1)
cpr <- tidy_cpr(cpr_df)
summary(cpr)
}
}
