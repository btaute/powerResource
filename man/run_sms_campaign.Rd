% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/campaign_functions.R
\name{run_sms_campaign}
\alias{run_sms_campaign}
\title{Run SMS Campaign}
\usage{
run_sms_campaign(megaframe)
}
\arguments{
\item{megaframe}{A megaframe with at least one satellite model and the sms model}
}
\value{
Dataframe that contains all of the results with a variable to identify the year and month of the last datapoint for the MCP
}
\description{
Run an MCP at monthly intervals throughout SMS Campaign.  With each additional month of data, you can see how the results of an MCP change.
}
\examples{
\dontrun{
run_sms_campaign(megaframe)
}
}
