% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaDate.R
\name{metaDate}
\alias{metaDate}
\title{Add date information}
\usage{
metaDate(system_df)
}
\arguments{
\item{system_df}{bat data frame with Date column}
}
\value{
Month, MonthFull, Year
}
\description{
Creates useful date information (Month, MonthFull, Year)
from the Date column; as extra columns in the data frame
}
\examples{
sys_data_frame <- tibble(Date = Sys.Date())
metaDate(sys_data_frame)

}