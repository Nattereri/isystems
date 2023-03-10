% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nameParamNS.R
\name{nameParamNS}
\alias{nameParamNS}
\title{Takes a North Site test name and makes a consistent parameter name}
\usage{
nameParamNS(df)
}
\arguments{
\item{df}{data frame with test column}
}
\value{
Parameter
}
\description{
Latis lab names only
}
\examples{
NS_df <- tibble(test = c("Appearance", "Total Molybdenum", "Electrical Conductivity", "Total Sulphate"))
nameParamNS(NS_df)

}
