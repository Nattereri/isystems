% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumpPower.R
\name{pumpPower}
\alias{pumpPower}
\title{Returns pump power (kW) for a head and discharge}
\usage{
pumpPower(Hm, Q, eff = 0.7)
}
\arguments{
\item{Hm}{pumping head (metres)}

\item{Q}{discharge volume (m3/s)}

\item{eff}{pump efficency (percentage)}
}
\value{
numeric (kW)
}
\description{
Returns pump power (kW) for a head and discharge
}
\examples{
pumpPower(1000, 0.0087, eff= 0.7)

}
