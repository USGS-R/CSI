% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSIinterp.R
\name{CSIinterp}
\alias{CSIinterp}
\title{Interpolate missing values for salinity object}
\usage{
CSIinterp(sal_na, limit = 6, method = "linear")
}
\arguments{
\item{sal_na}{data.frame with Year and Month timestamp columns, with columns of site salinity values. Generally created by one of the CSIimport_ family functions.}

\item{limit}{numeric. Limit length of months of gap to interpolate (default 6).}

\item{method}{character. Method used to interpolate data. Dfault is "linear"; optionally "spline" for cubic spline interplation.}
}
\value{
A salinity object data.frame for calculating CSI values; has Year and Month timestamp columns, with (optionally multiple) individual columns of site salinity values with interpolated internal NAs. A "filled_gaps" attribute will be added to the salinity object listing the data and duration of gaps filled, and an "interpolation_method" attribute containing the method.
}
\description{
Interpolate missing monthly salinity values from salinity object for a set of sites between the first and last measured values.
}
\examples{
# Data file with Year and Month columns
data_path <- system.file("extdata", "Daily_South_Carolina.csv", package = "CSI")
sal_na <- CSIimport_daily(data_path)
sal <- CSIinterp(sal_na)

}
