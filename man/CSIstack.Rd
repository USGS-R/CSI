% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSIstack.R
\name{CSIstack}
\alias{CSIstack}
\title{Create stacked CSI interval plots for sites}
\usage{
CSIstack(csi, dir = paste0(getwd(), "/csi_stacked"), thumbs = F,
  groupedx = T, groupedy = T, leg = "topleft")
}
\arguments{
\item{csi}{array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.}

\item{dir}{character Directory to write output files to. Default "csi_stacked" in working directory.}

\item{thumbs}{logical. If true, thumbnail plots will be generated in addition to full-sized plots. Default false.}

\item{groupedx}{logical. If true (default), x-asix will be the same across all output plots.}

\item{groupedy}{logical. If true (default), second y-asix (salinity) will be the same across all output plots.}

\item{leg}{character If "topleft" (default), legend will be displayed in upper left corner; if "bottom", legend will be placed horizontally along the figure bottom. Else no legend will be displayed.}
}
\description{
For each site (layer) in the csi object, plot the intervals (columns) on a stacked plot.
}
\examples{
# Data file with Year and Month columns
data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
sal <- CSIimport_monthly(data_path)
csi <- CSIcalc(sal)
CSIstack(csi)

}
