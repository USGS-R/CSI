#' @title Import monthly-value files for CSI calculation
#'
#' @description Read in monthly salinity values file.
#'
#' @param file character The monthly-value file to import. Must have Year and Month columns, or single YearMo column = 'YYYY-MM', and columns of salinty values by site.
#'
#' @return A salinity object data.frame for calculating CSI values; has Year and Month timestamp columns, with (optionally multiple) individual columns of site salinity values.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#'
#' # Data file with single YearMo column
#' data_path <- system.file("extdata", "Monthly_ACE_Basin.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#'
CSIimport_monthly <- function (file) {
  if (!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal <- read.csv(file)
  if ((!any(names(sal) %in% c('Year', 'year', 'YEAR')) || !any(names(sal) %in% c('Month', 'month', 'MONTH'))) && !any(names(sal) %in% c('YearMo', 'yearmo', 'YEARMO')))
    stop("File must be a CSV with columns 'Year' and 'Month', or single column 'YearMo' = 'YYYY-MM', and columns for each site.")
  print("Importing data...")
  Year <- Month <- 'dplyr'
  if (any(names(sal) %in% c('YearMo', 'yearmo', 'YEARMO'))) {
    ym <- which(names(sal) %in% c('YearMo', 'yearmo', 'YEARMO'))
    names(sal)[ym] <- 'YearMo'
    sal$YearMo <- as.Date(paste0(sal$YearMo, "-01"))
    sal$Year <- format(sal$YearMo, format = "%Y")
    sal$Month <- as.numeric(format(sal$YearMo, format = "%m"))
    sal <- sal[, -which(names(sal) == 'YearMo')]
    sal <- sal[, c(which(names(sal) == 'Year'), which(names(sal) == 'Month'), 1:(dim(sal)[2] - 2))]
  }
  yr <- which(names(sal) %in% c('Year', 'year', 'YEAR'))
  names(sal)[yr] <- 'Year'
  mo <- which(names(sal) %in% c('Month', 'month', 'MONTH'))
  names(sal)[mo] <- 'Month'
  # Find missing months and enter empty rows
  rng <- data.frame(Date = seq.Date(as.Date(paste(sal$Year[1], sal$Month[1], "01", sep = "-")), as.Date(paste(rev(sal$Year)[1], rev(sal$Month)[1], "01" , sep = "-")), by = "month"))
  rng$Year <- format(rng$Date, format = "%Y")
  rng$Month <- as.numeric(format(rng$Date, format = "%m"))
  rng <- rng[, -which(names(rng) == "Date")]
  sal <- merge(rng, sal, all.x = T)
  sal <- sal[order(sal$Year, sal$Month), ]

  return(sal)
}
