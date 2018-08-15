#' @title Import unit-value files for CSI calculation
#'
#' @description Read in unit value salinity file.
#'
#' @param file character The unit-value file to import. Must have Year, Month, Day, and Time columns, or single Timestamp = 'YYYY-MM-DD HH:mm:ss', and columns of salinty values by site.
#'
#' @return A salinity object data.frame for calculating CSI values; has Year and Month timestamp columns, with (optionally multiple) individual columns of site salinity values.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr group_by summarize_all
#'
#' @export
#'
#' @examples
#' # Data file with Year, Month, Day, and Time columns
#' data_path <- system.file("extdata", "Unitvalue_North_Inlet_ymdt.csv.zip", package="CSI")
#' unzip(data_path, exdir = getwd())
#' sal <- CSIimport_unit("Unitvalue_North_Inlet_ymdt.csv")
#'
#' # Data file with single Timestamp column
#' data_path <- system.file("extdata", "Unitvalue_North_Inlet.csv.zip", package="CSI")
#' unzip(data_path, exdir = getwd())
#' sal <- CSIimport_unit("Unitvalue_North_Inlet.csv")
#'
CSIimport_unit <- function (file) {
  if (!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal <- read.csv(file)
  if ((!any(names(sal) %in% c('Year', 'year', 'YEAR')) || !any(names(sal) %in% c('Month', 'month', 'MONTH')) || !any(names(sal) %in% c('Day', 'day', 'DAY'))  || !any(names(sal) %in% c('Time', 'time', 'TIME'))) && !any(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP')))
    stop("File must be a CSV with columns 'Year', 'Month', 'Day', and 'Time', or single column 'Timestamp' = 'YYYY-MM-DD HH:mm:ss', and salinity columns for each site")
  Year <- Month <- 'dplyr'
  if (any(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP'))) {
    ts <- which(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP'))
    names(sal)[ts] <- 'Timestamp'
    sal$Date <- as.Date(sal$Timestamp)
    sal$Month <- as.numeric(format(sal$Date, format = "%m"))
    sal$Year <- format(sal$Date, format = "%Y")
    sal <- sal[, -which(names(sal) == 'Timestamp')]
  }
  yr <- which(names(sal) %in% c('Year', 'year', 'YEAR'))
  names(sal)[yr] <- 'Year'
  mo <- which(names(sal) %in% c('Month', 'month', 'MONTH'))
  names(sal)[mo] <- 'Month'
  dy <- which(names(sal) %in% c('Day', 'day', 'DAY'))
  names(sal)[dy] <- 'Day'
  tm <- which(names(sal) %in% c('Time', 'time', 'TIME'))
  names(sal)[tm] <- 'Time'
  sal <- group_by(sal, Year, Month)
  if (any(names(sal) == 'Date')) sal <- sal[, -which(names(sal) == 'Date')]
  if (any(names(sal) == 'Day')) sal <- sal[, -which(names(sal) == 'Day')]
  if (any(names(sal) == 'Time')) sal <- sal[, -which(names(sal) == 'Time')]
  sal <- summarize_all(sal, mean, na.rm = T)
  sal <- as.data.frame(sal)
  # Find missing months and enter empty rows
  rng <- data.frame(Date = seq.Date(as.Date(paste(sal$Year[1], sal$Month[1], "01", sep = "-")), as.Date(paste(rev(sal$Year)[1], rev(sal$Month)[1], "01" , sep = "-")), by = "month"))
  rng$Year <- format(rng$Date, format = "%Y")
  rng$Month <- as.numeric(format(rng$Date, format = "%m"))
  rng <- rng[, -which(names(rng) == "Date")]
  sal <- merge(rng, sal, all.x = T)

  return(sal)
}

#' @title Import unit-value files for CSI calculation
#'
#' @description Read in unit value salinity file.
#'
#' @description Alias of CSIimport_unit.
#'
#' @param file character The unit-value file to import. Must have Year, Month, Day, and Time columns, or single Timestamp = 'YYYY-MM-DD HH:mm:ss', and columns of salinty values by site.
#'
#' @export
#'
CSIimport_interval <- CSIimport_unit
