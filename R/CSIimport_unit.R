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
  Year <- Month <- Day <- 'dplyr'
  if (any(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP'))) {
    ts <- which(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP'))
    names(sal)[ts] <- 'Timestamp'
    sal$Date <- as.Date(sal$Timestamp)
    sal$Year <- format(sal$Date, format = "%Y")
    sal$Month <- as.numeric(format(sal$Date, format = "%m"))
    sal$Day <- as.numeric(format(sal$Date, format = "%d"))
    sal <- sal[, -which(names(sal) == 'Timestamp')]
    sal <- sal[, -which(names(sal) == 'Date')]
  }
  yr <- which(names(sal) %in% c('Year', 'year', 'YEAR'))
  names(sal)[yr] <- 'Year'
  mo <- which(names(sal) %in% c('Month', 'month', 'MONTH'))
  names(sal)[mo] <- 'Month'
  dy <- which(names(sal) %in% c('Day', 'day', 'DAY'))
  names(sal)[dy] <- 'Day'
  tm <- which(names(sal) %in% c('Time', 'time', 'TIME'))
  names(sal)[tm] <- 'Time'
  sal_daily <- group_by(sal, Year, Month, Day)
  if (any(names(sal_daily) == 'Time')) sal_daily <- sal_daily[, -which(names(sal_daily) == 'Time')]
  sal_daily <- summarize_all(sal_daily, mean, na.rm = T)
  sal_daily <- as.data.frame(sal_daily)
  sal_daily <- sal_daily[, c(which(names(sal_daily) %in% c("Year", "Month", "Day")), which(!names(sal_daily) %in% c("Year", "Month", "Day")))]
  # Find missing days and enter empty rows
  rng <- data.frame(Date = seq.Date(as.Date(paste(sal_daily$Year[1], sal_daily$Month[1], sal_daily$Day[1], sep = "-")), as.Date(paste(rev(sal_daily$Year)[1], rev(sal_daily$Month)[1], rev(sal_daily$Day)[1] , sep = "-")), by = "day"))
  rng$Year <- format(rng$Date, format = "%Y")
  rng$Month <- as.numeric(format(rng$Date, format = "%m"))
  rng$Day <- as.numeric(format(rng$Date, format = "%d"))
  rng <- rng[, -which(names(rng) == "Date")]
  sal_daily <- merge(rng, sal_daily, all.x = T)
  sal_daily <- sal_daily[order(sal_daily$Year, sal_daily$Month, sal_daily$Day), ]
  sal <- group_by(sal, Year, Month)
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
  sal <- sal[order(sal$Year, sal$Month), ]
  attr(sal, "daily_data") <- sal_daily

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
