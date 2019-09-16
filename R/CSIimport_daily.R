#' @title Import daily-value files for CSI calculation
#'
#' @description Read in daily salinity values file.
#'
#' @param file character The daily-value file to import. Must have Year, Month, and Day columns, or single column Date = 'YYYY-MM-DD', and columns of salinty values by site.
#' @param mindays integer the minimum number of daily values needed to produce a mean value for a given month (default 15). Months with fewer data values than this will be asigned mean NA.
#'
#' @return A salinity object data.frame for calculating CSI values; has Year and Month timestamp columns, with (optionally multiple) individual columns of site salinity values.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr group_by summarize_all
#'
#' @export
#'
#' @examples
#' # Data file with Year, Month, and Day columns
#' data_path <- system.file("extdata", "Daily_Waccamaw_LittleBlack_Rivers.csv", package="CSI")
#' sal <- CSIimport_daily(data_path)
#'
#' # Data file with single Date column
#' data_path <- system.file("extdata", "Daily_South_Carolina.csv", package="CSI")
#' sal <- CSIimport_daily(data_path)
#'
CSIimport_daily <- function (file, mindays = 15) {
  if (!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  if (!(length(mindays) == 1) || !is.numeric(mindays))
    stop("mindays must be a single numeric value")
  sal <- read.csv(file)
  if ((!any(names(sal) %in% c('Year', 'year', 'YEAR')) || !any(names(sal) %in% c('Month', 'month', 'MONTH')) || !any(names(sal) %in% c('Day', 'day', 'DAY'))) && !any(names(sal) %in% c('Date', 'date', 'DATE')))
    stop("File must be a CSV with columns 'Year', 'Month', and 'Day', or single column 'Date' = 'YYYY-MM-DD', and columns of salinity values for each site")
  print("Importing data...")
  Year <- Month <- 'dplyr'
  if (any(names(sal) %in% c('Date', 'date', 'DATE'))) {
    dt <- which(names(sal) %in% c('Date', 'date', 'DATE'))
    names(sal)[dt] <- 'Date'
    sal$Date <- as.Date(sal$Date)
    sal$Year <- format(sal$Date, format = "%Y")
    sal$Month <- as.numeric(format(sal$Date, format = "%m"))
    sal$Day <- as.numeric(format(sal$Date, format = "%d"))
    sal <- sal[, -which(names(sal) == 'Date')]
  }
  yr <- which(names(sal) %in% c('Year', 'year', 'YEAR'))
  names(sal)[yr] <- 'Year'
  mo <- which(names(sal) %in% c('Month', 'month', 'MONTH'))
  names(sal)[mo] <- 'Month'
  dy <- which(names(sal) %in% c('Day', 'day', 'DAY'))
  names(sal)[dy] <- 'Day'
  sal_daily <- sal[, c(which(names(sal) %in% c("Year", "Month", "Day")), which(!names(sal) %in% c("Year", "Month", "Day")))]
  mo <- seq.Date(as.Date(paste(sal_daily$Year[1], sal_daily$Month[1], "01", sep = "-")), as.Date(paste(rev(sal_daily$Year)[1], rev(sal_daily$Month)[1], "01", sep = "-")), "month")
  for (i in 4:dim(sal_daily)[2])
    for (j in 1:length(mo))
      if (length(which(as.Date(paste(sal_daily$Year, sal_daily$Month, "01", sep = "-")) == mo[j] & !is.na(sal_daily[, i]))) < mindays) {
        print(paste(names(sal_daily)[i], substr(mo[j], 1, 7), "less than minimum", mindays, "days - removed"))
        sal_daily[which(as.Date(paste(sal_daily$Year, sal_daily$Month, "01", sep = "-")) == mo[j]), i] <- NA
      }
  sal <- group_by(sal_daily, Year, Month)
  sal <- sal[, -which(names(sal) == 'Day')]
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

#' @title Import daily-value files for CSI calculation
#'
#' @description Read in daily salinity values file.
#'
#' @description Alias of CSIimport_daily.
#'
#' @param file character The daily-value file to import. Must have Year, Month, and Day columns, or single column Date = 'YYYY-MM-DD', and columns of salinty values by site.
#' @param mindays integer the minimum number of daily values needed to produce a mean value for a given month (default 15). Months with fewer data values than this will be asigned mean NA.
#'
#' @export
#'
CSIimport_24hour <- CSIimport_daily
