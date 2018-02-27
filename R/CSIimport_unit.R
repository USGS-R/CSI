#' @title Import unit-value files for CSI calculation
#'
#' @description Read in unit value salinity file.
#'
#' @param file character The unit-value file to import. Must have Year, Month, Day, and Time columns, or single Timestamp = 'YYYY-MM-DD HH:mm:ss', and columns of salinty values by site.
#'
#' @return A data.frame with Year and Month timestamp columns, with columns of site salinity values.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr group_by summarize_all
#'
#' @export
#'
#' @examples
#' #ADD
#' # Data file with Year, Month, Day, and Hour columns
#' # data_path <- system.file("extdata", "", package="CSI")
#' # sal <- CSIimport_unit(data_path)
#'
#' # Data file with single Timestamp column
#' data_path <- system.file("extdata", "Unitvalue_North_Inlet.csv", package="CSI")
#' sal <- CSIimport_unit(data_path)
#'
CSIimport_unit <- function (file) {
  if (!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal <- read.csv(file)
  if ((!any(names(sal) %in% c('Year', 'year', 'YEAR')) || !any(names(sal) %in% c('Month', 'month', 'MONTH')) || !any(names(sal) %in% c('Day', 'day', 'DAY'))  || !any(names(sal) %in% c('Time', 'time', 'TIME'))) && !any(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP')))
    stop("File must be a CSV with columns 'Year', 'Month', 'Day', and 'Time', or single column 'Timestamp' = 'YYYY-MM-DD HH:mm:ss', and salinity columns for each site")
  Year <- Month <- 'dplyr'
  if (any(names(sal) %in% c('Timestamp', 'timestamp', 'TIMESTAMP'))) {
    sal$Date <- as.Date(sal$Timestamp)
    sal$Month <- format(sal$Date, format = "%m")
    sal$Year <- format(sal$Date, format = "%Y")
    sal <- sal[, -which(names(sal) == 'Timestamp')]
  }
  sal <- group_by(sal, Year, Month)
  sal <- summarize_all(sal, mean, na.rm = T)
  if (any(names(sal) == 'Date')) sal <- sal[, -which(names(sal) == 'Date')]
  if (any(names(sal) == 'Day')) sal <- sal[, -which(names(sal) == 'Day')]
  if (any(names(sal) == 'Time')) sal <- sal[, -which(names(sal) == 'Time')]
  sal <- as.data.frame(sal)

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