#' @title Interpolate missing values for salinity object
#'
#' @description Interpolate missing monthly salinity values from salinity object for a set of sites between the first and last measured values.
#'
#' @param sal_na data.frame with Year and Month timestamp columns, with columns of site salinity values. Generally created by one of the CSIimport_ family functions.
#' @param limit numeric. Limit length of months of gap to interpolate (default 6).
#'
#' @return A data.frame with Year and Month timestamp columns, with columns of site salinity values, with interpolated internal NAs.
#'
#' @importFrom zoo na.approx
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Daily_South_Carolina.csv", package = "CSI")
#' sal_na <- CSIimport_daily(data_path)
#' sal <- CSIinterp(sal_na)
#'
CSIinterp <- function (sal_na, limit = 6) {
  if (!(dim(sal_na)[1] >= 1) || !(dim(sal_na)[2] >= 3) || !is.data.frame(sal_na) || !any(names(sal_na) == 'Year') || !any(names(sal_na) == 'Month'))
    stop("sal_na must me a data.frame with Year and Month columns, and colums of site salinity values")
  if (!is.numeric(limit) | is.na(limit)) limit <- Inf
  num_sites <- dim(sal_na)[2] - 2 # number of sites in data set, removing Year and Month columns

  sal <- sal_na[, 1:2]
  for (j in 1:num_sites) { # check for internal NAs
    first_meas <- which(!is.na(sal_na[, j + 2]))[1]
    last_meas <- rev(which(!is.na(sal_na[, j + 2])))[1]
    filled <- unfilled <- 0
    if (any(which(is.na(sal_na[, j + 2])) > first_meas & which(is.na(sal_na[, j + 2])) < last_meas)) {
      runs <- rle(is.na(sal_na[first_meas:last_meas, j + 2]))
      filled <- sum(runs$lengths <= limit & runs$values == T)
      unfilled <- sum(runs$lengths > limit & runs$values == T)
      sal[first_meas:last_meas, j + 2] <- na.approx(sal_na[first_meas:last_meas, j + 2], maxgap = limit)
      message(paste0(names(sal_na)[j + 2], ": ", filled, " gaps <= ", limit, " months filled; ", unfilled, " gaps > ", limit, " months unfilled"))
    }
    else {
      sal[, j + 2] <-sal_na[, j + 2]
      message(paste0(names(sal_na)[j + 2], ": no gpas detected"))
    }
  }
  names(sal) <- names(sal_na)

  return(sal)
}
