#' @title Interpolate missing values for salinity object
#'
#' @description Interpolate missing monthly salinity values from salinity object for a set of sites between the first and last measured values.
#'
#' @param sal_na data.frame with Year and Month timestamp columns, with columns of site salinity values. Generally created by one of the CSIimport_ family functions.
#' @param limit numeric. Limit length of months of gap to interpolate (default 6).
#' @param method character. Method used to interpolate data. Dfault is "linear"; optionally "spline" for cubic spline interplation.
#'
#' @return A data.frame with Year and Month timestamp columns, with columns of site salinity values, with interpolated internal NAs. A "filled_gaps" attribute will be added to the salinity object listing the data and duration of gaps filled, and an "interpolation_method" attribute containing the method.
#'
#' @importFrom zoo na.approx na.spline
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Daily_South_Carolina.csv", package = "CSI")
#' sal_na <- CSIimport_daily(data_path)
#' sal <- CSIinterp(sal_na)
#'
CSIinterp <- function (sal_na, limit = 6, method = "linear") {
  if (!(dim(sal_na)[1] >= 1) || !(dim(sal_na)[2] >= 3) || !is.data.frame(sal_na) || !any(names(sal_na) == 'Year') || !any(names(sal_na) == 'Month'))
    stop("sal_na must me a data.frame with Year and Month columns, and colums of site salinity values")
  if (!is.numeric(limit) | is.na(limit)) limit <- Inf
  num_sites <- dim(sal_na)[2] - 2 # number of sites in data set, removing Year and Month columns

  sal <- sal_na[, 1:2]
  filled_gaps_attr <- NULL
  for (j in 1:num_sites) { # check for internal NAs
    first_meas <- which(!is.na(sal_na[, j + 2]))[1]
    last_meas <- rev(which(!is.na(sal_na[, j + 2])))[1]
    filled_num <- unfilled_num <- 0
    if (any(which(is.na(sal_na[, j + 2])) > first_meas & which(is.na(sal_na[, j + 2])) < last_meas)) {
      sal[first_meas:last_meas, j + 2] <- if (method == "spline") { na.spline(sal_na[first_meas:last_meas, j + 2], maxgap = limit) 
        } else na.approx(sal_na[first_meas:last_meas, j + 2], maxgap = limit)
      sal[which(sal[, j + 2] <= 0), j + 2] <- 0.01 # Prevent negative values
      runs <- rle(is.na(sal_na[first_meas:last_meas, j + 2]))
      filled_num <- sum(runs$lengths <= limit & runs$values == T)
      unfilled_num <- sum(runs$lengths > limit & runs$values == T)
      filled_gaps <- which(runs$lengths <= limit & runs$values == T)
      s <- t <- NULL
      for (i in 1:length(filled_gaps)) {
        t[i] <- paste(sal_na[first_meas + sum(runs$lengths[1:(filled_gaps[i] - 1)]), 1:2], collapse = "-")
        s[i] <- runs$lengths[filled_gaps[i]]
      }
      filled_gaps_attr[[paste0(names(sal_na)[j + 2], "_filled_gaps")]] <- t
      filled_gaps_attr[[paste0(names(sal_na)[j + 2], "_filled_gaps_len")]] <- s
      message(paste0(names(sal_na)[j + 2], ": ", filled_num, " gaps <= ", limit, " months filled; ", unfilled_num, " gaps > ", limit, " months unfilled"))
    }
    else {
      sal[, j + 2] <-sal_na[, j + 2]
      message(paste0(names(sal_na)[j + 2], ": no gaps detected"))
    }
  }
  names(sal) <- names(sal_na)
  attr(sal, "filled_gaps") <- filled_gaps_attr
  attr(sal, "interpolation_method") <- method

  return(sal)
}
