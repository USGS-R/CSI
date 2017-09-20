#' @title Calculate CSI values from salinity object
#'
#' @description Calculate monthly CSI values from salinity object for a set of sites over a range of scales.
#'
#' @param sal data.frame with Year and Month timestamp columns, with columns of site salinity values. Generally created by one of the CSIimport_ family functions.
#' @param scale integer representing the scale at which the CSI will be computed. For example, a value of six would imply that data from the current month and of the past five months will be used for computing the CSI value for a given month.
#'
#' @return A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#'
#' @importFrom SPEI spi
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#'
CSIcalc <- function (sal, scale = 24) {
  if (!(dim(sal)[1] >= 1) || !(dim(sal)[2] >= 3) || !is.data.frame(sal) || !any(names(sal) == 'Year') || !any(names(sal) == 'Month'))
    stop("sal must me a data.frame with Year and Month columns, and colums of site salinity values")
  yearmos <- paste(sal$Year, sal$Month, sep = "-")
  num_months <- dim(sal)[1]    # number of months in data set
  num_sites <- dim(sal)[2] - 2 # number of sites in data set, removing Year and Month columns

  r <- NULL
  for (j in 1:num_sites) { # check for internal NAs
    first_meas <- which(!is.na(sal[, j + 2]))[1]
    last_meas <- rev(which(!is.na(sal[, j + 2])))[1]
    if (any(which(is.na(sal[, j + 2])) > first_meas & which(is.na(sal[, j + 2])) < last_meas)) {
      warning(paste("Internal NAs detected. At least one measured value per month must be present between first and last measured values. Site", names(sal)[j + 2], "removed from analysis. Consider CSIinterp to fill gaps."), immediate. = T)
      r <- c(r, j + 2)
      num_sites <- num_sites - 1
    }
  }
  if (!is.null(r)) sal <- sal[, -r]

  csi <- array(NA, c(num_months, scale, num_sites), list(yearmos, 1:scale, names(sal)[3:(num_sites + 2)])) # initialize array for CSI values
  for (j in 3:dim(sal)[2]) { # loop for each site
    x <- matrix(NA, length(which(!is.na(sal[, j]))), scale) # temp matrix to hold site CSIs
    for (i in 1:scale) # loop for each scale
      x[,i] <- -as.vector(spi(sal[which(!is.na(sal[, j])), j], i)$fitted) # calculate CSI (negative SPI) and extract values
    # Pad matrix for record length differences across sites; assumes missing beginning or end records only -- no internal NAs
    if (is.na(sal[1, j])) x <- rbind(matrix(NA, length(which(is.na(sal[, j]))), scale), x)
    if (is.na(sal[num_months, j])) x <- rbind(x, matrix(NA, length(which(is.na(sal[, j]))), scale))
    csi[, , j - 2] <- x
  }
  attr(csi, "sal") <- sal

  return(csi)
}
