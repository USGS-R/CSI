#' @title Calculate CSI values from salinity object
#'
#' @description Calculate monthly CSI values from salinity object for a set of sites over a range of scales.
#'
#' @param sal data.frame with Year and Month timestamp columns, with columns of site salinity values. Generally created by one of the CSIimport_ family functions.
#' @param scale integer representing the scale at which the CSI will be computed. For example, a value of six would imply that data from the current month and of the past five months will be used for computing the CSI value for a given month.
#' @param lmode logical. If true, L-moments are used in calculating alpha and beta parameters of gamma distribution as per SPEI package and Vicente-Serrano et al., 2010. If false (default), no L-moments are used, and gamma distribution parameters are caluclated directly from the observed data as per National Drought Mitigation Center (http://drought.unl.edu/MonitoringTools/DownloadableSPIProgram.aspx) and McKee et al., 1993.
#'
#' @return A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#'
#' @importFrom lmomco pwm.ub pwm2lmom are.lmom.valid pargam cdfgam
#' @importFrom MASS fitdistr
#' @importFrom stats cycle embed qnorm sd ts
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#'
CSIcalc <- function (sal, scale = 24, lmode = FALSE) {
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

  start_year <- sal$Year[1]
  start_month <- sal$Month[1]
  # Convert to time series
  csi <- array(NA, c(num_months, scale, num_sites), list(yearmos, 1:scale, names(sal)[3:(num_sites + 2)])) # initialize array for CSI values
  for (j in 3:dim(sal)[2]) { # loop for each site
    data <- ts(as.matrix(sal[, j]), start = c(start_year, start_month), frequency = 12)
    colnames(data) <- colnames(sal)[j]
    x <- matrix(NA, length(data), scale) # temp matrix to hold site CSIs
    for (i in 1:scale) { # loop for each scale
      a <- data
      if (i > 1) {
        # Average months over CSI interval
        a[i:length(a)] <- rowMeans(embed(a, i), na.rm = F)
        a[1:(i - 1)] <- NA
      }
      # Loop over months
      for (c in (1:12)) {
        f <- which(cycle(a) == c)
        f <- f[!is.na(a[f])]
        month <- sort(a[f])
        if (length(month) == 0 | is.na(sd(month, na.rm = T)) | sd(month, na.rm = T) == 0) {
          x[f, i] <- NA
          (next)()
        }
        pwm <- pwm.ub(month[month > 0])
        lmom <- pwm2lmom(pwm)
        if (!are.lmom.valid(lmom) | is.na(sum(lmom[[1]])) | is.nan(sum(lmom[[1]])))
          (next)()
        gampar <- pargam(lmom)
        if (!lmode) {
          tmp <- fitdistr(month, "gamma")
          gampar$para <- c(tmp$estimate[1], 1 / tmp$estimate[2]) # overwrite gamma parameters
        }
        x[f, i] <- qnorm(cdfgam(a[f], gampar))
      }
    }
    csi[, , j - 2] <- -x
  }
  attr(csi, "sal") <- sal
  attr(csi, "lmode") <- lmode

  return(csi)
}
