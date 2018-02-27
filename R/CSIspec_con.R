#' @title Convert specific conductance to salinity
#'
#' @description Convert specific conductance values to salinity values for a set of sites.
#'
#' @param sc data.frame with Year and Month timestamp columns (e.g., produced by CSIimport_ family functions), with columns of site specific conductance values.
#'
#' @return A data.frame with Year and Month timestamp columns, with columns of site salinity values.
#'
#' @export
#'
#' @examples
#' # Data file with single Date column, and specific conductance values
#' data_path <- system.file("extdata", "Daily_South_Carolina_spec_con.csv", package="CSI")
#' sc <- CSIimport_daily(data_path)
#' sal <- CSIspec_con(sc)
#'
CSIspec_con <- function (sc) {
  if (!(dim(sc)[1] >= 1) || !(dim(sc)[2] >= 3) || !is.data.frame(sc) || !any(names(sc) == 'Year') || !any(names(sc) == 'Month'))
    stop("sc must me a data.frame with Year and Month columns, and colums of site specific conductance values")

  sal <- sc[, 1:2]
  k1 <- 0.0120    # Wagner et al., 2006
  k2 <- -0.2174
  k3 <- 25.3283
  k4 <- 13.7714
  k5 <- -6.4788
  k6 <- 2.5842
  r <- sc[, 3:dim(sc)[2]] / 53087
  sal[, 3:dim(sc)[2]] <- k1 + k2 * r ^ 0.5 + k3 * r + k4 * r ^ 1.5 + k5 * r ^ 2 + k6 * r ^ 2.5
  names(sal) <- names(sc)

  return(sal)
}
