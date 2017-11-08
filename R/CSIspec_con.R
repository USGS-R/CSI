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
  sal[, 3:dim(sc)[2]] <- 1.2817 * (0.36996 / (((sc[, 3:dim(sc)[2]] / 1000) ^ (-1.07)) - 0.0007464))
  names(sal) <- names(sc)

  return(sal)
}
