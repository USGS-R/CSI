#' @title Import salinity value files.
#'
#' @description Read in salinity values files of various intervals.
#'
#' @param file character The file to import.
#' @param interval character The timestamp frequency -- 'daily', 'interval'/'unit', or 'monthly' (default). Alternatively, 'NERRS' can be used to import NERRS-format data.
#'
#' @return A salinity object data.frame for calculating CSI values; has Year and Month timestamp columns, with (optionally multiple) individual columns of site salinity values.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport(data_path, interval = "monthly")
#'
CSIimport <- function (file, interval = c("monthly", "daily", "24hour", "interval", "unit", "NERRS")) {
  if (!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  interval <- match.arg(interval)
  sal <- do.call(paste0("CSIimport_", interval), list(file))

  return(sal)
}
