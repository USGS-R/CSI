#' @title Import salinity value files.
#'
#' @description Read in salinity values files of various intervals.
#'
#' @param file character The file to import.
#' @param interval character The timestamp frequency -- '15 minute', '60 minute', 'daily', 'interval', or 'monthly' (default).
#'
#' @return A data.frame of year+monthly timestamps with columns of salinity values
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' csi <- CSIimport("~/Desktop/R/CSI_source/Monthly_Waccamaw_LittleBlack_Rivers.csv")
#'
CSIimport <- function(file, interval=c("monthly","daily","hourly","15-minute","60-minute","irregular","interval"), scale=24) {
  if(!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  interval<-match.arg(interval)
  if(!(length(scale) == 1) || is.na(as.integer(scale)) || as.integer(scale) <= 0)
    stop("scale must be a single positive integer")
  scale<-as.integer(scale)
  if(interval=="monthly")
    sal<-CSIimport_monthly(file)
  else if(interval=="daily")
    sal<-CSIimport_daily(file)
  else if(interval=="hourly" || interval=="60-minute")
    sal<-CSIimport_hourly(file)
  else if(interval=="interval" || interval=="irregular")
    sal<-CSIimport_interval(file)

  return(sal)
}
