#' @title Import hourly-value files for CSI calculation
#'
#' @description Read in hourly salinity values file.
#'
#' @param file character The hourly-value file to import. Must have Year, Month, Day, and Hour columns, and columns of salinty values by site.
#'
#' @return A data.frame of year+monthly timestamps with columns of salinity values
#'
#' @importFrom utils read.csv
#' @importFrom dplyr group_by summarize_all
#'
#' @export
#'
#' @examples
#' #csi <- CSIimport_hourly("") #add file with Year,Month,Day,Hour columns
#'
CSIimport_hourly <- function(file) {
  if(!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal<-read.csv(file)
  if(!any(names(sal) %in% c('Year','year','YEAR')) || !any(names(sal) %in% c('Month','month','MONTH')) || !any(names(sal) %in% c('Day','day','DAY'))  || !any(names(sal) %in% c('Hour','hour','HOUR')) || !any(names(sal) %in% c('Time','time','TIME')))
    stop("file must be a CSV with columns 'Year', 'Month', 'Day', 'Hour', (or single column 'Time' = YYYY-MM-DD HH:mm:ss) and columns for each site")
  if(any(names(sal) %in% c('Time','time','TIME'))) {
    sal$Date<-as.Date(sal$Time)
    sal$Month<-format(sal$Date,format="%m")
    sal$Year<-format(sal$Date,format="%Y")
  }
  sal<-group_by(sal,Year,Month)
  sal<-summarize_all(sal,mean,na.rm=T)
  sal<-sal[,-which(names(sal)=='Date' || names(sal)=='Time')]
  sal<-as.data.frame(sal)

  return(sal)
}

#' @title Import hourly-value files for CSI calculation
#'
#' @description Read in hourly salinity values file.
#'
#' @description Alias of CSIimport_hourly.
#'
#' @export
#'
CSIimport_60minute <- CSIimport_hourly
