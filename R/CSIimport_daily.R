#' @title Import daily-value files for CSI calculation
#'
#' @description Read in daily salinity values file.
#'
#' @param file character The daily-value file to import. Must have Year, Month, and Day columns (or single column 'Date' = YYYY-MM-DD), and columns of salinty values by site.
#'
#' @return A data.frame of year+monthly timestamps with columns of salinity values
#'
#' @importFrom utils read.csv
#' @importFrom dplyr group_by summarize_all
#'
#' @export
#'
#' @examples
#' #add file with Year,Month,Day columns
#' csi <- CSIimport_daily(system.file("extdata","Daily_Waccamaw_LittleBlack_Rivers.csv",package="CSI"))
#'
CSIimport_daily <- function(file) {
  if(!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal<-read.csv(file)
  if((!any(names(sal) %in% c('Year','year','YEAR')) || !any(names(sal) %in% c('Month','month','MONTH')) || !any(names(sal) %in% c('Day','day','DAY'))) && !any(names(sal) %in% c('Date','date','DATE')))
    stop("file must be a CSV with columns 'Year', 'Month', 'Day', (or single column 'Date' = YYYY-MM-DD) and columns for each site")
  Year<-Month<-'dplyr'
  if(any(names(sal) %in% c('Date','date','DATE'))) {
    sal$Date<-as.Date(sal$Date)
    sal$Month<-format(sal$Date,format="%m")
    sal$Year<-format(sal$Date,format="%Y")
  }
  sal<-group_by(sal,Year,Month)
  sal<-summarize_all(sal,mean,na.rm=T)
  sal<-sal[,-which(names(sal)=='Date')]
  sal<-as.data.frame(sal)

  return(sal)
}

#' @title Import daily-value files for CSI calculation
#'
#' @description Read in daily salinity values file.
#'
#' @description Alias of CSIimport_daily.
#'
#' @export
#'
CSIimport_24hour <- CSIimport_daily
