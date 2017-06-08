#' @title Import monthly-value files for CSI calculation
#'
#' @description Read in monthly salinity values file.
#'
#' @param file character The monthly-value file to import. Must have Year and Month columns, and columns of salinty values by site.
#'
#' @return A data.frame of year+monthly timestamps with columns of salinity values
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' csi <- CSIimport_monthly("~/Desktop/R/CSI_source/Monthly_Waccamaw_LittleBlack_Rivers.csv")
#'
CSIimport_monthly <- function(file) {
  if(!(length(file) == 1) || !is.character(file))
    stop("file must be a single character string")
  sal<-read.csv(file)
  if(!any(names(sal) %in% c('Year','year','YEAR')) || !any(names(sal) %in% c('Month','month','MONTH')))
    stop("file must be a CSV with columns 'Year', 'Month', and columns for each site")

  return(sal)
}
