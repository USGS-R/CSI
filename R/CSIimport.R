#' @title CSIimport
#'
#' @description Read in salinity values file.
#'
#' @param file character The file to import.
#' @param interval character The timestamp frequency -- '15 minute', '60 minute', or 'daily' (default).
#' @param scale numeric The range of months over which to compare. Default: 24.
#'
#' @return An array of CSI values
#'
#' @importFrom utils read.csv
#' @importFrom SPEI spi
#'
#' @export
#'
#' @examples
#' csi <- CSIimport("~/Desktop/R/CSI_source/Monthly_Waccamaw_LittleBlack_Rivers.csv")

CSIimport <- function(file, interval="daily", scale=24) {
  sal<-read.csv(file)
  num_months<-dim(sal)[1]  # number of months in data set
  num_sites<-dim(sal)[2]-2 # number of sites in data set
  csi<-array(NA,c(num_months,scale,num_sites)) # initialize array for CSI values
  for(j in 3:dim(sal)[2]) { # loop for each site
    x<-matrix(NA,length(which(!is.na(sal[,j]))),scale) # temp matrix to hold site CSIs
    for(i in 1:scale) # loop for each scale
      x[,i]<- -as.vector(spi(sal[which(!is.na(sal[,j])),j],i)$fitted) # calculate CSI (negative SPI) and extract values
    # Pad matrix for record length differences across sites; assumes missing beginning or end records only -- no internal NAs
    if(is.na(sal[1,j])) x<-rbind(matrix(NA,length(which(is.na(sal[,j]))),scale),x)
    if(is.na(sal[num_months,j])) x<-rbind(x,matrix(NA,length(which(is.na(sal[,j]))),scale))
    csi[,,j-2]<-x
  }
  return(csi)
}
