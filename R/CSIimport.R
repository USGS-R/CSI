#' @title CSIimport
#'
#' @description Read in salinity values file.
#'
#' @param file character The file to import.
#' @param last character The last name.
#'
#' @export
#'
#' @examples
#' CSIimport("~/Desktop/R/CSI_source/Monthly_Waccamaw_LittleBlack_Rivers.csv")

CSIimport <- function(file) {
  site_group<-"Waccamaw_LittleBlack_Rivers" # Label for analysis

  # Bin ranges and colors for CSI categories
  csi.breaks<-c(-1000,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,1000)
  csi.cols<-c("#740200CC","#E80600CC","#FFAC00CC","#FCD57BCC","#FEFF00CC","#FDFFE4CC","#E4E1EACC","#B6D0E5CC","#91ABD0CC","#33648DCC","#23425FCC")

  sal<-read.csv(file)
  num_months<-dim(sal)[1]  # number of months in data set
  num_sites<-dim(sal)[2]-2 # number of sites in data set
  scale<-24                # range of months over which to compare: e.g., 1 to 24 months
}
