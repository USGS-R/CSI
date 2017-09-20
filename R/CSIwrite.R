#' @title Write monthly CSI values from 3D csi object to files
#'
#' @description Write monthly CSI values from 3D csi object to text files, one file per site.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#'
#' @importFrom utils write.table
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#' CSIwrite(csi)
#'
CSIwrite <- function (csi, dir = paste0(getwd(), "/csi_values")) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  for (j in 1:dim(csi)[3]) # loop for each site
    write.table(csi[, , j], paste0(dir, "/", dimnames(csi)[[3]][j], ".csv"), sep=",", col.names = NA)
}
