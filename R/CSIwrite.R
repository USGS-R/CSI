#' @title Write monthly CSI values from 3D csi object to files
#'
#' @description Write monthly CSI values from 3D csi object to text files, one file per site.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#'
#' @importFrom utils write.csv write.table capture.output
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

  sal <- attr(csi, "sal")
  options(warn = -1)
  txt <- "Final data used for CSI calculation"
  txt <- if (attr(csi, "lmode")) paste0(txt, " (L-moments used):") else paste0(txt, ":")
  write(txt, paste0(dir, "/CSI_calculation_data.csv"))
  write.csv(sal, paste0(dir, "/CSI_calculation_data.csv"), row.names = F, quote = F, append = T)
  write("Statistics for CSI data calculation:\n", paste0(dir, "/gage_stats.txt"))
  write(paste0("Data range: ", sal$Year[1], "-", sal$Month[1], " to ", rev(sal$Year)[1], "-", rev(sal$Month)[1], " (", length(sal$Year), " months)"), paste0(dir, "/gage_stats.txt"), append = T)
  write("\nGage statistics: ", paste0(dir, "/gage_stats.txt"), append = T)
  sum <- summary(sal)[, 3:dim(sal)[2]]
  sum <- gsub("[[:space:]]*:", ": ", gsub("Min.", "Minimum", gsub("Max.", "Maximum", gsub("Qu.", "Quartile", sum))))
  capture.output(paste(sum, collapse = "\n"), file = paste0(dir, "/gage_stats.txt"), append = T)
  options(warn = 0)
  for (j in 1:dim(csi)[3]) # loop for each site
    write.table(csi[, , j], paste0(dir, "/", dimnames(csi)[[3]][j], ".csv"), sep=",", col.names = NA)
  if (length(attr(csi, "filled_gaps"))) {
    filled_gaps <- attr(csi, "filled_gaps")
    txt2 <- if (attr(csi, "interpolation_method") == "linear") "Linearly" else "Cubic spline"
    write(paste(txt2, "interpolated gaps filled for CSI calculation:"), paste0(dir, "/filled_gaps.txt"))
    for (j in seq(1, length(filled_gaps), by = 2)) {
      write(paste0("\nGaps filled for ", substr(names(filled_gaps)[j], 1, nchar((names(filled_gaps)[j])) - 12), ":"), paste0(dir, "/filled_gaps.txt"), append = T)
      write(filled_gaps[[j]], paste0(dir, "/filled_gaps.txt"), append = T)
      write("Gap lengths (months):", paste0(dir, "/filled_gaps.txt"), append = T)
      write(filled_gaps[[j + 1]], paste0(dir, "/filled_gaps.txt"), append = T)
    }
  }
}
