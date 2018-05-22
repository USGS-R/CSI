#' @title Create cumulative percentage CSI plots for sites
#'
#' @description For each site (layer) in the csi object, plot the intervals (columns) on a cumulative percentage plot.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot rect text points
#' @importFrom stats ecdf
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#' CSIcumper(csi)
#'
CSIcumper <- function (csi, dir = paste0(getwd(), "/csi_cumper")) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#730000CC", "#E60000CC", "#FFAA00CC", "#FCD37FCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]
  for (i in 1:num_sites) {
    for (j in 1:scale) {
      t <- csi[, j, i]
      t <- t[!is.na(t)]
      s <- ecdf(t)
      mn <- paste0(dimnames(csi)[[3]][i], " ", j, "-Month Coastal Salinity Index Cumulative Percentage")
      if (attr(csi, "lmode")) mn <- paste(mn, "(using L moments)")
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][i], "_cumper_", j, ".png"), width = 1150, height = 614, units = "px", pointsize = 12)
      par(mar = c(5.1, 4.1, 4.1, 4.1))
      plot(s, pch = NA, xlim = c(-3, 3), ylab = "Cumulative percentage", yaxt = "n", xlab = "Coastal salinity index", main = mn, cex.lab = 1.25, cex.axis = 1.25, tck = 0.02)
      axis(2, seq(0, 1, 0.2), c("0%", "20%", "40%", "60%", "80%", "100%"), tck = 0.02, las = 1)
      rect(-1000, 0, -2, 1, col = csi.cols[1])
      rect(-2, 0, -1.6, 1, col = csi.cols[2])
      rect(-1.6, 0, -1.3, 1, col = csi.cols[3])
      rect(-1.3, 0, -0.8, 1, col = csi.cols[4])
      rect(-0.8, 0, -0.5, 1, col = csi.cols[5])
      rect(-0.5, 0, 0.5, 1, col = csi.cols[6])
      rect(0.5, 0, 0.8, 1, col = csi.cols[7])
      rect(0.8, 0, 1.3, 1, col = csi.cols[8])
      rect(1.3, 0, 1.6, 1, col = csi.cols[9])
      rect(1.6, 0, 2, 1, col = csi.cols[10])
      rect(2, 0, 1000, 1, col = csi.cols[11])
      lines(s, lwd = 3, pch = NA)
      abline(h = seq(0, 1, 0.1))
      points(c(-2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2), s(c(-2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2)), pch = 16, cex = 1.5)
      legend("topleft", c(paste0(j, "-month CSI"), "Threshold points"), lty = c(1, NA), lwd = c(3, NA), pch = c(NA, 16), cex = 1.25, inset = c(0.01, 0.05))
      text(c(-2.6, -1.8, -1.45, -1.05, -0.675, 0, 0.675, 1.05, 1.45, 1.8, 2.6), 1.02, c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "CW0", "CW1", "CW2", "CW3", "CW4"), font = 2, cex = 1.25)
      legend("bottomright", paste("Percentage < 0: ", round(s(0), 2)), cex = 1.25, inset = c(0.01, 0.05))
      dev.off()
    }
  }
}
