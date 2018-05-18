#' @title Create cumulative percentage CSI plots for sites
#'
#' @description For each site (layer) in the csi object, plot the intervals (columns) on a cumulative percentage plot.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics par plot rect hist
#' @importFrom stats dnorm
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
CSIdist <- function (csi, dir = paste0(getwd(), "/csi_dist")) {
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
      br <- NULL
      for (k in 2:11) br[k - 1] <- which.min(abs(t - csi.breaks[k]))
      mn <- paste0(dimnames(csi)[[3]][i], " ", j, "-Month Coastal Salinity Index Distribution")
      if (attr(csi, "lmode")) mn <- paste(mn, "(using L moments)")
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][i], "_dist_", j, ".png"), width = 1150, height = 614, units = "px", pointsize = 12)
      par(mar = c(5.1, 4.1, 4.1, 4.1))
      h <- hist(t, breaks = 40, density = 30, col = "gray20", xlim = c(-3, 3), xlab = "Coastal salinity index", ylab = "Frequency", main = mn, cex.lab = 1.25, cex.axis = 1.25, tck = 0.02)
      xfit <- seq(min(t), max(t), length = 40)
      yfit <- dnorm(xfit, mean = mean(t), sd = sd(t))
      yfit <- yfit * diff(h$mids[1:2]) * length(t)
      rect(-1000, 0, -2, 1000, col = csi.cols[1])
      rect(-2, 0, -1.6, 1000, col = csi.cols[2])
      rect(-1.6, 0, -1.3, 1000, col = csi.cols[3])
      rect(-1.3, 0, -0.8, 1000, col = csi.cols[4])
      rect(-0.8, 0, -0.5, 1000, col = csi.cols[5])
      rect(-0.5, 0, 0.5, 1000, col = csi.cols[6])
      rect(0.5, 0, 0.8, 1000, col = csi.cols[7])
      rect(0.8, 0, 1.3, 1000, col = csi.cols[8])
      rect(1.3, 0, 1.6, 1000, col = csi.cols[9])
      rect(1.6, 0, 2, 1000, col = csi.cols[10])
      rect(2, 0, 1000, 1000, col = csi.cols[11])
      plot(h, add = T)
      lines(xfit, yfit, lwd = 3)
      dev.off()
    }
  }
}
